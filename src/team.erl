-module(team).

-behaviour(gen_server).

%% External exports
-export([start_link/1,
		 pitstop_operations/5,
		 update/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-define(TEAM_NAME(Id), {global, utils:build_id_atom("team_", Id)}).

%% last_ls: list of chrono_notif containing at most
%%			one record for each intermediate
%% avg_consumption: {TyresC, FuelC}
-record(car_stats, {car_id,
					pitstop_count,
					avg_consumption = {undef, undef},
					last_ls = []}).

%% fuel_limit: if a car's fuel is lesser than the limit
%%			   then this car should stop at the pits
%% tyres_limit: if a car's tyres_consumption is greater than
%%				the limit then this car should stop at the pits
%% rain_sum: sum of rain field of all the segments
%% cars_stats: list of car_stats records
-record(state, {fuel_limit,
				tyres_limit,
				rain_sum,
				cars_stats = []}).

%% type: slick | intermediate | wet
%% min: minimum value of rain index
%% max: maximum value of rain index
-record(tyres_interval, {type, min, max}).

%% List of tyres_interval records, it represents the intervals
%% within which each different type of tyres performs better.
-define(TYRES_SPECS, [#tyres_interval{type = slick, min = 0.0, max = 3.0},
					  #tyres_interval{type = intermediate, min = 3.0, max = 6.5},
					  #tyres_interval{type = wet, min = 6.5, max = 10.0}]).


%% ====================================================================
%% External functions
%% ====================================================================

start_link(Config) when is_list(Config) ->
	{id, TeamId} = lists:keyfind(id, 1, Config),
	gen_server:start_link(?TEAM_NAME(TeamId), ?MODULE, Config, []).

pitstop_operations(TeamId, CarId, CarStatus, Lap, PSCount) when is_record(CarStatus, car_status) ->
	gen_server:call(?TEAM_NAME(TeamId), {pitstop, CarId, CarStatus, Lap, PSCount}, infinity).

update(TeamId, {weather, Delta}) when is_integer(Delta) ->
	gen_server:call(?TEAM_NAME(TeamId), {weather_update, Delta}, infinity);

update(TeamId, {chrono, Notif}) when is_record(Notif, chrono_notif) ->
	gen_server:call(?TEAM_NAME(TeamId), {chrono_update, Notif}, infinity).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Config) ->
	CarType = lists:foldl(
				fun({id, Id}, CT) ->
						CB = #callback{mod = ?MODULE, func = update, args = [Id]},
						event_dispatcher:subscribe(team, CB),
						CT#car_type{id = Id};
				   ({team_name, Name}, CT) ->
						CT#car_type{team_name = Name};
				   ({brake, Brake}, CT) ->
						CT#car_type{brake = Brake};
				   ({power, Power}, CT) ->
						CT#car_type{power = Power};
				   ({weight, Weight}, CT) ->
						CT#car_type{weight = Weight};
				   (_, CT) -> CT
				end, #car_type{}, Config),
	T = fun() ->
				mnesia:write(CarType)
		end,
	{atomic, ok} = mnesia:sync_transaction(T),
	{ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({weather_update, Delta}, _From, State) ->
	RainSum = State#state.rain_sum + Delta,
	{reply, ok, State#state{rain_sum = RainSum}};

handle_call({chrono_update, Chrono}, _From, State) ->
	
	% Phase 1: update the status %
	
	% update with the new values and calculates consumption per lap if possible
	CarStats = lists:keyfind(Chrono#chrono_notif.car, #car_stats.car_id, State#state.cars_stats),
	NCStats = case CarStats of
				  false ->
					  #car_stats{car_id = Chrono#chrono_notif.car,
								 pitstop_count = 0,
								 last_ls = [Chrono],
								 avg_consumption = {undef, undef}};
				  CarStats ->
					  LastLS = CarStats#car_stats.last_ls,
					  {NewLLS, Cons} = case lists:keyfind(Chrono#chrono_notif.intermediate,
														  #chrono_notif.intermediate,
														  LastLS) of
										   false ->
											   {[Chrono, LastLS], {undef, undef}};
										   OldNotif ->
											   NS = Chrono#chrono_notif.status,
											   OS = OldNotif#chrono_notif.status,
											   Laps = Chrono#chrono_notif.lap - OldNotif#chrono_notif.lap,
											   % TyresC and FuelC can be undef if a pitstop occurred
											   C = delta_consumption(OS, NS, Laps),
											   Temp = lists:keydelete(Chrono#chrono_notif.intermediate,
																	  #chrono_notif.intermediate,
																	  LastLS),
											   {[Chrono | Temp], C}
									   end,
					  CarStats#car_stats{last_ls = NewLLS, avg_consumption = Cons}
			  end,
	DelCS = lists:keydelete(Chrono#chrono_notif.car, #car_stats.car_id, State#state.cars_stats),
	NewState = State#state{cars_stats = [NCStats | DelCS]},
	
	% Phase 2: calculate next pitstop %
	
	% check if it has an appropriate tyres type
	AvgRain = State#state.rain_sum / utils:get_setting(sgm_number),
	BestTyres = best_tyres(AvgRain, ?TYRES_SPECS),
	CS = Chrono#chrono_notif.status,
	PSCount = NCStats#car_stats.pitstop_count,
	if
		CS#car_status.tyres_type /= BestTyres ->
			% schedule a pitstop for the current lap
			car:set_next_pitstop(Chrono#chrono_notif.car,
								 #next_pitstop{lap = Chrono#chrono_notif.lap,
											   stops_count = PSCount});
		true ->
			% when will the car need the next pitstop?
			{TCRatio, FCRatio} = NCStats#car_stats.avg_consumption,
			TS = CS#car_status.tyres_consumption,
			FS = CS#car_status.fuel,
			Next = calculate_laps_left(TS, FS, TCRatio, FCRatio, NewState),
			case is_number(Next) of
				true ->
					car:set_next_pitstop(Chrono#chrono_notif.car,
										 #next_pitstop{lap = Chrono#chrono_notif.lap + Next,
													   stops_count = PSCount});
				false -> % not enough information
					ok
			end
	end,
	{reply, ok, NewState};

handle_call({pitstop, CarId, CarStatus, Lap, CarPSCount}, _From, State) ->
	AvgRain = State#state.rain_sum / utils:get_setting(sgm_number),
	BestTyres = best_tyres(AvgRain, ?TYRES_SPECS),
	CarStats = lists:keyfind(CarId, #car_stats.car_id, State#state.cars_stats),
	{_TyresC, FuelC} = CarStats#car_stats.avg_consumption,
	LapsLeft = utils:get_setting(total_laps) - Lap,
	Fuel = CarStatus#car_status.fuel,
	NeededFuel = LapsLeft * FuelC + State#state.fuel_limit,
	AddF = case NeededFuel > ?TANK_DIM of
			   true ->
				   ?TANK_DIM - Fuel;
			   false ->
				   NeededFuel - Fuel
		   end,
	Rest = lists:keydelete(CarId, #car_stats.car_id, State#state.cars_stats),
	NewCS = CarStats#car_stats{pitstop_count = CarPSCount},
	Reply = #pitstop_ops{tyres = BestTyres, fuel = AddF},
	{reply, Reply, State#state{cars_stats = [NewCS | Rest]}};

handle_call(Msg, From, State) ->
	?WARN({"unhandled call", Msg, "from", From}),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%% --------------------------------------------------------------------
%% Function: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

best_tyres(Rain, [H | T]) when is_record(H, tyres_interval) ->
	case Rain >= H#tyres_interval.min andalso Rain < H#tyres_interval.max of
		true -> H#tyres_interval.type;
		false -> best_tyres(Rain, T)
	end;
best_tyres(_Rain, []) ->
	null.

%% Returns {TyreC, FuelC}
delta_consumption(OS, NS, Laps) when is_record(OS, car_status),
									 is_record(NS, car_status) ->
	TyresC = (NS#car_status.tyres_consumption - OS#car_status.tyres_consumption) / Laps,
	FuelC = (OS#car_status.fuel - NS#car_status.fuel) / Laps,
	Fun = fun(X) ->
				  if
					  X > 0 -> X;
					  true -> undef
				  end
		  end,
	{Fun(TyresC), Fun(FuelC)}.

%% Returns undef when no estimation can be done
calculate_laps_left(TS, FS, TCRatio, FCRatio, State) when is_record(State, state) ->
	Fun = fun({Left, Ratio}) ->
				  if
					  not is_number(Left) ->
						  undef;
					  Left > 0 ->
						  trunc(Left / Ratio);
					  true ->
						  0
				  end
		  end,
	TempT = case is_number(TS) of
				true ->
					State#state.tyres_limit - TS;
				false ->
					TS
			end,
	TempF = case is_number(FS) of
				true ->
					FS - State#state.fuel_limit;
				false ->
					FS
			end,
	lists:min(lists:map(Fun, [{TempT, TCRatio}, {TempF, FCRatio}])).
