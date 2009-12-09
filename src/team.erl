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

-type consumption() :: {TyresCons :: float() | 'undef',
						FuelCons :: float() | 'undef'}.

%% last_ls: list of chrono_notif containing at most
%%			one record for each intermediate
-record(car_stats, {car_id								:: car(),
					pitstop_count						:: non_neg_integer(),
					avg_consumption	= {undef, undef}	:: consumption(),
					last_ls			= []				:: [#chrono_notif{}]}).

%% fuel_limit: if a car's fuel is lesser than the limit
%%			   then this car should stop at the pits
%% tyres_limit: if a car's tyres_consumption is greater than
%%				the limit then this car should stop at the pits
%% rain_sum: sum of rain field of all the segments
%% cars_stats: list of car_stats records
-record(state, {fuel_limit	= 0.0	:: float(),
				tyres_limit	= 60.0	:: float(),
				rain_sum			:: non_neg_integer(),
				cars_stats	= []	:: [#car_stats{}]}).

%% type: type of tyres
%% min: minimum value of rain index
%% max: maximum value of rain index
-record(tyres_interval, {type	:: tyres(),
						 min	:: float(),
						 max	:: float()}).

%% List of tyres_interval records, it represents the intervals
%% within which each different type of tyres performs better.
-define(TYRES_SPECS, [#tyres_interval{type = slick, min = 0.0, max = 3.0},
					  #tyres_interval{type = intermediate, min = 3.0, max = 6.5},
					  #tyres_interval{type = wet, min = 6.5, max = 10.0}]).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link(conflist()) -> start_result().

start_link(Config) when is_list(Config) ->
	{id, TeamId} = lists:keyfind(id, 1, Config),
	gen_server:start_link(?TEAM_NAME(TeamId), ?MODULE, Config, []).

-spec pitstop_operations(pos_integer(), car(), #car_status{}, non_neg_integer(),
						 non_neg_integer()) -> #pitstop_ops{}.

pitstop_operations(TeamId, CarId, CarStatus, Lap, PSCount)
  when is_record(CarStatus, car_status) ->
	gen_server:call(?TEAM_NAME(TeamId), {pitstop, CarId, CarStatus, Lap, PSCount}, infinity).

-spec update(pos_integer(), {'update', #chrono_notif{}}
						  | {'init', {'rain_sum', non_neg_integer()}}) -> 'ok'.

update(TeamId, {init, {rain_sum, RainSum}}) ->
	gen_server:call(?TEAM_NAME(TeamId), {set_rain_sum, RainSum});
update(TeamId, {update, Chrono}) when is_record(Chrono, chrono_notif) ->
	gen_server:call(?TEAM_NAME(TeamId), {chrono_update, Chrono}).


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
	Parse = fun
			   ({id, Id}, CT)
				 when is_integer(Id), Id > 0 ->
					CB = #callback{mod = ?MODULE, func = update, args = [Id]},
					event_dispatcher:subscribe(team, CB),
					CT#car_type{id = Id};
			   ({team_name, Name}, CT)
				 when is_list(Name) ->
					CT#car_type{team_name = Name};
			   ({brake, Brake}, CT)
				 when is_number(Brake), Brake > 0 ->
					CT#car_type{brake = -1 * Brake};
			   ({power, Power}, CT)
				 when is_number(Power), Power > 0 ->
					CT#car_type{power = Power};
			   ({weight, Weight}, CT)
				 when is_number(Weight), Weight > 0 ->
					CT#car_type{weight = Weight};
			   (_, _CT) ->
					throw("team configuration error")
			end,
	CarType = lists:foldl(Parse, #car_type{}, Config),
	{atomic, ok} = mnesia:sync_transaction(fun() -> mnesia:write(CarType) end),
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
											   {[Chrono | LastLS], {undef, undef}};
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
	% dynamically adapt fuel_limit
	NewState = case NCStats#car_stats.avg_consumption of
				   {_, undef} ->
					   State#state{cars_stats = [NCStats | DelCS]};
				   {_, NewFuelCons} ->
					   State#state{cars_stats = [NCStats | DelCS],
								   fuel_limit = 2 * NewFuelCons}
			   end,
	
	% Phase 2: calculate next pitstop %
	
	% check if it has an appropriate tyres type
	AvgRain = State#state.rain_sum / utils:get_setting(sgm_number),
	BestTyres = best_tyres(AvgRain, ?TYRES_SPECS),
	CS = Chrono#chrono_notif.status,
	PSCount = NCStats#car_stats.pitstop_count,
	if
		CS#car_status.tyres_type /= BestTyres ->
			% schedule a pitstop for the current lap
			%?DBG({"scheduling an immediate pitstop for car", Chrono#chrono_notif.car}),
			car:set_next_pitstop(Chrono#chrono_notif.car,
								 #next_pitstop{lap = Chrono#chrono_notif.lap,
											   stops_count = PSCount});
		true ->
			% when will the car need the next pitstop?
			AvgCons = NCStats#car_stats.avg_consumption,
			TS = CS#car_status.tyres_consumption,
			FS = CS#car_status.fuel,
			case calculate_laps_left(TS, FS, AvgCons, NewState) of
				undef ->
					% not enough information
					ok;
				Next when is_integer(Next) ->
					CarId = Chrono#chrono_notif.car,
					PSLap = Chrono#chrono_notif.lap + Next,
					%?DBG({"scheduling a pitstop in lap", PSLap, "for car", CarId}),
					car:set_next_pitstop(CarId, #next_pitstop{lap = PSLap,
															  stops_count = PSCount})
			end
	end,
	{reply, ok, NewState};

handle_call({pitstop, CarId, CarStatus, Lap, CarPSCount}, _From, State) ->
	AvgRain = State#state.rain_sum / utils:get_setting(sgm_number),
	BestTyres = case best_tyres(AvgRain, ?TYRES_SPECS) of
					null ->
						% no appropriate tyres_type found: keep the old one
						CarStatus#car_status.tyres_type;
					Else ->
						Else
				end,
	CarStats = lists:keyfind(CarId, #car_stats.car_id, State#state.cars_stats),
	{_TyresC, FuelC} = CarStats#car_stats.avg_consumption,
	LapsLeft = utils:get_setting(total_laps) - Lap,
	Fuel = CarStatus#car_status.fuel,
	NeededFuel = case FuelC of
					 undef ->
						 ?TANK_DIM;
					 _ ->
						 LapsLeft * FuelC + State#state.fuel_limit
				 end,
	AddF = if
			   NeededFuel >= ?TANK_DIM ->
				   ?TANK_DIM - Fuel;
			   Fuel >= NeededFuel ->
				   0.0;
			   true ->
				   NeededFuel - Fuel
		   end,
	Rest = lists:keydelete(CarId, #car_stats.car_id, State#state.cars_stats),
	NewCS = CarStats#car_stats{pitstop_count = CarPSCount + 1},
	Reply = #pitstop_ops{tyres = BestTyres, fuel = AddF},
	{reply, Reply, State#state{cars_stats = [NewCS | Rest]}};

handle_call({set_rain_sum, RainSum}, _From, State) ->
	{reply, ok, State#state{rain_sum = RainSum}};

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

-spec best_tyres(float(), [#tyres_interval{}]) -> tyres() | 'null'.

best_tyres(Rain, [H | T]) ->
	case Rain >= H#tyres_interval.min andalso Rain < H#tyres_interval.max of
		true -> H#tyres_interval.type;
		false -> best_tyres(Rain, T)
	end;
best_tyres(_Rain, []) ->
	null.

-spec delta_consumption(#car_status{}, #car_status{}, integer()) -> consumption().

delta_consumption(OS, NS, Laps) ->
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
-spec calculate_laps_left(float(), float(), consumption(), #state{}) ->
						  non_neg_integer() | 'undef'.

calculate_laps_left(TS, FS, {TCRatio, FCRatio}, State) ->
	Fun = fun({Left, Ratio}) ->
				  if
					  Ratio == undef ->
						  undef;
					  Left > 0 ->
						  trunc(Left / Ratio);
					  true ->
						  0
				  end
		  end,
	TempT = State#state.tyres_limit - TS,
	TempF = FS - State#state.fuel_limit,
	lists:min(lists:map(Fun, [{TempT, TCRatio}, {TempF, FCRatio}])).
