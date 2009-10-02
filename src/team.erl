-module(team).

-behaviour(gen_server).

%% External exports
-export([start_link/1,
		 weather_update/1,
		 chrono_update/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-define(TEAM_NAME(Id), {global, utils:build_id_atom("team_", Id)}).

%% last_ls: list of chrono_notif containing at most one record for each intermediate
%% avg_consumption: {TyresC , FuelC}
-record(car_stats, {car_id,
					pitstop_count,
					avg_consumption = {undef, undef},
					last_ls = []
				   }).
%% type: slick | intermediate | wet
%% min: minimum value of rain index
%% max: maximum value of rain index
-record(tyres_interval, {type,
						 min,
						 max}).

%% fuel_limit: if a car's fuel is lesser than the limit then this car shuold stop to pits
%% tyres_limit: if a car's tyres_consumption is greater than the limit then this car shuold stop to pits
%% rain_sum: sum of rain field of all the segments
%% tyres_int: list of tyres_intervals, represent the intervals in which each different type of 
%% tyres performs better
%% cars_stats: list of car_stats
-record(state, {fuel_limit,
				tyres_limit,
				rain_sum,
				tyres_int = [#tyres_interval{type = slick, min = 0.0, max = 3.0},
							 #tyres_interval{type = intermediate, min = 3.0, max = 6.5},
							 #tyres_interval{type = wet, min = 6.5, max = 10.0}],
				cars_stats = []}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Config) when is_list(Config) ->
	{id, TeamId} = lists:keyfind(id, 1, Config),
	gen_server:start_link(?TEAM_NAME(TeamId), ?MODULE, Config, []).

weather_update(Delta) when is_integer(Delta) ->
	gen_server:call(?GLOBAL_NAME, {weather_update, Delta}, infinity).

chrono_update(Notif) when is_record(Notif, chrono_notif) ->
	gen_server:call(?GLOBAL_NAME, {chrono_update, Notif}, infinity).
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

handle_call({chrono_update, Chrono}, _From, State) when is_record(Chrono, chrono_notif) ->
	
	%% Phase 1: Update the status
	
	%% Update with the new values and calculates consumption per lap if possible
	CarStats = lists:keyfind(Chrono#chrono_notif.car, #car_stats.car_id, State#state.cars_stats),
	NCStats = case CarStats of
				  false ->
					  #car_stats{car_id = Chrono#chrono_notif.car,
								 pitstop_count = 0,
								 last_ls = [Chrono],
								 avg_consumption = {undef, undef}};
				  CarStats -> 
					  LastLS = CarStats#car_stats.last_ls,
					  Res = case lists:keyfind(Chrono#chrono_notif.intermediate, 
											   #chrono_notif.intermediate,
											   LastLS) of
								false ->
									{[Chrono, LastLS], {undef, undef}};
								OldNotif ->
									NS = Chrono#chrono_notif.status,
									OS = OldNotif#chrono_notif.status,
									Laps = Chrono#chrono_notif.lap - OldNotif#chrono_notif.lap,
									%% TyresC and FuelC can be undef if a pitstop occurred
									C = delta_consumption(OS, NS, Laps),
									
									Temp = lists:keydelete(Chrono#chrono_notif.intermediate, 
														   #chrono_notif.intermediate,
														   LastLS),
									{[Chrono | Temp], C}
							end,
					  {NewLLS, Cons} = Res,
					  CarStats#car_stats{last_ls = NewLLS, avg_consumption = Cons}
			  end,
	
	DelCS = lists:delete(Chrono#chrono_notif.car, #car_stats.car_id, State#state.cars_stats),
	NewState = State#state{cars_stats = [NCStats | DelCS]},
	
	%% Phase 2: Calculate next pitstop
	
	%% Checks if it has an appropriate tyres type
	AvgRain = State#state.rain_sum/utils:get_setting(sgm_number),
	BestTyres = best_tyres(AvgRain, State#state.tyres_int),
	CS = Chrono#chrono_notif.status,
	if
		CS#car_status.tyres_type /= BestTyres ->
			%% Schedule a pitstop for the current lap
			car:set_next_pitstop(Chrono#chrono_notif.car,
								 #next_pitstop{lap = Chrono#chrono_notif.lap,
											   stops_count = NCStats#car_stats.pitstop_count});
		true ->
			%% When will the car need the next pitstop?
			{TCRatio, FCRatio} = NCStats#car_stats.avg_consumption,
			TS = CS#car_status.tyres_consumption,
			FS = CS#car_status.fuel,
			Next = calculate_laps_left(TS, FS, TCRatio, FCRatio),
			case is_number(Next) of
				false ->
					%% Not enough information
					ok;
				true ->
					car:set_next_pitstop(Chrono#chrono_notif.car,
										 #next_pitstop{lap = Chrono#chrono_notif.lap + Next,
													   stops_count = NCStats#car_stats.pitstop_count})
			end
	end,
	{reply, ok, NewState};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

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

best_tyres(Rain, [H | T]) when is_number(Rain),
							   is_record(H, tyres_interval) ->
	case Rain >= H#tyres_interval.min andalso Rain < H#tyres_interval.max of
		true -> H#tyres_interval.type;
		false -> best_tyres(Rain, T)
	end;

best_tyres(_, []) ->
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

%% If no prevision can be done returns undef
calculate_laps_left(TS, FS, TCRatio, FCRatio) ->
	Fun = fun({Left, Ratio}) ->
				  case is_number(Left) of
					  false -> 
						  undef;
					  true ->
						  trunc(Left / Ratio)
				  end
		  end,
	lists:min(lists:map(Fun, [{100.0 - TS, TCRatio}, {FS, FCRatio}])).
					  