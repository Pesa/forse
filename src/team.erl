-module(team).

-behaviour(gen_server).

%% External exports
-export([start_link/1]).

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
-record(car_stats, {car_id,
					pitstop_count,
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
	%% Update with the new values and calculates consumption per lap if possible
	CarStats = lists:keyfind(Chrono#chrono_notif.car, #car_stats.car_id, State#state.cars_stats),
	Res = case CarStats of
			  false ->
				  {#car_stats{car_id = Chrono#chrono_notif.car,
							  pitstop_count = 0,
							  last_ls = [Chrono]},
				   {undef, undef}};
			  CarStats -> 
				  LastLS = CarStats#car_stats.last_ls,
				  {NewLLS, Cons} = case lists:keyfind(Chrono#chrono_notif.intermediate, 
													  #chrono_notif.intermediate,
													  LastLS) of
									   false ->
										   {[Chrono, LastLS], {undef, undef}};
									   OldNotif ->
										   %% Qui posso calcolare i consumi per giro etc... TODO
										   NS = Chrono#chrono_notif.status,
										   OS = OldNotif#chrono_notif.status,
										   %% TyresC and FuelC can be undef if a pitstop occurred
										   C = delta_consumption(OS, NS),
										   
										   Temp = lists:keydelete(Chrono#chrono_notif.intermediate, 
																  #chrono_notif.intermediate,
																  LastLS),
										   {[Chrono | Temp], C}
								   end,
				  {CarStats#car_stats{last_ls = NewLLS}, Cons}
		  end,
	{NewCarStats, {TC, FC}} = Res,
	DelCS = lists:delete(Chrono#chrono_notif.car, #car_stats.car_id, State#state.cars_stats),
	NewState = State#state{cars_stats = [NewCarStats | DelCS]},
	
	%% Checks if it has an appropriate tyres type
	AvgRain = State#state.rain_sum/utils:get_setting(sgm_number),
	BestTyres = best_tyres(AvgRain, State#state.tyres_int),
	CS = Chrono#chrono_notif.status,
	if
		CS#car_status.tyres_type /= BestTyres ->
			%% TODO vai ai box subito
			ok;
		true ->
			%%TODO calcola per quanti giri bastano gomme e carb e invia
			ok
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

delta_consumption(OS, NS) when is_record(OS, car_status),
							   is_record(NS, car_status) ->
	TyresC = NS#car_status.tyres_consumption - OS#car_status.tyres_consumption,
	FuelC = OS#car_status.fuel - NS#car_status.fuel,
	Fun = fun(X) ->
				  if
					  X > 0 -> X;
					  true -> undef
				  end
		  end,
	{Fun(TyresC), Fun(FuelC)}.