%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
%%                      Daniele Battaglia <dbat.fk@gmail.com>
%%
%%  This file is part of FORSE.
%%
%%  FORSE is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  FORSE is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-type avg_consumption() :: {TyresCons :: float() | 'undef',
							FuelCons :: float() | 'undef'}.

%% last_ls: list of chrono_notif containing at most
%%			one record for each intermediate
-record(car_stats, {car_id								:: car(),
					pitstop_count						:: non_neg_integer(),
					avg_consumption	= {undef, undef}	:: avg_consumption(),
					last_ls			= []				:: [#consumption{}]}).

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
						 max	:: float() | 'infinity'}).

%% List of tyres_interval records, it represents the intervals
%% within which each different type of tyres performs better.
-define(TYRES_SPECS, [#tyres_interval{type = slick, min = 0.0, max = 3.0},
					  #tyres_interval{type = intermediate, min = 3.0, max = 6.5},
					  #tyres_interval{type = wet, min = 6.5, max = infinity}]).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link(conflist()) -> start_result().

start_link(Config) when is_list(Config) ->
	{id, TeamId} = lists:keyfind(id, 1, Config),
	gen_server:start_link(?TEAM_NAME(TeamId), ?MODULE, Config, []).

-spec pitstop_operations(team(), car(), #car_status{}, non_neg_integer(),
						 non_neg_integer()) -> #pitstop_ops{}.

pitstop_operations(TeamId, CarId, CarStatus, Lap, PSCount)
  when is_record(CarStatus, car_status) ->
	gen_server:call(?TEAM_NAME(TeamId), {pitstop, CarId, CarStatus, Lap, PSCount}, infinity).

-spec update(team(), {'update', #consumption{}}
				   | {'init', {'rain_sum', non_neg_integer()}}) -> 'ok'.

update(TeamId, {init, {rain_sum, RainSum}}) ->
	gen_server:call(?TEAM_NAME(TeamId), {set_rain_sum, RainSum});

update(TeamId, {init, Status}) when is_record(Status, consumption) ->
	case Status#consumption.lap of
		start ->
			ok;
		_ ->
			gen_server:call(?TEAM_NAME(TeamId), {status_update, Status})
	end;

update(_TeamId, _) ->
	ok.


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
					Opts = [utils:build_id_atom("", Id)],
					ok = event_dispatcher:subscribe(team, CB, Opts),
					CT#car_type{id = Id};
			   ({team_name, Name}, CT)
				 when is_list(Name) ->
					CT#car_type{team_name = Name};
			   ({brake, Brake}, CT)
				 when is_number(Brake), Brake > 0 ->
					CT#car_type{brake = -Brake};
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
	mnesia:activity(sync_transaction, fun() -> mnesia:write(CarType) end),
	event_dispatcher:notify(#config_notif{app = ?MODULE, config = CarType}),
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
handle_call({status_update, Status}, _From, State) ->
	CarId = Status#consumption.car,
	
	% Phase 1: update the status %
	
	% update with the new values and calculates consumption per lap if possible
	CarStats = lists:keyfind(CarId, #car_stats.car_id, State#state.cars_stats),
	NCStats = case CarStats of
				  false ->
					  #car_stats{car_id = CarId,
								 pitstop_count = 0,
								 last_ls = [Status],
								 avg_consumption = {undef, undef}};
				  CarStats ->
					  LastLS = CarStats#car_stats.last_ls,
					  {NewLLS, Cons} = case lists:keyfind(Status#consumption.intermediate,
														  #consumption.intermediate,
														  LastLS) of
										   false ->
											   {[Status | LastLS], {undef, undef}};
										   OldNotif ->
											   Laps = Status#consumption.lap - OldNotif#consumption.lap,
											   % TyresC and FuelC can be undef if a pitstop occurred
											   C = delta_consumption(OldNotif, Status, Laps),
											   Temp = lists:keydelete(Status#consumption.intermediate,
																	  #consumption.intermediate,
																	  LastLS),
											   {[Status | Temp], C}
									   end,
					  CarStats#car_stats{last_ls = NewLLS, avg_consumption = Cons}
			  end,
	DelCS = lists:keydelete(CarId, #car_stats.car_id, State#state.cars_stats),
	% dynamically adapt fuel_limit
	NewState = case NCStats#car_stats.avg_consumption of
				   {_, undef} ->
					   State#state{cars_stats = [NCStats | DelCS]};
				   {_, NewFuelCons} ->
					   State#state{cars_stats = [NCStats | DelCS],
								   fuel_limit = 2 * NewFuelCons}
			   end,
	
	% Phase 2: calculate next pitstop %
	
	case calculate_next_pitstop(Status, NewState#state.rain_sum,
								NCStats#car_stats.avg_consumption,
								NewState#state.fuel_limit,
								NewState#state.tyres_limit) of
		undefined ->
			% no estimate could be done
			ok;
		Lap ->
			NextPitstop = #next_pitstop{lap = Lap,
										stops_count = NCStats#car_stats.pitstop_count},
			car:set_next_pitstop(CarId, NextPitstop)
	end,
	{reply, ok, NewState};

handle_call({pitstop, CarId, CarStatus, Lap, CarPSCount}, _From, State) ->
	AvgRain = State#state.rain_sum / utils:get_setting(sgm_number),
	BestTyres = best_tyres(AvgRain, ?TYRES_SPECS),
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
handle_cast(Msg, State) ->
	?WARN({"unhandled cast", Msg}),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?WARN({"unhandled info", Info}),
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

-spec best_tyres(float(), [#tyres_interval{}]) -> tyres().

best_tyres(Rain, [H | _])
  when Rain >= H#tyres_interval.min, Rain < H#tyres_interval.max ->
	H#tyres_interval.type;
best_tyres(Rain, [_ | T]) ->
	best_tyres(Rain, T).

-spec delta_consumption(#consumption{}, #consumption{}, integer()) -> avg_consumption().

delta_consumption(OS, NS, Laps) ->
	TyresC = (NS#consumption.tyres_consumption - OS#consumption.tyres_consumption) / Laps,
	FuelC = (OS#consumption.fuel - NS#consumption.fuel) / Laps,
	Fun = fun(X) ->
				  if
					  X > 0 -> X;
					  true -> undef
				  end
		  end,
	{Fun(TyresC), Fun(FuelC)}.

-spec calculate_next_pitstop(#consumption{}, non_neg_integer() | 'undefined',
							 avg_consumption(), float(), float()) ->
		pos_integer() | 'undefined'.

calculate_next_pitstop(Status, undefined, {TyresC, FuelC}, FuelLimit, TyresLimit) ->
	% when will the car need the next pitstop?
	MapFun = fun({Left, Ratio}) ->
					 if
						 Ratio == undef ->
							 undef;
						 Left > 0 ->
							 trunc(Left / Ratio);
						 true ->
							 0
					 end
			 end,
	T = TyresLimit - Status#consumption.tyres_consumption,
	F = Status#consumption.fuel - FuelLimit,
	case lists:min(lists:map(MapFun, [{T, TyresC}, {F, FuelC}])) of
		undef ->
			% not enough information
			undefined;
		Next when is_integer(Next) ->
			Status#consumption.lap + Next
	end;
calculate_next_pitstop(Status, RainSum, AvgCons, FL, TL) ->
	% check if the car's tyres_type is appropriate
	% for the current amount of rain on the track
	AvgRain = RainSum / utils:get_setting(sgm_number),
	BestTyres = best_tyres(AvgRain, ?TYRES_SPECS),
	if
		Status#consumption.tyres_type /= BestTyres ->
			% schedule a pitstop for the current lap
			Status#consumption.lap;
		true ->
			calculate_next_pitstop(Status, undefined, AvgCons, FL, TL)
	end.
