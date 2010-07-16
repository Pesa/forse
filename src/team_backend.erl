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

-module(team_backend).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-type time_speed_record() :: {intermediate(), Time :: time(), Speed :: float()}
						   | {'lap', lap(), Time :: time()}.

-record(pilot_info, {msg_opt			:: atom(),
					 id					:: car(),
					 name				:: string(),
					 state				:: car_state(),
					 car_status			:: #consumption{},
					 pit_count			:: non_neg_integer(),
					 records	= []	:: [time_speed_record()],
					 last_interm		:: time(),
					 last_finish		:: time()}).

-record(state, {subscribers		= []	:: [#subscriber{}],
				finish_line_index		:: intermediate(),
				rain_sum				:: non_neg_integer(),
				pilots			= []	:: [#pilot_info{}],
				teams			= []	:: [{team(), Name :: string()}]}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link() -> start_result().

start_link() ->
	gen_server:start_link(?LOCAL_NAME, ?MODULE, [], []).


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
init([]) ->
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
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({subscribe, S}, State) when is_record(S, subscriber) ->
	Pred = fun(E) ->
				   lists:member(E#pilot_info.msg_opt, S#subscriber.opts)
		   end,
	Pilots = lists:filter(Pred, State#state.pilots),
	PilotMsgs = lists:flatmap(fun build_sub_msgs/1, Pilots),
	List = PilotMsgs ++ [{max_fuel, ?TANK_DIM},
						 {names, State#state.teams},
						 {rain_sum, State#state.rain_sum}],
	NewSubs = event_dispatcher:add_subscriber(S, State#state.subscribers, List),
	{noreply, State#state{subscribers = NewSubs}};

handle_cast(#car_state_notif{car = CarId, state = CarState}, State) ->
	PInfo = lists:keyfind(CarId, #pilot_info.id, State#state.pilots),
	Pilots = lists:keyreplace(CarId, #pilot_info.id, State#state.pilots,
							  PInfo#pilot_info{state = CarState}),
	Msg = {car_state, CarId, CarState},
	Subs = event_dispatcher:notify_init(PInfo#pilot_info.msg_opt, Msg, State#state.subscribers),
	{noreply, State#state{subscribers = Subs,
						  pilots = Pilots}};

handle_cast(Msg, State) when is_record(Msg, chrono_notif) ->
	Car = Msg#chrono_notif.car,
	PInfo = lists:keyfind(Car, #pilot_info.id, State#state.pilots),
	CNStatus = Msg#chrono_notif.status,
	
	% notify car status
	Status = #consumption{car = Car,
						  intermediate = Msg#chrono_notif.intermediate,
						  lap = Msg#chrono_notif.lap,
						  fuel = CNStatus#car_status.fuel,
						  tyres_consumption = CNStatus#car_status.tyres_consumption,
						  tyres_type = CNStatus#car_status.tyres_type},
	Opt = PInfo#pilot_info.msg_opt,
	Subs1 = event_dispatcher:notify_init(Opt, Status, State#state.subscribers),
	
	PInfoUp = PInfo#pilot_info{car_status = Status},
	{NewPInfo, Subs2} = calculate_time(Msg, PInfoUp, Subs1, State#state.finish_line_index),
	Pilots = lists:keyreplace(Car, #pilot_info.id, State#state.pilots, NewPInfo),
	{noreply, State#state{subscribers = Subs2,
						  pilots = Pilots}};

handle_cast(#config_notif{app = track, config = Config}, State) ->
	{initial_rain_sum, RainSum} = lists:keyfind(initial_rain_sum, 1, Config),
	NewSubs = event_dispatcher:notify_init({rain_sum, RainSum},
										   State#state.subscribers),
	{finish_line_index, FLI} = lists:keyfind(finish_line_index, 1, Config),
	{noreply, State#state{subscribers = NewSubs,
						  finish_line_index = FLI,
						  rain_sum = RainSum}};

handle_cast(#config_notif{app = car, config = Pilot}, State) ->
	PilotId = Pilot#pilot.id,
	TeamId = Pilot#pilot.team,
	CNStatus = Pilot#pilot.car_status,
	CS = #consumption{car = PilotId,
					  intermediate = start,
					  lap = start,
					  fuel = CNStatus#car_status.fuel,
					  tyres_consumption = CNStatus#car_status.tyres_consumption,
					  tyres_type = CNStatus#car_status.tyres_type},
	Opt = utils:build_id_atom("", TeamId),
	PInfo = #pilot_info{id = PilotId,
						name = Pilot#pilot.name,
						state = ready,
						car_status = CS,
						pit_count = Pilot#pilot.pitstop_count,
						msg_opt = Opt},
	% dispatch info about new pilot
	InitMsg = build_new_pilot_msg(PInfo),
	Subs1 = event_dispatcher:notify_init(Opt, InitMsg, State#state.subscribers),
	Subs2 = event_dispatcher:notify_init(Opt, CS, Subs1),
	{noreply, State#state{pilots = State#state.pilots ++ [PInfo],
						  subscribers = Subs2}};

handle_cast(#config_notif{app = team, config = CarType}, State) ->
	T = {CarType#car_type.id, CarType#car_type.team_name},
	Subs = case State#state.teams of
			   [] ->
				   event_dispatcher:notify_init({names, [T]}, State#state.subscribers);
			   _ ->
				   event_dispatcher:notify_update({names, T}, State#state.subscribers)
		   end,
	{noreply, State#state{teams = State#state.teams ++ [T],
						  subscribers = Subs}};

handle_cast(Msg, State) when is_record(Msg, config_notif) ->
	% ignore config_notif from other apps
	{noreply, State};

handle_cast(Msg, State) when is_record(Msg, pitstop_notif) ->
	Car = Msg#pitstop_notif.car,
	PInfo = lists:keyfind(Car, #pilot_info.id, State#state.pilots),
	PC = PInfo#pilot_info.pit_count + 1,
	Ops = Msg#pitstop_notif.ops,
	Pilots = lists:keyreplace(Car, #pilot_info.id, State#state.pilots,
							  PInfo#pilot_info{pit_count = PC}),
	% {pitstop, CarId, PitCount, AddedFuel, Tyres}
	PitMsg = {pitstop, Car, PC, Ops#pitstop_ops.fuel, Ops#pitstop_ops.tyres},
	Subs = event_dispatcher:notify_init(PInfo#pilot_info.msg_opt, PitMsg, State#state.subscribers),
	{noreply, State#state{subscribers = Subs,
						  pilots = Pilots}};

handle_cast(#weather_notif{changes = Changes}, State) ->
	F = fun(#weather_change{old_weather = W1, new_weather = W2}, Sum) ->
				Sum + W2 - W1
		end,
	NewRainSum = lists:foldl(F, State#state.rain_sum, Changes),
	NewSubs = event_dispatcher:notify_init({rain_sum, NewRainSum},
										   State#state.subscribers),
	{noreply, State#state{subscribers = NewSubs,
						  rain_sum = NewRainSum}}.

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

calculate_time(Chrono, PInfo, Subs, FLI) ->
	Car = Chrono#chrono_notif.car,
	Lap = Chrono#chrono_notif.lap,
	Int = Chrono#chrono_notif.intermediate,
	Time = Chrono#chrono_notif.time,
	Speed = Chrono#chrono_notif.max_speed,
	Opt = PInfo#pilot_info.msg_opt,
	
	IntTime = case PInfo#pilot_info.last_interm of
				  undefined ->
					  undefined;
				  N ->
					  Time - N
			  end,
	{Rec1, Subs2} = if
						IntTime == undefined ->
							{PInfo#pilot_info.records, Subs};
						true ->
							% {chrono, CarId, Interm, Lap, Time, Speed}
							Msg = {chrono, Car, Int, Lap, IntTime, Speed},
							Subs1 = event_dispatcher:notify_init(Opt, Msg, Subs),
							calculate_int_records(Car, Int, IntTime, Speed,
												  PInfo#pilot_info.records, Subs1, Opt)
					end,	
	LastFinish = PInfo#pilot_info.last_finish,
	{Rec2, Subs3, LF} = if
							Int /= FLI ->
								{Rec1, Subs2, LastFinish};
							LastFinish == undefined ->
								{Rec1, Subs2, Time};
							true ->
								{R, S} = calculate_lap_records(Car, Lap, LastFinish, Time, Rec1, Subs2, Opt),
								{R, S, Time}
						end,
	NewPInfo = PInfo#pilot_info{records = Rec2,
								last_interm = Time,
								last_finish = LF},
	{NewPInfo, Subs3}.

%% Calculates and notifies new intermediate records.
calculate_int_records(CarId, Int, Time, Speed, Records, Subs, Opt) ->
	case lists:keyfind(Int, 1, Records) of
		false ->
			% {best_time, CarId, Int, Time}
			BTMsg = {best_time, CarId, Int, Time},
			Subs1 = event_dispatcher:notify_init(Opt, BTMsg, Subs),
			
			% {best_speed, CarId, Int, Speed}
			BSMsg = {best_speed, CarId, Int, Speed},
			Subs2 = event_dispatcher:notify_init(Opt, BSMsg, Subs1),
			
			{[{Int, Time, Speed} | Records], Subs2};
		
		{_, T, S} when Time < T; Speed > S ->
			RT = if
					 T > Time -> Time;
					 true -> false
				 end,
			RS = if
					 S < Speed -> Speed;
					 true -> false
				 end,
			
			% notify new records
			Subs1 = case RT of
						false ->
							Subs;
						_ ->
							% {best_time, CarId, Int, Time}
							BTMsg = {best_time, CarId, Int, RT},
							event_dispatcher:notify_init(Opt, BTMsg, Subs)
					end,
			Subs2 = case RS of
						false ->
							Subs1;
						_ ->
							% {best_speed, CarId, Int, Speed}
							BSMsg = {best_speed, CarId, Int, RS},
							event_dispatcher:notify_init(Opt, BSMsg, Subs1)
					end,
			
			NewBestInt = {Int, erlang:min(T, Time), erlang:max(S, Speed)},
			NewRecords = lists:keyreplace(Int, 1, Records, NewBestInt),
			{NewRecords, Subs2};
		
		_ ->
			{Records, Subs}
	end.

calculate_lap_records(Car, Lap, LastFinish, Time, Records, Subs, Opt) ->
	LapTime = Time - LastFinish,
	% {last_lap, CarId, LapTime}
	Subs1 = event_dispatcher:notify_init(Opt, {last_lap, Car, LapTime}, Subs),
	case lists:keyfind(lap, 1, Records) of
		false ->
			% {best_lap, CarId, LapTime}
			BLMsg = {best_lap, Car, LapTime},
			Subs2 = event_dispatcher:notify_init(Opt, BLMsg, Subs1),
			{[{lap, Lap, LapTime} | Records], Subs2};
		{lap, _BestLap, BestTime} when BestTime > LapTime ->
			% {best_lap, CarId, LapTime}
			BLMsg = {best_lap, Car, LapTime},
			Subs2 = event_dispatcher:notify_init(Opt, BLMsg, Subs1),
			{lists:keyreplace(lap, 1, Records, {lap, Lap, LapTime}), Subs2};
		_ ->
			{Records, Subs1}
	end.

build_new_pilot_msg(PInfo) when is_record(PInfo, pilot_info) ->
	% {new_pilot, Id, Name, Status, PitCount}
	{new_pilot,
	 PInfo#pilot_info.id,
	 PInfo#pilot_info.name,
	 PInfo#pilot_info.state,
	 PInfo#pilot_info.pit_count}.

build_sub_msgs(PInfo) when is_record(PInfo, pilot_info) ->
	Car = PInfo#pilot_info.id,
	MapFun = fun({lap, _Lap, Time}) ->
					 [{best_lap, Car, Time}];
				({Int, Time, Speed}) ->
					 [{best_time, Car, Int, Time},
					  {best_speed, Car, Int, Speed}];
				(_) ->
					 []
			 end,
	Records = lists:flatmap(MapFun, PInfo#pilot_info.records),
	[build_new_pilot_msg(PInfo),
	 PInfo#pilot_info.car_status | Records].
