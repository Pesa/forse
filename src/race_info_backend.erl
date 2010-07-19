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

-module(race_info_backend).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 preprocess_sectors/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-record(state, {subscribers		= []			:: [#subscriber{}],
				finish_line_index				:: intermediate(),
				race_state		= initialized	:: race_state(),
				speedup							:: pos_integer(),
				sectors			= []			:: [tuple()],
				cars_pos		= []			:: [{car(), Pos :: non_neg_integer(), Pit :: boolean()}],
				pilots			= []			:: [{car(), team(), CarName :: string(),
													 TeamName :: string() | 'undefined'}],
				teams			= []			:: [{team(), Name :: string()}],
				standings		= []			:: [{car(), Pos :: non_neg_integer(), car_state()}],
				best_lap						:: {car(), lap(), time()},
				max_speed						:: {car(), intermediate(), lap(), float()},
				last_interm						:: {intermediate(), lap()},
				last_finish		= []			:: [{car(), lap(), time()}]}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link() -> start_result().

start_link() ->
	gen_server:start_link(?LOCAL_NAME, ?MODULE, [], []).

-spec preprocess_sectors([sector()]) -> [tuple()].

preprocess_sectors(Sectors) when is_list(Sectors) ->
	F = fun({pitlane_entrance}, _Pit) ->
				{{pitlane_entrance}, true};
		   ({pitlane_exit}, _Pit) ->
				{{pitlane_exit}, false};
		   ({finish_line}, Pit) ->
				{{finish_line, Pit}, Pit};
		   ({finish_line, _}, Pit) ->
				{{finish_line, Pit}, Pit};
		   ({straight, Len, _, _, _, _}, Pit) ->
				{{straight, Len, Pit}, Pit};
		   ({straight, Len, _}, Pit) ->
				{{straight, Len, Pit}, Pit};
		   ({Type, Len, Curv, _, _, _, _}, Pit)
			 when Type == left orelse Type == right ->
				{{Type, Len, Curv, Pit}, Pit};
		   ({Type, Len, Curv, _}, Pit)
			 when Type == left orelse Type == right ->
				{{Type, Len, Curv, Pit}, Pit};
		   (Other, Pit) ->
				{Other, Pit}
		end,
	{Sectors2, Pit} = lists:mapfoldl(F, false, Sectors),
	{Sectors3, _} = lists:mapfoldl(F, Pit, Sectors2),
	Sectors3.


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
	process_flag(trap_exit, true),
	mnesia:subscribe({table, track, detailed}),
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
	List = [{sectors, State#state.sectors},
			{cars_pos, State#state.cars_pos},
			{race_state, State#state.race_state},
			{speedup, State#state.speedup},
			{names, lists:map(fun name/1, State#state.pilots)},
			{cars_state, extract_states(State#state.standings)},
			{standings, extract_standings(State#state.standings)}],
	List1 = case State#state.max_speed of
				undefined -> List;
				SR -> [{speed_record, SR} | List]
			end,
	List2 = case State#state.best_lap of
				undefined -> List1;
				BL -> [{best_lap, BL} | List1]
			end,
	NewSubs = event_dispatcher:add_subscriber(S, State#state.subscribers, List2),
	{noreply, State#state{subscribers = NewSubs}};

handle_cast(#car_state_notif{car = Car, state = {retired, Reason}}, State) ->
	{_, RetPos, _} = lists:keyfind(Car, 1, State#state.standings),
	Sorted = lists:keysort(2, State#state.standings),
	Split = fun({_, _, {retired, _}}) -> false;
			   (_) -> true
			end,
	{Others, RetiredCars} = lists:splitwith(Split, Sorted),
	F = fun({Id, Pos, _}, Acc) when Pos == RetPos ->
				NewStand = {Id, length(Others), {retired, Reason}},
				{NewStand, [NewStand | Acc]};
		   ({Id, Pos, S}, Acc) when Pos > RetPos ->
				NewStand = {Id, Pos - 1, S},
				{NewStand, [NewStand | Acc]};
		   (Standing, Acc) ->
				{Standing, Acc}
		end,
	{NewRunStands, Changes} = lists:mapfoldl(F, [], Others),
	Subs1 = event_dispatcher:notify_init({cars_state, [{Car, {retired, Reason}}]},
										 State#state.subscribers),
	Subs2 = event_dispatcher:notify_update({standings, extract_standings(Changes)}, Subs1),
	{noreply, State#state{subscribers = Subs2,
						  standings = NewRunStands ++ RetiredCars}};

handle_cast(#car_state_notif{car = Car, state = CarState}, State) ->
	{_, Pos, _} = lists:keyfind(Car, 1, State#state.standings),
	NewStands = lists:keyreplace(Car, 1, State#state.standings, {Car, Pos, CarState}),
	Subs = event_dispatcher:notify_init({cars_state, [{Car, CarState}]},
										State#state.subscribers),
	{noreply, State#state{subscribers = Subs,
						  standings = NewStands}};

handle_cast(Msg, State) when is_record(Msg, chrono_notif) ->
	Car = Msg#chrono_notif.car,
	Int = Msg#chrono_notif.intermediate,
	Lap = Msg#chrono_notif.lap,
	Time = Msg#chrono_notif.time,
	Subs = State#state.subscribers,
	
	% check if there's a new speed record
	{MaxSpeed, Subs1} = case State#state.max_speed of
							_ when Lap == 0 ->
								{undefined, Subs};
							undefined ->
								% notify new speed record
								SR = {Car, Int, Lap, Msg#chrono_notif.max_speed},
								Subs2 = event_dispatcher:notify_init({speed_record, SR}, Subs),
								{SR, Subs2};
							{_, _, _, Best} when Best < Msg#chrono_notif.max_speed ->
								% notify new speed record
								SR = {Car, Int, Lap, Msg#chrono_notif.max_speed},
								Subs2 = event_dispatcher:notify_init({speed_record, SR}, Subs),
								{SR, Subs2};
							_ ->
								{State#state.max_speed, Subs}
						end,
	
	% send intermediate results
	LI = State#state.last_interm,
	{NewLI, Subs3} = if
						 Lap == 0 ->
							 {undefined, Subs1};
						 LI == undefined;
						 Lap > element(2, LI);
						 Int > element(1, LI) andalso Lap >= element(2, LI) ->
							 {_, _, LineTime} = lists:keyfind(Car, 1, State#state.last_finish),
							 RelTime = Time - LineTime,
							 M = {chrono, Car, Int, Lap, RelTime, Time},
							 Subs4 = event_dispatcher:notify_init(M, Subs1),
							 {{Int, Lap}, Subs4};
						 true ->
							 M = {chrono, Car, Int, Lap, Time},
							 Subs4 = event_dispatcher:notify_update(M, Subs1),
							 {LI, Subs4}
					 end,
	
	% check if there's a new time record
	FinishLine = Int == State#state.finish_line_index,
	LF = State#state.last_finish,
	OldBestLap = State#state.best_lap,
	{BestLap, NewLF, Subs5} =
		if
			Lap == 0 andalso FinishLine ->
				{OldBestLap, [{Car, Lap, Time} | LF], Subs3};
			FinishLine ->
				{_, _, LineTime1} = lists:keyfind(Car, 1, LF),
				LF2 = lists:keyreplace(Car, 1, LF, {Car, Lap, Time}),
				LapTime = Time - LineTime1,
				if
					OldBestLap == undefined;
					LapTime < element(3, OldBestLap) ->
						NewBestLap = {Car, Lap, LapTime},
						Subs6 = event_dispatcher:notify_init({best_lap, NewBestLap}, Subs3),
						{NewBestLap, LF2, Subs6};
					true ->
						{OldBestLap, LF2, Subs3}
				end;
			true ->
				{OldBestLap, LF, Subs3}
		end,
	
	{noreply, State#state{subscribers = Subs5,
						  best_lap = BestLap,
						  max_speed = MaxSpeed,
						  last_interm = NewLI,
						  last_finish = NewLF}};

handle_cast(#config_notif{app = scheduler, config = Config}, State) ->
	{speedup, Speedup} = lists:keyfind(speedup, 1, Config),
	Subs = event_dispatcher:notify_init({speedup, Speedup}, State#state.subscribers),
	{noreply, State#state{subscribers = Subs,
						  speedup = Speedup}};

handle_cast(#config_notif{app = track, config = Config}, State) ->
	{finish_line_index, FLI} = lists:keyfind(finish_line_index, 1, Config),
	
	{sectors, Sectors} = lists:keyfind(sectors, 1, Config),
	S = preprocess_sectors(Sectors),
	Subs1 = event_dispatcher:notify_init({sectors, S}, State#state.subscribers),
	
	{starting_pos, StartPos} = lists:keyfind(starting_pos, 1, Config),
	Sorted = lists:keysort(2, StartPos),
	CarsPos = lists:map(fun({CarId, Pos}) ->
								% {CarId, Position (on track), Pit}
								{CarId, Pos, false}
						end, Sorted),
	Subs2 = event_dispatcher:notify_init({cars_pos, CarsPos}, Subs1),
	
	% build initial standings
	{Standings, _} = lists:mapfoldl(fun({CarId, _}, Acc) ->
											% {CarId, Ranking, State}
											{{CarId, Acc, ready}, Acc + 1}
									end, 1, lists:reverse(Sorted)),
	Subs3 = event_dispatcher:notify_init({cars_state, extract_states(Standings)}, Subs2),
	Subs4 = event_dispatcher:notify_init({standings, extract_standings(Standings)}, Subs3),
	
	{noreply, State#state{subscribers = Subs4,
						  finish_line_index = FLI,
						  cars_pos = CarsPos,
						  sectors = S,
						  standings = Standings}};

handle_cast(#config_notif{app = car, config = Pilot}, State) ->
	TeamName = case lists:keyfind(Pilot#pilot.team, 1, State#state.teams) of
				   false -> undefined;
				   {_Id, Name} -> Name
			   end,
	NewPilot = {Pilot#pilot.id, Pilot#pilot.team, Pilot#pilot.name, TeamName},
	Subs = case TeamName of
			   undefined -> State#state.subscribers;
			   _ -> event_dispatcher:notify_init({names, [name(NewPilot)]},
												 State#state.subscribers)
		   end,
	{noreply, State#state{pilots = [NewPilot | State#state.pilots],
						  subscribers = Subs}};

handle_cast(#config_notif{app = team, config = CarType}, State) ->
	SetTeam = fun({Id, T, N, undefined}, Acc) when T == CarType#car_type.id ->
					  Pilot = {Id, T, N, CarType#car_type.team_name},
					  S = event_dispatcher:notify_init({names, [name(Pilot)]}, Acc),
					  {Pilot, S};
				 (X, Acc) ->
					  {X, Acc}
			  end,
	{Pilots, Subs} = lists:mapfoldl(SetTeam, State#state.subscribers, State#state.pilots),
	Teams = [{CarType#car_type.id, CarType#car_type.team_name} | State#state.teams],
	{noreply, State#state{subscribers = Subs,
						  pilots = Pilots,
						  teams = Teams}};

handle_cast(Msg, State) when is_record(Msg, config_notif) ->
	% ignore config_notif from other apps
	{noreply, State};

handle_cast(#race_notif{event = Ev}, State) ->
	RaceState = case Ev of
					started -> running;
					resumed -> running;
					Else -> Else
				end,
	Subs = event_dispatcher:notify_init({race_state, RaceState}, State#state.subscribers),
	{noreply, State#state{subscribers = Subs,
						  race_state = RaceState}};
	
handle_cast(Msg, State) when is_record(Msg, surpass_notif) ->
	{_, SedP, SedS} = lists:keyfind(Msg#surpass_notif.surpassed, 1, State#state.standings),
	{_, SerP, SerS} = lists:keyfind(Msg#surpass_notif.surpasser, 1, State#state.standings),
	if
		SedP + 1 == SerP ->
			Ser = {Msg#surpass_notif.surpasser, SedP, SerS},
			Sed = {Msg#surpass_notif.surpassed, SerP, SedS},
			S1 = lists:keyreplace(Msg#surpass_notif.surpasser, 1, State#state.standings, Ser),
			S2 = lists:keyreplace(Msg#surpass_notif.surpassed, 1, S1, Sed),
			Subs = event_dispatcher:notify_update({standings, extract_standings([Sed, Ser])},
												  State#state.subscribers),
			{noreply, State#state{subscribers = Subs,
								  standings = S2}};
		true ->
			{noreply, State}
	end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({mnesia_table_event, {write, track, NewSgm, OldSgms, _}}, State)
  when is_record(NewSgm, segment) ->
	OldQueue = case lists:keyfind(NewSgm#segment.id, #segment.id, OldSgms) of
				   #segment{queued_cars = Q} -> Q;
				   false -> []
			   end,
	Diff = NewSgm#segment.queued_cars -- OldQueue,
	CPs = State#state.cars_pos,
	Subs = State#state.subscribers,
	{NewCPs, NewSubs} = case Diff of
							[#car_position{car_id = CarId, exit_lane = Lane}] ->
								case lists:keytake(CarId, 1, CPs) of
									{value, {_, Pos, _}, Rest} ->
										NewPos = Pos + NewSgm#segment.length,
										NewCP = {CarId, NewPos, Lane < 0},
										{[NewCP | Rest],
										 event_dispatcher:notify_update({cars_pos, [NewCP]}, Subs)};
									false ->
										{CPs, Subs}
								end;
							_ ->
								{CPs, Subs}
						end,
	{noreply, State#state{cars_pos = NewCPs,
						  subscribers = NewSubs}};
handle_info(_Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
	case State#state.race_state of
		finished -> ok;
		_ -> event_dispatcher:notify_init({race_state, terminated},
										  State#state.subscribers)
	end.

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

extract_standings(S) ->
	lists:map(fun({Id, Pos, _}) -> {Id, Pos} end, S).

extract_states(S) ->
	lists:map(fun({Id, _, State}) -> {Id, State} end, S).

name({Id, _, N, TN}) ->
	{Id, N, TN}.
