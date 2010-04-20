-module(track).

%% Exported Functions
-export([init/3,
		 move/3,
		 simulate/3,
		 preelaborate/1,
		 is_pre_pitlane/1,
		 reachable_lanes/2,
		 where_am_i/1]).

-include("db_schema.hrl").


%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

%% Initializes the track table and a few settings.
-spec init([sector()], [team()], [car()]) -> 'ok' | {'error', Reason :: term()}.
init(TrackConfig, TeamsList, CarsList)
  when is_list(TrackConfig), is_list(TeamsList), is_list(CarsList) ->
	try
		{Ph1, SgmNum, PitStart, PitEnd, Config} = parse_config(TrackConfig),
		Ph2 = set_chrono_lanes(Ph1, SgmNum),
		Ph3 = fill_starting_grid(lists:sort(CarsList), Ph2, SgmNum),
		{StartPos, PitLen} = calculate_start_pos_and_pit_len(Ph3, PitStart, PitEnd),
		SgmList = build_pit_area(Ph3, SgmNum, PitStart, PitLen div ?SEGMENT_LENGTH, TeamsList),
		
		T = fun() ->
					mnesia:write_lock_table(track),
					lists:foreach(fun(Sgm) ->
										  mnesia:write(track, Sgm, write)
								  end, SgmList)
			end,
		mnesia:activity(sync_transaction, T),
		
		% calculate and store intermediates' indexes
		Map = build_intermediate_map(lists:filter(fun is_chrono_sgm/1, SgmList)),
		utils:set_setting(intermediate_map, Map),
		
		% notify the final configuration
		C = [{finish_line_index, length(Map)},
			 {starting_pos, StartPos} | Config],
		event_dispatcher:notify(#config_notif{app = ?MODULE, config = C})
	catch
		error: {badmatch, _} = Error ->
			{error, Error};
		throw: Error ->
			{error, Error}
	end.

-spec parse_config([sector()]) ->
		{[#segment{}], pos_integer(), sgm_id() | -1, sgm_id() | -1, conflist()}.
parse_config(TrackConfig) ->
	{SgmList, SgmNum, SectorsMap, PitStart, PitEnd, RainSum} =
		build_sector(TrackConfig, [], [], 0, 0, -1, -1, 0),
	F = fun({K, V}) ->
				Sector = utils:build_id_atom("sector_", K),
				utils:set_setting(Sector, V)
		end,
	lists:foreach(F, SectorsMap),
	utils:set_setting(sgm_number, SgmNum),
	Config = [{sectors, TrackConfig},
			  {sectors_map, SectorsMap},
			  {initial_rain_sum, RainSum}],
	{SgmList, SgmNum, PitStart, PitEnd, Config}.

-spec build_sector([sector()], [#segment{}], [{non_neg_integer(), {sgm_id(), sgm_id()}}],
				   non_neg_integer(), sgm_id(), sgm_id() | -1, sgm_id() | -1, non_neg_integer()) ->
		{[#segment{}], sgm_id(), [{non_neg_integer(), {sgm_id(), sgm_id()}}],
		 sgm_id() | -1, sgm_id() | -1, non_neg_integer()}.

build_sector([], SgmList, SectorsMap, _Sect, Sgm, PitS, PitE, RainSum) ->
	{SgmList, Sgm, SectorsMap, PitS, PitE, RainSum};

build_sector([{straight, Len, MinLane, MaxLane, Incl, Rain} | Tail],
			 SgmList, SectorsMap, Sect, Sgm, PitS, PitE, RainSum)
  when is_number(Len), Len > 0, is_integer(MinLane), is_integer(MaxLane),
	   MinLane > 0, MinLane =< MaxLane, is_number(Incl),
	   is_integer(Rain), Rain >= 0, Rain =< 10 ->
	S = #segment{type = normal,
				 min_lane = MinLane,
				 max_lane = MaxLane,
				 length = ?SEGMENT_LENGTH,
				 inclination = Incl,
				 curvature = 0.0,
				 rain = Rain},
	SgmNum = round(Len / ?SEGMENT_LENGTH),
	Last = SgmNum + Sgm - 1,
	NewSgms = [ S#segment{id = X} || X <- lists:seq(Sgm, Last) ],
	NewMapping = {Sect, {Sgm, Last}},
	build_sector(Tail, NewSgms ++ SgmList, [NewMapping | SectorsMap],
				 Sect + 1, Last + 1, PitS, PitE, RainSum + SgmNum * Rain);

build_sector([{Type, Len, Curv, MinLane, MaxLane, Incl, Rain} | Tail],
			 SgmList, SectorsMap, Sect, Sgm, PitS, PitE, RainSum)
  when (Type == left orelse Type == right), is_number(Len), Len > 0,
	   is_integer(MinLane), is_integer(MaxLane), MinLane > 0, MinLane =< MaxLane,
	   is_number(Curv), Curv > 0, is_number(Incl),
	   is_integer(Rain), Rain >= 0, Rain =< 10 ->
	S = #segment{type = normal,
				 min_lane = MinLane,
				 max_lane = MaxLane,
				 length = ?SEGMENT_LENGTH,
				 inclination = Incl,
				 curvature = Curv,
				 rain = Rain},
	SgmNum = round(Len / ?SEGMENT_LENGTH),
	Last = SgmNum + Sgm - 1,
	NewSgms = [ S#segment{id = X} || X <- lists:seq(Sgm, Last) ],
	NewMapping = {Sect, {Sgm, Last}},
	build_sector(Tail, NewSgms ++ SgmList, [NewMapping | SectorsMap],
				 Sect + 1, Last + 1, PitS, PitE, RainSum + SgmNum * Rain);

build_sector([{finish_line} | Tail], SgmList, SectorsMap, Sect, Sgm, PitS, PitE, RainSum) ->
	S = #segment{id = Sgm,
				 type = finish_line,
				 length = 0},
	build_sector(Tail, [S | SgmList], SectorsMap,
				 Sect, Sgm + 1, PitS, PitE, RainSum);

build_sector([{intermediate} | Tail], SgmList, SectorsMap, Sect, Sgm, PitS, PitE, RainSum) ->
	S = #segment{id = Sgm,
				 type = intermediate,
				 length = 0},
	build_sector(Tail, [S | SgmList], SectorsMap,
				 Sect, Sgm + 1, PitS, PitE, RainSum);

build_sector([{pitlane_entrance} | Tail], SgmList, SectorsMap, Sect, Sgm, -1, PitE, RainSum) ->
	build_sector(Tail, SgmList, SectorsMap, Sect, Sgm, Sgm, PitE, RainSum);
build_sector([{pitlane_entrance} | _], _SgmList, _SectorsMap, _Sect, _Sgm, _PitS, _PitE, _RainSum) ->
	throw("multiple pitlane entrances");

build_sector([{pitlane_exit} | Tail], SgmList, SectorsMap, Sect, Sgm, PitS, -1, RainSum) ->
	build_sector(Tail, SgmList, SectorsMap, Sect, Sgm, PitS, Sgm, RainSum);
build_sector([{pitlane_exit} | _], _SgmList, _SectorsMap, _Sect, _Sgm, _PitS, _PitE, _RainSum) ->
	throw("multiple pitlane exits");

build_sector([S | _], _SgmList, _SectorsMap, _Sect, _Sgm, _PitS, _PitE, _RainSum) ->
	throw({"invalid sector", S}).

-spec build_pit_area([#segment{}], pos_integer(), sgm_id() | -1,
					 non_neg_integer(), [pos_integer()]) -> [#segment{}].
build_pit_area(SgmList, SgmNum, PitStart, PitLen, TeamsList) ->
	% 100 m before and after boxes
	Pit = 100 div ?SEGMENT_LENGTH,
	Rem = PitLen - (3 * length(TeamsList) + 2 * Pit),
	if
		% 150 m of pre/post_pitlane
		Rem < 300 div ?SEGMENT_LENGTH ->
			throw("pitlane is too short");
		true ->
			ok
	end,
	PrePit = (Rem + 1) div 2,
	PostPit = Rem - PrePit,
	{T1, N1} = set_sgm_type(pre_pitlane, PitStart, PrePit, SgmList),
	{T2, N2} = set_sgm_type(pitlane, N1, Pit, T1),
	{T3, N3} = build_pitstop(N2, TeamsList, T2, SgmNum),
	{T4, N4} = set_sgm_type(pitlane, N3, Pit, T3),
	{T5, _} = set_sgm_type(post_pitlane, N4, PostPit, T4),
	T5.

set_chrono_lanes(List, SgmNum) ->
	Chrono = lists:filter(fun is_chrono_sgm/1, List),
	set_chrono_lanes(Chrono, List, SgmNum).

set_chrono_lanes([], List, _SgmNum) ->
	List;
set_chrono_lanes([H | T], List, SgmNum) ->
	Id = H#segment.id,
	Pre = lists:keyfind(prev_segment(Id, SgmNum), #segment.id, List),
	Post = lists:keyfind(next_segment(Id), #segment.id, List),
	MaxLane = erlang:max(Pre#segment.max_lane, Post#segment.max_lane),
	MinLane = erlang:min(Pre#segment.min_lane, Post#segment.min_lane),
	NewSgm = H#segment{min_lane = MinLane,
					   max_lane = MaxLane},
	NewList = lists:keyreplace(Id, #segment.id, List, NewSgm),
	set_chrono_lanes(T, NewList, SgmNum).

%% Calculates the starting position of
%% each car and the pit area length
-spec calculate_start_pos_and_pit_len([#segment{}], sgm_id() | -1, sgm_id() | -1) ->
		{conflist(), non_neg_integer()}.
calculate_start_pos_and_pit_len(_SgmList, -1, _PitEnd) ->
	throw("missing pitlane entrance");
calculate_start_pos_and_pit_len(_SgmList, _PitStart, -1) ->
	throw("missing pitlane exit");
calculate_start_pos_and_pit_len(SgmList, PitStart, PitEnd) ->
	SortedList = lists:keysort(#segment.id, SgmList),
	FL = (lists:keyfind(finish_line, #segment.type, SortedList))#segment.id,
	{Tail, Head} = lists:split(FL, SortedList),
	G = fun(S, {Len, PosList, PS1, PE1}) ->
				NewLen = Len + S#segment.length,
				{PS2, PE2} = case S#segment.id of
								 PitStart ->
									 {NewLen, PE1};
								 PitEnd ->
									 {PS1, NewLen};
								 _ ->
									 {PS1, PE1}
							 end,
				case S#segment.queued_cars of
					[#car_position{car_id = CarId}] ->
						{NewLen, [{CarId, NewLen} | PosList], PS2, PE2};
					_ ->
						{NewLen, PosList, PS2, PE2}
				end
		end,
	{TrackLen, StartPos, PS, PE} = lists:foldl(G, {0, [], 0, 0}, Head ++ Tail),
	PitLen = if
				 PE >= PS ->
					 PE - PS;
				 true ->
					 PE - PS + TrackLen
			 end,
	{StartPos, PitLen}.

build_pitstop(Start, [], SgmList, _SgmNum) ->
	{SgmList, Start};
build_pitstop(Start, [TeamId | Tail], SgmList, SgmNum) ->
	{L1, N1} = set_sgm_type(pitlane, Start, 1, SgmList),
	{L2, N2} = set_sgm_type(pitstop, N1, 1, L1),
	{L3, N3} = set_sgm_type(pitlane, N2, 1, L2),
	PitSgmId = prev_segment(N2, SgmNum),
	T = fun() ->
				[CT] = mnesia:wread({car_type, TeamId}),
				mnesia:write(car_type, CT#car_type{pitstop_sgm = PitSgmId}, write)
		end,
	mnesia:activity(sync_transaction, T),
	build_pitstop(N3, Tail, L3, SgmNum).

set_sgm_type(_Type, Start, 0, Sgms) ->
	{Sgms, Start};
set_sgm_type(Type, Start, Num, Sgms) ->
	{value, S, Rest} = lists:keytake(Start, #segment.id, Sgms),
	Next = next_segment(Start),
	case S#segment.type of
		intermediate ->
			set_sgm_type(Type, Next, Num, Sgms);
		finish_line ->
			set_sgm_type(Type, Next, Num, Sgms);
		normal ->
			NewSgm = S#segment{type = Type},
			set_sgm_type(Type, Next, Num - 1, [NewSgm | Rest]);
		_ ->
			throw("track is too short")
	end.

-spec fill_starting_grid([car()], [#segment{}], pos_integer()) -> [#segment{}].
fill_starting_grid(CarsList, SgmList, SgmNum) ->
	Line = (lists:keyfind(finish_line, #segment.type, SgmList))#segment.id,
	First = prev_segment(prev_segment(Line, SgmNum), SgmNum),
	add_cars(CarsList, SgmList, First, 1, SgmNum).

-spec add_cars([car()], [#segment{}], sgm_id(), 1 | 2, pos_integer()) -> [#segment{}].
add_cars([H | T] = IdList, SgmList, Index, LanePos, SNum) ->
	Sgm = lists:keyfind(Index, #segment.id, SgmList),
	Type = Sgm#segment.type,
	if
		Type == intermediate;
		Type == finish_lane ->
			add_cars(IdList, SgmList, prev_segment(Index, SNum), LanePos, SNum);
		true ->
			NextLP = case LanePos of
						 1 -> 2;
						 2 -> 1
					 end,
			NewSgm = place_car(H, Sgm#segment.min_lane, Sgm#segment.max_lane, Sgm, LanePos),
			NewList = lists:keyreplace(Index, #segment.id, SgmList, NewSgm),
			add_cars(T, NewList, prev_segment(Index, SNum), NextLP, SNum)
	end;
add_cars([], SgmList, _Index, _LanePos, _SNum) ->
	SgmList.

-spec place_car(car(), lane(), lane(), #segment{}, 1 | 2) -> #segment{}.
place_car(CarId, MinLane, MaxLane, Sgm, LanePos) ->
	Lanes = MaxLane - MinLane + 1,
	if
		Lanes < 3 ->
			throw("starting grid is too narrow");
		true ->
			Delta = (Lanes - 3) div 2,
			L = case LanePos of
					1 -> MinLane + Delta;
					2 -> MaxLane - Delta
				end,
			CP = #car_position{car_id = CarId,
							   enter_lane = L,
							   exit_lane = L},
			Sgm#segment{queued_cars = [CP]}
	end.


%% Moves the car to the next segment.
%% Pit: true if pilot wants to stop at the pits
-spec move(#pilot{}, lane(), boolean()) ->
		{NextTime :: float(), #pilot{}} | 'fail' | 'race_ended'.
move(Pilot, ExitLane, Pit) when is_record(Pilot, pilot) ->
	Sgm = next_segment(Pilot#pilot.segment),
	SOld = utils:mnesia_read(track, Pilot#pilot.segment),
	% FTNOTE: check if keyfind returns false
	CarPos = lists:keyfind(Pilot#pilot.id, #pilot.id, SOld#segment.queued_cars),
	EnterLane = Pilot#pilot.lane,
	S = utils:mnesia_read(track, Sgm),
	
	case simulate(Pilot, S, EnterLane, ExitLane, Pit, CarPos) of
		race_ended ->
			remove_car(SOld, Pilot#pilot.id),
			race_ended;
		pits ->
			CarStatus = Pilot#pilot.car_status,
			Ops = team:pitstop_operations(Pilot#pilot.team, Pilot#pilot.id, CarStatus,
										  Pilot#pilot.lap, Pilot#pilot.pitstop_count),
			PitstopTime = pitstop_time(Ops),
			% check if there's another car at the pits
			CarBoxPos = physics:get_car_ahead(S, ExitLane, 1),
			CPExitTime = CarPos#car_position.exit_t,
			ETime = case CarBoxPos of
						null ->
							CPExitTime;
						_ ->
							erlang:max(CPExitTime, CarBoxPos#car_position.exit_t)
					end,
			NewCarPos = CarPos#car_position{speed = 0.0,
											enter_t = CPExitTime,
											exit_t = ETime + PitstopTime,
											enter_lane = EnterLane,
											exit_lane = ExitLane},
			
			% update car_position in track table
			move_car(SOld, S, NewCarPos),
			Fuel = CarStatus#car_status.fuel + Ops#pitstop_ops.fuel,
			NewCarStatus = #car_status{fuel = Fuel,
									   tyres_consumption = 0.0,
									   tyres_type = Ops#pitstop_ops.tyres},
			NewPilot = Pilot#pilot{segment = Sgm,
								   lane = ExitLane,
								   car_status = NewCarStatus,
								   next_pitstop = undefined,
								   pitstop_count = Pilot#pilot.pitstop_count + 1,
								   run_preelab = true},
			
			% finally notify the event_dispatcher
			event_dispatcher:notify(#pitstop_notif{car = Pilot#pilot.id, ops = Ops}),
			{NewCarPos#car_position.exit_t, NewPilot};
		{fail, Reason} ->
			event_dispatcher:notify(#retire_notif{car = Pilot#pilot.id,
												  reason = Reason}),
			remove_car(SOld, Pilot#pilot.id),
			?DBG({"car", Pilot#pilot.id, "crashed in segment", Sgm}),
			fail;
		{ok, Time, Speed} ->
			NewCarPos = CarPos#car_position{speed = Speed,
											enter_t = CarPos#car_position.exit_t,
											exit_t = CarPos#car_position.exit_t + Time,
											enter_lane = EnterLane,
											exit_lane = ExitLane},
			
			MaxSpeed = erlang:max(Pilot#pilot.max_speed, NewCarPos#car_position.speed),
			% update car_position in track table
			move_car(SOld, S, NewCarPos),
			NewCarStatus = update_car_status(Pilot#pilot.car_status, S),
			NewPilot = case S#segment.type of
						   intermediate ->
							   Msg = #chrono_notif{car = Pilot#pilot.id,
												   lap = Pilot#pilot.lap,
												   intermediate = intermediate_index(S#segment.id),
												   time = NewCarPos#car_position.exit_t,
												   max_speed = MaxSpeed,
												   status = NewCarStatus},
							   event_dispatcher:notify(Msg),
							   Pilot#pilot{segment = Sgm,
										   lane = ExitLane,
										   car_status = NewCarStatus,
										   max_speed = 0.0};
						   finish_line ->
							   Msg = #chrono_notif{car = Pilot#pilot.id,
												   lap = Pilot#pilot.lap,
												   intermediate = intermediate_index(S#segment.id),
												   time = NewCarPos#car_position.exit_t,
												   max_speed = MaxSpeed,
												   status = NewCarStatus},
							   event_dispatcher:notify(Msg),
							   Pilot#pilot{segment = Sgm,
										   lap = Pilot#pilot.lap + 1,
										   lane = ExitLane,
										   car_status = NewCarStatus,
										   max_speed = 0.0,
										   run_preelab = true};
						   _ ->
							   Pilot#pilot{segment = Sgm,
										   lane = ExitLane,
										   car_status = NewCarStatus}
					   end,
			{NewCarPos#car_position.exit_t, NewPilot}
	end.


%% Returns the time needed by Car to cover the next segment.
%% Pit: true if pilot wants to stop at the pits
-spec simulate(#pilot{}, lane(), boolean()) ->
		Time :: float() | {'fail', Reason :: atom()} | 'pits' | 'race_ended'.
simulate(Pilot, ExitLane, Pit) when is_record(Pilot, pilot) ->
	Sgm = next_segment(Pilot#pilot.segment),
	SOld = utils:mnesia_read(track, Pilot#pilot.segment),
	% FTNOTE: check if keyfind returns false
	CarPos = lists:keyfind(Pilot#pilot.id, #pilot.id, SOld#segment.queued_cars),
	EnterLane = Pilot#pilot.lane,
	S = utils:mnesia_read(track, Sgm),
	case simulate(Pilot, S, EnterLane, ExitLane, Pit, CarPos) of
		{ok, Time, _Speed} -> Time;
		Else -> Else
	end.

-spec simulate(#pilot{}, #segment{}, lane(), lane(), boolean(), #car_position{}) ->
		{ok, Time :: float(), Speed :: float()} | {'fail', Reason :: atom()} | 'pits' | 'race_ended'.
simulate(Pilot, S, EnterLane, ExitLane, Pit, CarPos) ->
	CS = Pilot#pilot.car_status,
	TotalLaps = utils:get_setting(total_laps),
	if
		Pilot#pilot.lap > TotalLaps ->
			race_ended;
		CS#car_status.tyres_consumption >= 100.0 ->
			{fail, 'tyres exploded'};
		CS#car_status.fuel =< 0.0 ->
			{fail, 'out of fuel'};
		Pilot#pilot.retire ->
			{fail, 'team request'};
		true ->
			case access:check_move(Pilot, S, EnterLane, ExitLane, Pit) of
				go ->
					EnterTime = CarPos#car_position.exit_t,
					Space = S#segment.length,
					EnterSpeed = CarPos#car_position.speed,
					Car = utils:mnesia_read(car_type, Pilot#pilot.team),
					FAcc = Car#car_type.power,
					FDec = Car#car_type.brake,
					Mass = Car#car_type.weight + Pilot#pilot.weight
							+ CS#car_status.fuel * ?FUEL_SPEC_GRAVITY,
					Inc = physics:deg_to_rad(S#segment.inclination),
					Bound = utils:mnesia_read(?PREELAB_TABLE(Pilot#pilot.id), S#segment.id),
					
					% if in pit area use lane bound otherwise choose using Pit value
					PL = is_pit_area_lane(S, ExitLane),
					SB = case is_pit_area(S) of
							 true when PL -> Bound#speed_bound.pit_bound;
							 true -> Bound#speed_bound.bound;
							 false when Pit -> Bound#speed_bound.pit_bound;
							 false -> Bound#speed_bound.bound
						 end,
					MaxExitSpeed = erlang:min(SB, physics:engine_max_speed(Car#car_type.power)),
					
					Amin = physics:acceleration(FDec, Mass, Inc, CS, S#segment.rain),
					Amax = physics:acceleration(FAcc, Mass, Inc, CS, S#segment.rain),
					SkCoeff = 0.9 + Pilot#pilot.skill / 100.0,
					physics:simulate(S, EnterLane, ExitLane, EnterTime, 1, Space,
									 EnterSpeed, MaxExitSpeed, Amin, Amax, SkCoeff);
				Else -> Else
			end
	end.


%% Calculates the maximum speed that Pilot's car
%% can reach in each segment of the track.
-spec preelaborate(#pilot{}) -> 'ok'.
preelaborate(Pilot) when is_record(Pilot, pilot) ->
	Car = utils:mnesia_read(car_type, Pilot#pilot.team),
	CarStatus = Pilot#pilot.car_status,
	Mass = Car#car_type.weight + Pilot#pilot.weight
			+ CarStatus#car_status.fuel * ?FUEL_SPEC_GRAVITY,
	FDec = Car#car_type.brake,
	SgmNum = utils:get_setting(sgm_number),
	
	Bounds = preelab_bent_and_pit(CarStatus),
	{MinBoundIndex, MinSpeed} = min_bound(Bounds),
	BoundsPre = preelab_sgm(Bounds, #speed_bound.bound, FDec,
							prev_segment(MinBoundIndex, SgmNum),
							MinBoundIndex, MinSpeed, CarStatus,
							Mass, SgmNum),
	{MinPitBoundIndex, MinPitSpeed} = min_pit_bound(Bounds),
	FinalBounds = preelab_sgm(BoundsPre, #speed_bound.pit_bound, FDec,
							  prev_segment(MinPitBoundIndex, SgmNum),
							  MinPitBoundIndex, MinPitSpeed,
							  CarStatus, Mass, SgmNum),
	
	TabName = ?PREELAB_TABLE(Pilot#pilot.id),
	T = fun() ->
				mnesia:write_lock_table(TabName),
				lists:foreach(fun(Elem) ->
									  mnesia:write(TabName, Elem, sticky_write)
							  end, FinalBounds)
		end,
	mnesia:activity(sync_transaction, T),
	ok.

%% Recursively calculates the speed bounds for AttIndex.
%% FDec: power of brakes
%% Sgm: id of the segment that is being computed
%% LastSgm: id of min speed bound segment
%% VNext: speed bound of the next segment
%% SgmNum: total number of segments in the track
-spec preelab_sgm([#speed_bound{}], pos_integer(), number(), sgm_id(), sgm_id(), float(),
				  #car_status{}, float(), pos_integer()) -> [#speed_bound{}].
preelab_sgm(BoundList, AttIndex, FDec, Sgm, LastSgm, VNext, CarStatus, Mass, SgmNum) ->
	S = utils:mnesia_read(track, Sgm),
	Length = S#segment.length,
	Incl = physics:deg_to_rad(S#segment.inclination),
	Amin = physics:acceleration(FDec, Mass, Incl, CarStatus, S#segment.rain),
	Calc = physics:sgm_max_speed(VNext, Amin, Length),
	case lists:keyfind(Sgm, #speed_bound.sgm_id, BoundList) of
		false ->
			NewBound = setelement(AttIndex, #speed_bound{sgm_id = Sgm}, Calc),
			Rec = case Sgm /= LastSgm of
					  true ->
						  preelab_sgm(BoundList, AttIndex, FDec,
									  prev_segment(Sgm, SgmNum), LastSgm,
									  Calc, CarStatus, Mass, SgmNum);
					  false ->
						  % this is the last segment, so there's
						  % no need to do the recursive call
						  BoundList
				  end,
			[NewBound | Rec];
		OldBound ->
			NewBoundList = lists:keydelete(Sgm, #speed_bound.sgm_id, BoundList),
			NewBound = if
						   element(AttIndex, OldBound) == undefined
							   orelse element(AttIndex, OldBound) > Calc ->
							   setelement(AttIndex, OldBound, Calc);
						   true ->
							   OldBound
					   end,
			Rec = case Sgm /= LastSgm of
					  true ->
						  preelab_sgm(NewBoundList, AttIndex, FDec,
									  prev_segment(Sgm, SgmNum), LastSgm,
									  element(AttIndex, NewBound),
									  CarStatus, Mass, SgmNum);
					  false ->
						  % this is the last segment, so there's
						  % no need to do the recursive call
						  NewBoundList
				  end,
			[NewBound | Rec]
	end.

-spec preelab_bent_and_pit(#car_status{}) -> [#speed_bound{}].
preelab_bent_and_pit(CarStatus) ->
	bent_and_pit(CarStatus, utils:get_setting(sgm_number) - 1, {undefined, undefined}).

-spec bent_and_pit(#car_status{}, sgm_id() | -1, {float() | 'undefined', float() | 'undefined'}) ->
			[#speed_bound{}].
bent_and_pit(_CarStatus, -1, _) ->
	[];
bent_and_pit(CarStatus, Sgm, {NextBentB, NextPitB}) ->
	S = utils:mnesia_read(track, Sgm),
	BentBound = case S#segment.curvature /= 0 of
					true ->
						physics:bent_max_speed(CarStatus, S);
					false ->
						undefined
				end,
	T = S#segment.type,
	BentB = erlang:min(BentBound, NextBentB),
	PitB = erlang:min(NextPitB, BentBound),
	if
		T == pitstop;
		T == pitlane ->
			R = #speed_bound{sgm_id = Sgm,
							 pit_bound = erlang:min(?PIT_MAX_SPEED, PitB),
							 bound = BentB},
			[R | bent_and_pit(CarStatus, Sgm - 1, {BentBound, erlang:min(?PIT_MAX_SPEED, BentBound)})];
		true ->
			R = #speed_bound{sgm_id = Sgm,
							 bound = BentB,
							 pit_bound = PitB},
			[R | bent_and_pit(CarStatus, Sgm - 1, {BentBound, BentBound})]
	end.


%% Returns true if next segment's type is 'pre_pitlane', false otherwise.
-spec is_pre_pitlane(sgm_id()) -> boolean().
is_pre_pitlane(Id) when is_integer(Id), Id >= 0 ->
	Sgm = utils:mnesia_read(track, next_segment(Id)),
	Sgm#segment.type == pre_pitlane.


%% Returns the list of lanes a car can reach when driving
%% from the segment PrevSgm to the following one.
-spec reachable_lanes(lane(), sgm_id()) -> [lane()].
reachable_lanes(EnterL, PrevSgm)
  when is_integer(EnterL), is_integer(PrevSgm), PrevSgm >= 0 ->
	Sgm = utils:mnesia_read(track, next_segment(PrevSgm)),
	MinLane = Sgm#segment.min_lane,
	if
		EnterL == -1 ->
			[-2, -1, MinLane];
		EnterL == -2 ->
			[-2, -1];
		EnterL =< MinLane ->
			[-1, EnterL, EnterL + 1];
		true ->
			lists:seq(EnterL - 1, EnterL + 1)
	end.


%% Used by the first invocation of car:move/2 for each car
%% in a race to find out their starting segment and lane.
-spec where_am_i(car()) -> {sgm_id(), lane()}.
where_am_i(CarId) when is_integer(CarId) ->
	Select = fun
				(#segment{queued_cars = []}, Acc) ->
					 Acc;
				(#segment{id = Id, queued_cars = Q}, Acc) ->
					 [{Id, Q} | Acc]
			 end,
	T = fun() ->
				mnesia:foldl(Select, [], track)
		end,
	List = mnesia:activity(sync_transaction, T),
	FindCar = fun({Id, CPs}, Acc) ->
					  CP = lists:keyfind(CarId, #car_position.car_id, CPs),
					  case CP of
						  false ->
							  Acc;
						  _ ->
							  [{Id, CP#car_position.exit_lane} | Acc]
					  end
			  end,
	[Res] = lists:foldl(FindCar, [], List),
	Res.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Returns the id of the segment with the minimum
%% bound value and its associated speed.
-spec min_bound([#speed_bound{}]) -> {sgm_id(), float()}.
min_bound(List) ->
	R = min_bound_rec(List, #speed_bound.bound),
	{R#speed_bound.sgm_id, R#speed_bound.bound}.

%% Returns the id of the segment with the minimum
%% pit_bound value and its associated speed.
-spec min_pit_bound([#speed_bound{}]) -> {sgm_id(), float()}.
min_pit_bound(List) ->
	R = min_bound_rec(List, #speed_bound.pit_bound),
	{R#speed_bound.sgm_id, R#speed_bound.pit_bound}.

%% Returns the speed_bound record that has the
%% minimum value in the Index-th position.
-spec min_bound_rec([#speed_bound{}], pos_integer()) -> #speed_bound{} | 'error'.
min_bound_rec([Head | Tail], Index) ->
	Min = min_bound_rec(Tail, Index),
	if
		Min == error ->
			Head;
		element(Index, Head) < element(Index, Min) ->
			Head;
		true ->
			Min
	end;
min_bound_rec([], _Index) ->
	error.

%% Delete CarPos queued in OldSgm and insert it in NewSgm.
-spec move_car(#segment{}, #segment{}, #car_position{}) -> 'ok'.
move_car(OldSgm, NewSgm, CarPos) ->
	OldQUpdate = lists:keydelete(CarPos#car_position.car_id,
								 #car_position.car_id,
								 OldSgm#segment.queued_cars),
	OldSUpdate = OldSgm#segment{queued_cars = OldQUpdate},
	% FTNOTE: apply a keydelete on NewQ to remove CarPos.car_id
	NewQ = NewSgm#segment.queued_cars,
	
	% check whether any cars have just been surpassed
	Surpass = fun(Elem) ->
					  if
						  Elem#car_position.enter_t < CarPos#car_position.enter_t
							andalso Elem#car_position.exit_t > CarPos#car_position.exit_t ->
							  true;
						  true ->
							  false
					  end
			  end,
	Surpassed = lists:filter(Surpass, NewQ),
	
	NewQUpdate = [CarPos | NewQ],
	NewSUpdate = NewSgm#segment{queued_cars = NewQUpdate},
	% insert the updated segments in track table
	T = fun() ->
				mnesia:write(track, OldSUpdate, write),
				mnesia:write(track, NewSUpdate, write)
		end,
	mnesia:activity(sync_transaction, T),
	
	% send surpass notifications to event_dispatcher
	SendNotif = fun(Elem) ->
						Msg = #surpass_notif{surpasser = CarPos#car_position.car_id,
											 surpassed = Elem#car_position.car_id},
						event_dispatcher:notify(Msg)
				end,
	lists:foreach(SendNotif, lists:reverse(lists:keysort(#car_position.exit_t, Surpassed))).

%% Removes car_position of index PilotId from segment S.
-spec remove_car(#segment{}, car()) -> 'ok'.
remove_car(S, PilotId) ->
	NewQueue = lists:keydelete(PilotId, #car_position.car_id,
							   S#segment.queued_cars),
	NewS = S#segment{queued_cars = NewQueue},
	T = fun() ->
				Running = utils:get_setting(running_cars) - 1,
				mnesia:write(track, NewS, write),
				utils:set_setting(running_cars, Running),
				Running
		end,
	case mnesia:activity(sync_transaction, T) of
		0 -> event_dispatcher:notify(#race_notif{event = finished});
		_ -> ok
	end.

%% Returns car's status after driving Sgm.
-spec update_car_status(#car_status{}, #segment{}) -> #car_status{}.
update_car_status(Status, Sgm) ->
	FCons = ?FUEL_PER_SGM * (1 + math:sin(physics:deg_to_rad(Sgm#segment.inclination))),
	Coeff = case Sgm#segment.curvature /= 0 of
				true -> 1.5;
				false -> 1.0
			end,
	TCons = Coeff * tyres_cons(Status#car_status.tyres_type, Sgm#segment.rain),
	Status#car_status{fuel = Status#car_status.fuel - FCons,
					  tyres_consumption = Status#car_status.tyres_consumption + TCons}.

%% Returns the time needed to perform the pitstop operations.
-spec pitstop_time(#pitstop_ops{}) -> float().
pitstop_time(#pitstop_ops{fuel = F, tyres = T}) ->
	RefuelTime = 2.0 + F / ?REFUEL_SPEED,
	if
		T == null;
		RefuelTime > ?TYRES_CHANGE ->
			RefuelTime;
		true ->
			?TYRES_CHANGE
	end.

%% Returns percentual consumption of tyres.
-spec tyres_cons(tyres(), rain_amount()) -> float().
tyres_cons(slick, Rain) ->
	-0.0001 * Rain + 0.004;
tyres_cons(intermediate, Rain) ->
	-0.0003 * Rain + 0.006;
tyres_cons(wet, Rain) ->
	-0.0005 * Rain + 0.008.

-spec next_segment(sgm_id()) -> sgm_id().
next_segment(Id) ->
	(Id + 1) rem utils:get_setting(sgm_number).

-spec prev_segment(sgm_id(), pos_integer()) -> sgm_id().
prev_segment(0, N) ->
	N - 1;
prev_segment(Id, _N) ->
	Id - 1.

-spec is_chrono_sgm(#segment{}) -> boolean().
is_chrono_sgm(#segment{type = intermediate}) ->
	true;
is_chrono_sgm(#segment{type = finish_line}) ->
	true;
is_chrono_sgm(Sgm) when is_record(Sgm, segment) ->
	false.

-spec is_pit_area_lane(#segment{}, lane()) -> boolean().
is_pit_area_lane(#segment{type = pitlane}, -1) ->
	true;
is_pit_area_lane(#segment{type = pitstop}, Lane) when Lane =< -1 ->
	true;
is_pit_area_lane(#segment{type = pre_pitlane}, -1) ->
	true;
is_pit_area_lane(Sgm, _Lane) when is_record(Sgm, segment) ->
	false.

-spec is_pit_area(#segment{}) -> boolean().
is_pit_area(#segment{type = pitlane}) ->
	true;
is_pit_area(#segment{type = pitstop}) ->
	true;
is_pit_area(#segment{type = pre_pitlane}) ->
	true;
is_pit_area(Sgm) when is_record(Sgm, segment) ->
	false.

%% Takes the id of an intermediate segment as input and returns its
%% index. The finish line is the intermediate with the maximum index.
-spec intermediate_index(sgm_id()) -> intermediate().
intermediate_index(Id) when is_integer(Id), Id >= 0 ->
	Map = utils:get_setting(intermediate_map),
	{Id, Index} = lists:keyfind(Id, 1, Map),
	Index.

%% Returns a list of {SgmId, Index} tuples.
-spec build_intermediate_map([#segment{}]) -> [{sgm_id(), intermediate()}].
build_intermediate_map(List) ->
	build_intermediate_map(lists:keysort(#segment.id, List), 0).

-spec build_intermediate_map([#segment{}], non_neg_integer()) -> [{sgm_id(), intermediate()}].
build_intermediate_map([H | T], 0) ->
	I = case H#segment.type of
			finish_line -> 1;
			_ -> 0
		end,
	build_intermediate_map(T ++ [H], I);
build_intermediate_map([H | T], I) ->
	[{H#segment.id, I} | build_intermediate_map(T, I + 1)];
build_intermediate_map([], _I) ->
	[].
