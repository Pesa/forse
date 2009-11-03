-module(track).

%% Exported Functions
-export([init/3,
		 move/3,
		 simulate/3,
		 preelaborate/1,
		 is_pre_pitlane/1]).

-include("db_schema.hrl").


%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

%% Initializes the track table in mnesia
init(TrackConfig, TeamsNum, CarsList)
  when is_list(TrackConfig), is_integer(TeamsNum), is_list(CarsList) ->
	try
		{Ph0, _} = lists:mapfoldl(fun build_sector/2, {0, 0}, TrackConfig),
		Ph1 = lists:flatten(Ph0),
		{pitlane_entrance, Pit} = lists:keyfind(pitlane_entrance, 1, Ph1),
		Ph2 = lists:keydelete(pitlane_entrance, 1, Ph1),
		utils:set_setting(sgm_number, length(Ph2)),
		Ph3 = build_pit_area(Ph2, Pit, TeamsNum),
		Ph4 = set_chrono_lanes(Ph3),
		SgmList = fill_starting_grid(lists:sort(CarsList), Ph4),
		TabDef = [{record_name, segment},
				  {attributes, record_info(fields, segment)}],
		{atomic, ok} = mnesia:create_table(track, TabDef),
		T = fun() ->
					lists:foreach(fun(Sgm) ->
										  mnesia:write(track, Sgm, sticky_write)
								  end, SgmList)
			end,
		{atomic, ok} = mnesia:sync_transaction(T),
		ok
	catch
		% TODO
		throw : E ->
			{error, E};
		error : {badmatch, _} ->
			{error, something_went_wrong}
	end.

build_sector({straight, Len, MinLane, MaxLane, Incl, Rain}, {Sect, Sgm}) ->
	Temp = #segment{type = normal,
					min_lane = MinLane,
					max_lane = MaxLane,
					length = ?SEGMENT_LENGTH,
					inclination = Incl,
					curvature = 0,
					rain = Rain},
	Last = round(Len / ?SEGMENT_LENGTH) + Sgm,
	utils:set_setting(utils:build_id_atom("sector_", Sect), {Sgm, Last - 1}),
	{sector_to_segments(Temp, Sgm, Last), {Sect + 1, Last}};

build_sector({bent, Len, CurveRadius, MinLane, MaxLane, Incl, Rain}, {Sect, Sgm}) ->
	Temp = #segment{type = normal,
					min_lane = MinLane,
					max_lane = MaxLane,
					length = ?SEGMENT_LENGTH,
					inclination = Incl,
					curvature = CurveRadius,
					rain = Rain},
	Last = round(Len / ?SEGMENT_LENGTH) + Sgm,
	utils:set_setting(utils:build_id_atom("sector_", Sect), {Sgm, Last - 1}),
	{sector_to_segments(Temp, Sgm, Last), {Sect + 1, Last}};

build_sector({finish_line}, {Sect, Sgm}) ->
	S = #segment{id = Sgm,
				   type = finish_line,
				   length = 0},
	{[S], {Sect, Sgm + 1}};

build_sector({intermediate}, {Sect, Sgm}) ->
	S = #segment{id = Sgm,
				   type = intermediate,
				   length = 0},
	{[S], {Sect, Sgm + 1}};

build_sector({pitlane_entrance}, {Sect, Sgm}) ->
	{[{pitlane_entrance, Sgm}], {Sect, Sgm}}.

sector_to_segments(Template, Start, Stop) when Start < Stop ->
	[Template#segment{id = Start} | sector_to_segments(Template, Start + 1, Stop)];
sector_to_segments(_Template, Start, Start) ->
	[].

build_pit_area(List, Index, Teams) ->
	PrePit = 40,
	Pit = 10,
	PostPit = 40,
	{T1, N1} = set_sgm_type(pre_pitlane, Index, PrePit, List),
	{T2, N2} = set_sgm_type(pitlane, N1, Pit, T1),
	{T3, N3} = build_pitstop(N2, Teams, T2),
	{T4, N4} = set_sgm_type(pitlane, N3, Pit, T3),
	{T5, _N5} = set_sgm_type(post_pitlane, N4, PostPit, T4),
	T5.

set_sgm_type(_Type, Start, 0, Sgms) ->
	{Sgms, Start};
set_sgm_type(Type, Start, Num, Sgms) ->
	S = lists:keyfind(Start, #segment.id, Sgms),
	#segment{type = T} = S,
	Next = next_segment(Start),
	if
		T == intermediate;
		T == finish_line ->
			set_sgm_type(Type, Next, Num, Sgms);
		T == normal ->
			Temp = lists:keydelete(Start, #segment.id, Sgms),
			NewSgm = S#segment{type = Type,
							   max_lane = max_lane(Type, S)},
			set_sgm_type(Type, Next, Num - 1, [NewSgm | Temp]);
		true ->
			% FIXME: track is too short, throw an exception
			throw(track_too_short)
	end.

max_lane(Type, #segment{max_lane = L}) ->
	if
		Type == pre_pitlane;
		Type == post_pitlane;
		Type == pitlane ->
			L + 1;
		Type == pitstop ->
			L + 2;
		true ->
			L
	end.

build_pitstop(Start, 0, SgmList) ->
	{SgmList, Start};
build_pitstop(Start, Num, SgmList) ->
	{L1, N1} = set_sgm_type(pitstop, Start, 1, SgmList),
	{L2, N2} = set_sgm_type(pitlane, N1, 1, L1),
	build_pitstop(N2, Num - 1, L2).

set_chrono_lanes(List) ->
	Pred = fun(S) ->
				  T = S#segment.type,
				  T == intermediate orelse T == finish_line
		  end,
	Chrono = lists:filter(Pred, List),
	set_chrono_lanes_rec(Chrono, List).

set_chrono_lanes_rec([], List) ->
	List;
set_chrono_lanes_rec([H | T], List) ->
	Id = H#segment.id,
	Pre = lists:keyfind(prev_segment(Id, utils:get_setting(sgm_number)), #segment.id, List),
	Post = lists:keyfind(next_segment(Id), #segment.id, List),
	Temp = lists:keydelete(Id, #segment.id, List),
	MaxLane = erlang:max(Pre#segment.max_lane, Post#segment.max_lane),
	MinLane = erlang:min(Pre#segment.min_lane, Post#segment.min_lane),
	Sgm = H#segment{min_lane = MinLane,
					max_lane = MaxLane},
	set_chrono_lanes_rec(T, [Sgm | Temp]).

fill_starting_grid(CarsList, SgmList) ->
	Line = (lists:keyfind(finish_line, #segment.type, SgmList))#segment.id,
	SgmNumber = length(SgmList),
	put_cars(CarsList, SgmList, prev_segment(Line, SgmNumber), 1, SgmNumber).

%% LanePos: 1 | 2
put_cars([H | T] = IdList, SgmList, Index, LanePos, SNum) ->
	Sgm = lists:keyfind(Index, #segment.id, SgmList),
	Type = Sgm#segment.type,
	NextLP = case LanePos of
				 1 -> 2;
				 2 -> 1
			 end,
	if
		Type == intermediate;
		Type == finish_lane ->
			put_cars(IdList, SgmList, prev_segment(Index, SNum), LanePos, SNum);
		true ->
			X = if
					Type == normal ->
						0;
					Type == pitstop ->
						2;
					true ->
						1
				end,
			NewSgm = put_one_car(H, Sgm#segment.min_lane, Sgm#segment.max_lane - X, Sgm, LanePos),
			Temp = lists:keyreplace(Index, #segment.id, SgmList, NewSgm),
			put_cars(T, Temp, prev_segment(Index, SNum), NextLP, SNum)
	end;
put_cars([], SgmList, _Index, _LanePos, _SNum) ->
	SgmList.

put_one_car(CarId, MinLane, MaxLane, Sgm, LanePos) ->
	Lanes = MaxLane - MinLane,
	% FIXME: se Lines < 3 deve lanciare un'eccezione
	L = MinLane + LanePos * (Lanes div 3),
	CP = #car_position{car_id = CarId,
					   enter_lane = L,
					   exit_lane = L},
	Sgm#segment{queued_cars = [CP]}.


%% Moves the car to the next segment returning
%% crash | {NextTime, PilotState} | race_ended
%% Pilot: record of type pilot
%% ExitLane: guess...
%% Pit: true if pilot wants to stop at the pits

% FIXME: spostare car_pos in Pilot?
move(Pilot, ExitLane, Pit) when is_record(Pilot, pilot) ->
	Sgm = next_segment(Pilot#pilot.segment),
	SOld = utils:mnesia_read(track, Pilot#pilot.segment),
	% FIXME: check if keyfind returns false
	CarPos = lists:keyfind(Pilot#pilot.id, #pilot.id, SOld#segment.queued_cars),
	EnterLane = Pilot#pilot.lane,
	S = utils:mnesia_read(track, Sgm),
	
	{Time, Speed} = simulate(Pilot, S, EnterLane, ExitLane, Pit, CarPos),
	case Time of
		race_ended ->
			remove_car(SOld, Pilot#pilot.id),
			race_ended;
		crash ->
			event_dispatcher:notify(#retire_notif{car = Pilot#pilot.id}),
			remove_car(SOld, Pilot#pilot.id),
			crash;
		pits ->
			CarStatus = Pilot#pilot.car_status,
			Ops = team:pitstop_operations(Pilot#pilot.team, Pilot#pilot.id, CarStatus,
										  Pilot#pilot.lap, Pilot#pilot.pitstop_count),
			PitstopTime = pitstop_time(Ops),
			
			NewCarPos = CarPos#car_position{speed = 0,
											enter_t = CarPos#car_position.exit_t,
											exit_t = CarPos#car_position.exit_t + PitstopTime,
											enter_lane = EnterLane,
											exit_lane = ExitLane},
			
			% update car_position in track table
			move_car(SOld, S, NewCarPos),
			Fuel = CarStatus#car_status.fuel + Ops#pitstop_ops.fuel,
			NewCarStatus = case Ops#pitstop_ops.tyres == null of
							   true ->
								   CarStatus#car_status{fuel = Fuel};
							   false ->
								   #car_status{fuel = Fuel,
											   tyres_consumption = 0.0,
											   tyres_type = Ops#pitstop_ops.tyres}
						   end,
			NewPilot = Pilot#pilot{segment = Sgm,
								   lane = ExitLane,
								   car_status = NewCarStatus,
								   next_pitstop = -1,
								   pitstop_count = Pilot#pilot.pitstop_count + 1,
								   run_preelab = true},
			
			% finally notify the event_dispatcher
			event_dispatcher:notify(#pitstop_notif{car = Pilot#pilot.id, ops = Ops}),
			{NewCarPos#car_position.exit_t, NewPilot};
		_ ->
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
												   intermediate = S#segment.id,
												   time = NewCarPos#car_position.exit_t,
												   max_speed = MaxSpeed,
												   status = NewCarStatus},
							   event_dispatcher:notify(Msg),
							   Pilot#pilot{segment = Sgm,
										   lane = ExitLane,
										   car_status = NewCarStatus,
										   max_speed = 0};
						   finish_line ->
							   Msg = #chrono_notif{car = Pilot#pilot.id,
												   lap = Pilot#pilot.lap + 1,
												   intermediate = S#segment.id,
												   time = NewCarPos#car_position.exit_t,
												   max_speed = MaxSpeed,
												   status = NewCarStatus},
							   event_dispatcher:notify(Msg),
							   Pilot#pilot{segment = Sgm,
										   lane = ExitLane,
										   car_status = NewCarStatus,
										   max_speed = 0,
										   run_preelab = true};
						   _ ->
							   Pilot#pilot{segment = Sgm,
										   lane = ExitLane,
										   car_status = NewCarStatus}
					   end,
			{NewCarPos#car_position.exit_t, NewPilot}
	end.
	

%% Calculates the time needed by Car to cover the next segment
%% or one of the atoms: crash | pits | race_ended
%% Pilot: record of type pilot
%% ExitLane: guess...
%% Pit: true if pilot wants to stop at the pits

simulate(Pilot, ExitLane, Pit) when is_record(Pilot, pilot) ->
	Sgm = next_segment(Pilot#pilot.segment),
	SOld = utils:mnesia_read(track, Pilot#pilot.segment),
	% FIXME: check if keyfind returns false
	CarPos = lists:keyfind(Pilot#pilot.id, #pilot.id, SOld#segment.queued_cars),
	EnterLane = Pilot#pilot.lane,
	S = utils:mnesia_read(track, Sgm),
	{Time, _Speed} = simulate(Pilot, S, EnterLane, ExitLane, Pit, CarPos),
	Time.

simulate(Pilot, S, EnterLane, ExitLane, Pit, CarPos) when is_record(Pilot, pilot),
														  is_record(S, segment),
														  is_record(CarPos, car_position) ->
	CS = Pilot#pilot.car_status,
	TotalLaps = utils:get_setting(total_laps),
	if
		Pilot#pilot.lap > TotalLaps ->
			{race_ended, 0};
		CS#car_status.tyres_consumption >= 100.0;
		CS#car_status.fuel =< 0.0;
		Pilot#pilot.retire ->
			{crash, 0};
		true ->
			case access:check_move(Pilot, S, EnterLane, ExitLane, Pit) of
				crash -> {crash, 0};
				pits -> {pits, 0};
				go ->
					EnterTime = CarPos#car_position.exit_t,
					Space = S#segment.length,
					EnterSpeed = CarPos#car_position.speed,
					Car = utils:mnesia_read(car_type, Pilot#pilot.team),
					FAcc = Car#car_type.power,
					FDec = Car#car_type.brake,
					Mass = Car#car_type.weight + Pilot#pilot.weight
							+ CS#car_status.fuel * ?FUEL_SPECIFIC_GRAVITY,
					Inc = physics:deg_to_rad(S#segment.inclination),
					Bound = utils:mnesia_read(preelab_tab_name(Pilot#pilot.id), S#segment.id),
					
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
					
					physics:simulate(S, EnterLane, ExitLane, EnterTime, 1,
									 Space, EnterSpeed, MaxExitSpeed, Amin, Amax)
			end
	end.


%% Calculates the maximum speed that Pilot's car
%% can reach in each segment of the track.

preelaborate(Pilot) when is_record(Pilot, pilot) ->
	Car = utils:mnesia_read(car_type, Pilot#pilot.team),
	CarStatus = Pilot#pilot.car_status,
	Mass = Car#car_type.weight + Pilot#pilot.weight
			+ CarStatus#car_status.fuel * ?FUEL_SPECIFIC_GRAVITY,
	FDec = Car#car_type.brake,
	SgmNum = utils:get_setting(sgm_number),
	
	Bounds = preelab_bent_and_pit(Pilot),
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
	
	TabName = preelab_tab_name(Pilot#pilot.id),
	case utils:table_exists(TabName) of
		false ->
			create_pilot_tab(Pilot);
		true ->
			ok
	end,
	T = fun() ->
				lists:foreach(fun(Elem) ->
									  mnesia:write(TabName, Elem, write)
							  end, FinalBounds)
		end,
	{atomic, _} = mnesia:sync_transaction(T).

%% Recursively calculates speed bound for AttIndex
%% BoundList:
%% AttIndex:
%% FDec: power of brakes
%% Sgm: id of the segment that is being computed
%% LastSgm: id of min speed bound segment
%% VNext: speed bound of the next segment
%% SgmNum: total number of segments in the track
preelab_sgm(_BoundList, _AttIndex, _FDec, LastSgm, LastSgm, _VNext, _CarStatus, _Mass, _SgmNum) ->
	[];
preelab_sgm(BoundList, AttIndex, FDec, Sgm, LastSgm, VNext, CarStatus, Mass, SgmNum) ->
	S = utils:mnesia_read(track, Sgm),
	Length = S#segment.length,
	Incl = physics:deg_to_rad(S#segment.inclination),
	Amin = physics:acceleration(FDec, Mass, Incl, CarStatus, S#segment.rain),
	Calc = physics:sgm_max_speed(VNext, Amin, Length),
	case lists:keyfind(Sgm, #speed_bound.sgm_id, BoundList) of
		false ->
			NewBound = setelement(AttIndex, #speed_bound{sgm_id = Sgm}, Calc),
			Rec = preelab_sgm(BoundList, AttIndex, FDec,
							  prev_segment(Sgm, SgmNum), LastSgm,
							  Calc, CarStatus, Mass, SgmNum),
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
			Rec = preelab_sgm(NewBoundList, AttIndex, FDec,
							  prev_segment(Sgm, SgmNum), LastSgm,
							  element(AttIndex, NewBound),
							  CarStatus, Mass, SgmNum),
			[NewBound | Rec]
	end.

%% Returns a list of speed_bound records
preelab_bent_and_pit(Pilot) when is_record(Pilot, pilot) ->
	bent_and_pit(Pilot, utils:get_setting(sgm_number) - 1).

bent_and_pit(_Pilot, -1) ->
	[];
bent_and_pit(Pilot, Sgm) ->
	S = utils:mnesia_read(track, Sgm),
	BentBound = case S#segment.curvature /= 0 of
					true ->
						physics:bent_max_speed(Pilot, S);
					false ->
						undefined
				end,
	T = S#segment.type,
	if
		T == pitstop;
		T == pitlane ->
			R = #speed_bound{sgm_id = Sgm,
							 pit_bound = erlang:min(?PIT_SPEED_LIM, BentBound),
							 bound = BentBound},
			[R | bent_and_pit(Pilot, Sgm - 1)];
		true ->
			R = #speed_bound{sgm_id = Sgm,
							 bound = BentBound,
							 pit_bound = BentBound},
			[R | bent_and_pit(Pilot, Sgm - 1)]
	end.


%% Checks if next segment's type is 'pre_pitlane'

is_pre_pitlane(Id) when is_integer(Id) ->
	Sgm = utils:mnesia_read(track, next_segment(Id)),
	Sgm#segment.type == pre_pitlane.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Creates an empty pilot pre-elaboration tab
create_pilot_tab(Pilot) when is_record(Pilot, pilot) ->
	TabName = preelab_tab_name(Pilot#pilot.id),
	TabDef = [{record_name, speed_bound},
			  {attributes, record_info(fields, speed_bound)}],
	{atomic, ok} = mnesia:create_table(TabName, TabDef).

%% Returns the name of the preelaboration table
%% associated with Pilot
preelab_tab_name(Pilot) ->
	utils:build_id_atom("pilot_", Pilot).

%% Returns the id of the segment which has the minimum bound or 0
min_bound(List) ->
	R = min_bound_rec(List, #speed_bound.bound),
	case R of
		error -> {0, track_error}; % no bents or pits in this track!!
		#speed_bound{sgm_id = Id, bound = B} -> {Id, B}
	end.

%% Returns the id of the segment which has the minimum pit_bound or 0
min_pit_bound(List) ->
	R = min_bound_rec(List, #speed_bound.pit_bound),
	case R of
		error -> {0, track_error}; % no bents or pits in this track!!
		#speed_bound{sgm_id = Id, pit_bound = B} -> {Id, B}
	end.

%% Returns speed_bound record that has minimum element in the Index-th
%% position of the record or error
min_bound_rec([Head | Tail], Index) when is_record(Head, speed_bound) ->
	Min = min_bound_rec(Tail, Index),
	if
		element(Index, Head) == undefined ->
			Min;
		Min == error orelse element(Index, Min) > element(Index, Head) ->
			Head;
		true ->
			Min
	end;
min_bound_rec([], _Index) ->
	error.

%% Delete car_status queued in OldS and insert CS in NewS
%% OldS: Old segment
%% NewS: New segment
%% CS: car status
move_car(OldS, NewS, CS) when is_record(CS, car_position),
							  is_record(OldS, segment),
							  is_record(NewS, segment) ->
	OldQUpdate = lists:keydelete(CS#car_position.car_id,
								 #car_position.car_id,
								 OldS#segment.queued_cars),
	OldSUpdate = OldS#segment{queued_cars = OldQUpdate},
	% FIXME: keydelete su NewQ per togliere CS.car_id
	NewQ = NewS#segment.queued_cars,
	
	% check if CS surpassed some cars in NewQ
	Surpass = fun(Elem) ->
					  if
						  Elem#car_position.enter_t < CS#car_position.enter_t
							andalso Elem#car_position.exit_t > CS#car_position.exit_t ->
							  true;
						  true ->
							  false
					  end
			  end,
	Surpassed = lists:filter(Surpass, NewQ),
	
	NewQUpdate = [CS | NewQ],
	NewSUpdate = NewS#segment{queued_cars = NewQUpdate},
	% insert the updated segments in track table
	T = fun() ->
				mnesia:write(track, OldSUpdate, write),
				mnesia:write(track, NewSUpdate, write)
		end,
	{atomic, _} = mnesia:sync_transaction(T),
	
	% send surpass notification to event_dispatcher
	SendNotif = fun(Elem) ->
						Msg = #surpass_notif{surpasser = CS#car_position.car_id,
											 surpassed = Elem#car_position.car_id},
						event_dispatcher:notify(Msg)
				end,
	lists:foreach(SendNotif, Surpassed).

%% Removes car_position of index PilotId from segment S
remove_car(S, PilotId) when is_record(S, segment) ->
	QueueUpdate = lists:keydelete(PilotId,
								  #car_position.car_id,
								  S#segment.queued_cars),
	SUpdate = S#segment{queued_cars = QueueUpdate},
	T = fun() ->
				mnesia:write(track, SUpdate, write)
		end,
	{atomic, _} = mnesia:sync_transaction(T).

%% Returns car status after driving Sgm
update_car_status(Status, Sgm) when is_record(Status, car_status),
									is_record(Sgm, segment) ->
	FCons = ?L_PER_SGM + ?L_PER_SGM * math:sin(physics:deg_to_rad(Sgm#segment.inclination)),
	BentCoeff = case Sgm#segment.curvature /= 0 of
					true -> 1.5;
					false -> 1
				end,
	TCons = BentCoeff * tyres_cons(Status#car_status.tyres_type, Sgm#segment.rain),
	Status#car_status{fuel = Status#car_status.fuel - FCons,
					  tyres_consumption = Status#car_status.tyres_consumption + TCons}.

%% Returns the time needed to perform Ops during a pitstop
pitstop_time(#pitstop_ops{fuel = F, tyres = T}) ->
	FuelTime = F * ?TIME_PER_L + 2000,
	if
		T == null;
		FuelTime > ?TYRES_CHANGE ->
			FuelTime;
		true ->
			?TYRES_CHANGE
	end.

%% Returns percentual consumption of tyres
tyres_cons(slick, Rain) ->
	-0.0001 * Rain + 0.004;
tyres_cons(intermediate, Rain) ->
	-0.0003 * Rain + 0.006;
tyres_cons(wet, Rain) ->
	-0.0005 * Rain + 0.008.

%% --------------------------------------------------

next_segment(Id) ->
	(Id + 1) rem utils:get_setting(sgm_number).

prev_segment(0, N) ->
	N - 1;
prev_segment(Id, _N) ->
	Id - 1.

is_pit_area_lane(#segment{type = pitlane} = Sgm, Lane) ->
	if
		Sgm#segment.max_lane == Lane -> true;
		true -> false
	end;
is_pit_area_lane(#segment{type = pitstop} = Sgm, Lane) ->
	if
		Sgm#segment.max_lane - 1 =< Lane -> true;
		true -> false
	end;
is_pit_area_lane(#segment{type = pre_pitstop} = Sgm, Lane) ->
	if
		Sgm#segment.max_lane == Lane -> true;
		true -> false
	end;
is_pit_area_lane(Sgm, _Lane) when is_record(Sgm, segment) ->
	false.

is_pit_area(#segment{type = pitlane}) ->
	true;
is_pit_area(#segment{type = pitstop}) ->
	true;
is_pit_area(#segment{type = pre_pitstop}) ->
	true;
is_pit_area(Sgm) when is_record(Sgm, segment) ->
	false.
