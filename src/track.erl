-module(track).

%% Exported Functions
-export([simulate/3,
		 move/3,
		 preelaborate/1,
		 is_pre_pitlane/1]).

-include("db_schema.hrl").


%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

%% Moves the car to the next segment returning
%% {crash, _} | {NextTime, PilotState} | {race_ended, _}
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
			% FIXME: togliere Pilot
			{race_ended, Pilot};
		crash ->
			event_dispatcher:notify(#retire_notif{car = Pilot#pilot.id}),
			remove_car(SOld, Pilot#pilot.id),
			% FIXME: togliere Pilot
			{crash, Pilot};
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
		CS#car_status.fuel =< 0.0 ->
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
	case S#segment.type of
		pitstop ->
			R = #speed_bound{sgm_id = Sgm,
							 pit_bound = ?PIT_SPEED_LIM},
			[R | bent_and_pit(Pilot, Sgm - 1)];
		pitlane ->
			R = #speed_bound{sgm_id = Sgm,
							 pit_bound = ?PIT_SPEED_LIM},
			[R | bent_and_pit(Pilot, Sgm - 1)];
		bent ->
			Bound = physics:bent_max_speed(Pilot, Sgm),
			R = #speed_bound{sgm_id = Sgm,
							 bound = Bound,
							 pit_bound = Bound},
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
	TabDef = [{attributes, record_info(fields, speed_bound)},
			  {record_name, speed_bound}],
	mnesia:create_table(TabName, TabDef).

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
	BentCoeff = case Sgm#segment.type of
					bent -> 1.5;
					_ -> 1
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
