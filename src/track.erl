-module(track).

%%
%% Include files
%%
-include("config.hrl").

%% Exported Functions
-export([simulate/3,
		 preelaborate/1]).

%%
%% API Functions
%%

%% Calculates the time needed by Car to cover the next segment 
%% or atom 'crash'
%% exiting from it in ExitLane lane.
%% Pilot: record of type pilot
%% ExitLane: guess...
%% Pit: true if pilot wants to stop at the pits

simulate(Pilot, ExitLane, Pit) when is_record(Pilot, pilot)->
	Sgm = next_segment(Pilot#pilot.segment),
	[S2] = mnesia:read(track, Pilot#pilot.segment),
	CarPos = find_pilot(Pilot#pilot.id, S2#segment.queued_cars),
	EnterLane = CarPos#car_position.exit_lane,
	[S] = mnesia:read(track, Sgm),
	
	case access:allow_move(Pilot, S, EnterLane, ExitLane) of
		crash -> crash;
		pits -> ok; %%TODO inserire chiamata ai box
		true ->
			EnterTime = CarPos#car_position.exit_t,
			Space = S#segment.length,
			EnterSpeed = CarPos#car_position.speed,
			
			[Car] = mnesia:read(car_type, Pilot#pilot.team_name),
			FAcc = Car#car_type.power,
			FDec = Car#car_type.brake,
			Mass = Car#car_type.weight + Pilot#pilot.weight + 
					(Pilot#pilot.car_status)#car_status.fuel*?FUEL_SPECIFIC_GRAVITY,
			Inc = physics:deg_to_rad(S#segment.inclination),
			
			[Bound] = mnesia:read(preelab_tab_name(Pilot#pilot.id), Sgm),
			
			%% If in pit area use lane bound otherwise choose using Pit value
			PL = is_pit_area_lane(S, ExitLane),
			SB = case is_pit_area(S) of
					 true when PL -> Bound#speed_bound.pit_bound;
					 true -> Bound#speed_bound.bound;
					 false when Pit -> Bound#speed_bound.pit_bound;
					 false -> Bound#speed_bound.bound
				 end,
			MaxExitSpeed = lists:min([SB, physics:engine_max_speed(Car#car_type.power)]),
			
			Amin = physics:acceleration(FDec, Mass, Inc),
			Amax = physics:acceleration(FAcc, Mass, Inc),
			
			physics:simulate(Sgm, EnterLane, ExitLane, EnterTime, 1,
						 Space, EnterSpeed, MaxExitSpeed, Amin, Amax)
	end.


%% Calculates max speed the car with id equal to Pilot can have
%% in each segment of the track.

preelaborate(Pilot) when is_record(Pilot, pilot) -> 
	%% [C] = mnesia:read(car_type, P#pilot.team_name), serve???
	
	%% Controlla se esiste la tabella, altrimenti la crea
	case table_exists(preelab_tab_name(Pilot#pilot.id)) of
		false -> 
			%%TODO crea la tabella
			ok
	end,
	
	%%TODO preelabora
	%% per ogni segmento:
	%% se è bent usa preelaborate_bent e lo imposta come valore
	%% sia per bound che per pit_bound crea una lista di record
	%% speed_bound
	%% se è pitlane o pitsop imposta #speed_bound.pit_bound a ?PIT_SPEED_LIM
	%% e bound ad undef
	
	Bounds = preelab_bent_and_pit(Pilot),
	
	ok.


%%
%% Local Functions
%%

%% Given a segment's id it calculates next segment's id
next_segment(Id) -> 
	(Id + 1) rem ?GET_SETTING(sgm_number).

%% Given a segment's id it calculates previous segment's id
prev_segment(0) ->
	?GET_SETTING(sgm_number) - 1;
prev_segment(Id) ->
	Id - 1.

%% Returns the name of the preelaboration table
%% associated with Pilot
preelab_tab_name(Pilot) ->
	list_to_atom("pilot_" ++ integer_to_list(Pilot)).

%% Check if a table exits
table_exists(TableName) ->
   Tables = mnesia:system_info(tables),
   lists:member(TableName,Tables).


%% Extract car_position with car_id == Pilot from the queue
find_pilot(Pilot, [#car_position{car_id = Pilot} = Pos | _]) ->
	Pos;
find_pilot(Pilot, [_ | Tail]) ->
	find_pilot(Pilot, Tail);
find_pilot(_, []) ->
	null.

%% Returns a list of speed_bound

preelab_bent_and_pit(Pilot) when is_record(Pilot, pilot) ->
	bent_and_pit_rec(Pilot, ?GET_SETTING(sgm_number) - 1).

%% First time should be invoked with 
%% Sgm == ?GET_SETTING(sgm_number) - 1
bent_and_pit_rec(_Pilot, -1) ->
	[];

bent_and_pit_rec(Pilot, Sgm) ->
	[S] = mnesia:read(track, Sgm),
	case S#segment.type of
		pitstop -> 
			R = #speed_bound{sgm_id = Sgm,
							 bound = undef,
							 pit_bound = ?PIT_SPEED_LIM},
			[R | bent_and_pit_rec(Pilot, Sgm - 1)];
		pitlane ->
			R = #speed_bound{sgm_id = Sgm,
							 bound = undef,
							 pit_bound = ?PIT_SPEED_LIM},
			[R | bent_and_pit_rec(Pilot, Sgm - 1)];
		bent ->
			Bound = physics:bent_max_speed(Pilot, Sgm),
			R = #speed_bound{sgm_id = Sgm,
							 bound = Bound,
							 pit_bound = Bound},
			[R | bent_and_pit_rec(Pilot, Sgm - 1)]
	end.

%%%%%%%%

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

is_pit_area_lane(Sgm, _Lane) when is_record(Sgm, segment) ->
	false.


%% True if segment's type is pitlane | pitstop

is_pit_area(#segment{type = pitlane}) -> 
	true;

is_pit_area(#segment{type = pitstop}) -> 
	true;

is_pit_area(Sgm) when is_record(Sgm, segment) ->
	false.

%% Recursively calculates speed bound for Att
%% Att: bound | pit_bound
%% FDec: power of brakes
%% Sgm: id of the segment that is being computed
%% LastSgm: id of min speed bound segment
%% VNext: speed bound of the next segment
%% preelaborate_sgm_rec(Att, FDec, Sgm, LastSgm, VNext)

%% TODO DA RIFARE A#b.Var non funge
%% preelaborate_sgm_rec(Att, FDec, LastSgm, LastSgm, VNext) ->
%% 	[];
%% 
%% preelaborate_sgm_rec(Att, FDec, Sgm, LastSgm, VNext) ->
%% 	[S] = mnesia:read(track, Sgm),
%% 	Val = asd. %%TODO