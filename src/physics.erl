-module(physics).

-include("config.hrl").

%% Exported Functions
-export([simulate/2,
		 preelaborate/1]).


%%TODO Dove metto l'inclinazione della pista? nel calcolo di Amax Amin o altrove?

%% ====================================================================
%% External functions
%% ====================================================================

%% Calculates the time needed by Car to cover the next segment
%% exiting from it in ExitLane lane.
%% Pilot: id in pilot table
%% ExitLane: guess...

simulate(Pilot, ExitLane) ->
	[P] = mnesia:read(pilot, Pilot),
	Sgm = next_segment(P#pilot.segment),
	[S2] = mnesia:read(?TRACK_TAB, P#pilot.segment),
	Car = find_pilot(Pilot, S2#segment.queued_cars),
	EnterLane = Car#car_position.exit_lane,
	EnterTime = Car#car_position.exit_t,
	
	[S] = mnesia:read(?TRACK_TAB, Sgm),
	Space = S#segment.length,
	EnterSpeed = Car#car_position.speed,
	
	[Bound] = mnesia:read(preelab_tab_name(Pilot), Sgm),
	%% TODO MaxExitSpeed deve fare il minimo con la velocità max dell'auto data da motore
	MaxExitSpeed = Bound#speed_bound.bound,
	%%TODO mettere a posto sti valori
	Amin = 0,
	Amax = 0,
	
	simulate_rec(Sgm, EnterLane, ExitLane, EnterTime, 1,
				 Space, EnterSpeed, MaxExitSpeed, Amin, Amax).

%% Calculates max speed the car with id equal to Pilot can have
%% in each segment of the track.

preelaborate(Pilot) -> 
	[P] = mnesia:read(pilot, Pilot),
	%% [C] = mnesia:read(car_type, P#pilot.team_name), serve???
	
	%% Controlla se esiste la tabella, altrimenti la crea
	case table_exists(preelab_tab_name(Pilot)) of
		false -> 
			%%TODO crea la tabella
			ok
	end,
	
	ok.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Calculates the time needed to cover Space with a starting
%% speed of Speed and an ending speed of MaxSpeed. Amin is the 
%% max deceleration of brakes (always negative) while Amax is 
%% the maximum acceleration engine can supply. 
calculate_time(Space, Speed, MaxSpeed, Amin, Amax) ->
	T1 = 2 * Space / (Speed + MaxSpeed),
	A = (MaxSpeed - Speed) / T1,
	if
		A < Amin -> crash; %% TODO l'auto sbara
		A > Amax -> (math:sqrt(math:pow(Speed, 2) + 8*Amax*Space) - Speed) / (2*Amax);
		true -> T1
	end.

%% returns null or a car_position record
%% Index starts from 1
get_car_ahead(Sgm, Lane, Index) ->
	[R] = mnesia:read(?TRACK_TAB, Sgm),
	Q = R#segment.queued_cars,

	Filter = fun(Pos) ->
					 case Pos of
						 #car_position{exit_lane = Lane} -> true;
						 _ -> false
					 end
			 end,
	Sort = fun(Elem1, Elem2) ->
				   Elem1#car_position.exit_t > Elem2#car_position.exit_t
		   end,
	Slist = lists:sort(Sort, lists:filter(Filter, Q)),

	if
		length(Slist) >= Index -> lists:nth(Index, Slist);
		true -> null
	end.

%%TODO mettere a posto gli args.. sono troppi....
simulate_rec(Sgm, EnterLane, ExitLane, EnterTime, Index, 
			 Space, EnterSpeed, MaxExitSpeed, Amin, Amax) ->
	G = if
			EnterLane == ExitLane -> 0;
			true -> ?LANE_CHANGE_TIME
		end,
	K = get_car_ahead(Sgm, ExitLane, Index),

	%%TODO trattare il caso in cui calculate_time ritorni crash...

	case K of
		null ->
			G + calculate_time(Space, EnterSpeed, MaxExitSpeed, Amin, Amax);
		_ when EnterLane == K#car_position.enter_lane ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			G + calculate_time(Space, EnterSpeed, MaxSpeed, Amin, Amax);
		_ when EnterTime + G > K#car_position.enter_t + ?LANE_CHANGE_TIME ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			G + calculate_time(Space, EnterSpeed, MaxSpeed, Amin, Amax);
		_ ->
			simulate_rec(Sgm, EnterLane, ExitLane, EnterTime, Index + 1,
						 Space, EnterSpeed, MaxExitSpeed, Amin, Amax)
	end.

%% Given a segment's id it calculates next segment's id
next_segment(Id) -> 
	(Id + 1) rem ?GET_SETTING(sgm_number).

%% Extract car_position with car_id == Pilot from the queue
find_pilot(Pilot, [#car_position{car_id = Pilot} = Pos | _]) ->
	Pos;
find_pilot(Pilot, [_ | Tail]) ->
	find_pilot(Pilot, Tail);
find_pilot(_, []) ->
	null.

%% Returns the name of the preelaboration table
%% associated with Pilot
preelab_tab_name(Pilot) ->
	list_to_atom("pilot_" ++ integer_to_list(Pilot)).

%% Pilot: pilot's id
%% Sgm: segment's id
%% Sgm MUST be of type bent
preelaborate_bent(Pilot, Sgm) ->
	G = ?G,
	[S] = mnesia:read(?TRACK_TAB, Sgm),
	R = S#segment.curvature,
	Cos = math:cos(deg_to_rad(S#segment.inclination)),
	
	[P] = mnesia:read(pilot, Pilot),
	K = friction(P#pilot.car_status, S#segment.rain),
	math:sqrt(K*Cos*R*G).

%% Check if a table exits
table_exists(TableName) ->
   Tables = mnesia:system_info(tables),
   lists:member(TableName,Tables).

%% Guess
deg_to_rad(A)-> 
	math:pi()*A/180.0.

%% Calculates coefficient of friction
friction(CarStatus, Rain) ->
	A = consumption(CarStatus#car_status.tyres_consumption),
	B = friction_tab(CarStatus#car_status.tyres_type, Rain),
	A*B*?FRICTION_BASE.

%% TODO
consumption(Val) -> 0.

%% Returns a float between 0 and 1
%% Y = ((y2 - y1)/(x2 - x1))(X - x1) + y1
friction_tab(slick, Rain) -> %% (0,1) (10,0.3)
	-0.07*Rain + 1.0;

friction_tab(intermediate, Rain) -> %% (0,0.9) (10,0.5)
	-0.04*Rain + 0.9;

friction_tab(wet, Rain) -> %% (0,0.7) (10,0.6)
	-0.01*Rain + 0.7.