-module(physics).

-include("config.hrl").

%% Exported Functions
-export([simulate/10,
		 bent_max_speed/2,
		 deg_to_rad/1,
		 acceleration/3,
		 engine_max_speed/1]).


%% ====================================================================
%% External functions
%% ====================================================================

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
		A < Amin -> crash;
		A > Amax -> (math:sqrt(math:pow(Speed, 2) + 8*Amax*Space) - Speed) / (2*Amax);
		true -> T1
	end.

%% returns null or a car_position record
%% Index starts from 1
get_car_ahead(Sgm, Lane, Index) ->
	[R] = mnesia:read(track, Sgm),
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


simulate(Sgm, EnterLane, ExitLane, EnterTime, Index, 
			 Space, EnterSpeed, MaxExitSpeed, Amin, Amax) ->
	G = if
			EnterLane == ExitLane -> 0;
			true -> ?LANE_CHANGE_TIME
		end,
	K = get_car_ahead(Sgm, ExitLane, Index),

	case K of
		null ->
			G + calculate_time(Space, EnterSpeed, MaxExitSpeed, Amin, Amax);
		_ when EnterLane == K#car_position.enter_lane ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			add_g(G, calculate_time(Space, EnterSpeed, MaxSpeed, Amin, Amax));
		_ when EnterTime + G > K#car_position.enter_t + ?LANE_CHANGE_TIME ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			add_g(G, calculate_time(Space, EnterSpeed, MaxSpeed, Amin, Amax));
		_ ->
			simulate(Sgm, EnterLane, ExitLane, EnterTime, Index + 1,
						 Space, EnterSpeed, MaxExitSpeed, Amin, Amax)
	end.

%% 
add_g(_, crash) -> 
	crash;
add_g(G, T) ->
	G + T.

%% Pilot: pilot's id
%% Sgm: segment's id
%% Sgm MUST be of type bent
bent_max_speed(Pilot, Sgm) when is_record(Pilot, pilot) ->
	G = ?G,
	[S] = mnesia:read(track, Sgm),
	R = S#segment.curvature,
	Cos = math:cos(deg_to_rad(S#segment.inclination)),
	
	K = friction(Pilot#pilot.car_status, S#segment.rain),
	math:sqrt(K*Cos*R*G).

%% Guess
deg_to_rad(A)-> 
	math:pi()*A/180.0.

%% Calculates coefficient of friction
friction(CarStatus, Rain) ->
	A = consumption(CarStatus#car_status.tyres_consumption),
	B = friction_tab(CarStatus#car_status.tyres_type, Rain),
	A*B*?FRICTION_BASE.

%% Returns a float from 1 to 0.5
%% Y = (-3/500)X^2 + (1/10)X + 100
consumption(Val) ->
	Per = (-3.0/500.0)*math:pow(Val, 2) + Val/10.0 + 100.0,
	Per/100.0.

%% Returns a float between 0 and 1
%% Y = ((y2 - y1)/(x2 - x1))(X - x1) + y1
friction_tab(slick, Rain) -> %% (0,1) (10,0.3)
	-0.07*Rain + 1.0;

friction_tab(intermediate, Rain) -> %% (0,0.9) (10,0.5)
	-0.04*Rain + 0.9;

friction_tab(wet, Rain) -> %% (0,0.7) (10,0.6)
	-0.01*Rain + 0.7.

%% Acceleration
%% F: force (brake or engine)
%% M: mass
%% Inc: inclination in rad
acceleration(F, M, Inc) ->
	F/M - math:sin(Inc)*?G.

%% Max speed the car can reach
%% TODO fix the number
engine_max_speed(F) -> 42.
