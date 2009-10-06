-module(physics).

%% Exported Functions
-export([simulate/10,
		 bent_max_speed/2,
		 sgm_max_speed/3,
		 engine_max_speed/1,
		 acceleration/5,
		 deg_to_rad/1]).

-include("db_schema.hrl").


%% ====================================================================
%% External functions
%% ====================================================================

simulate(Sgm, EnterLane, ExitLane, EnterTime, Index,
		 Space, EnterSpeed, MaxExitSpeed, Amin, Amax) when is_record(Sgm, segment) ->
	G = if
			EnterLane == ExitLane -> 0;
			true -> ?LANE_CHANGE_TIME
		end,
	K = get_car_ahead(Sgm, ExitLane, Index),
	GK = if
			 K#car_position.enter_lane == K#car_position.exit_lane -> 0;
			 true -> ?LANE_CHANGE_TIME
		 end,
	case K of
		null ->
			add_g(G, calculate(Space, EnterSpeed, MaxExitSpeed, Amin, Amax));
		_ when EnterLane == K#car_position.enter_lane ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			add_g(G, calculate(Space, EnterSpeed, MaxSpeed, Amin, Amax));
		_ when EnterTime + G > K#car_position.enter_t + GK ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			add_g(G, calculate(Space, EnterSpeed, MaxSpeed, Amin, Amax));
		_ ->
			simulate(Sgm, EnterLane, ExitLane, EnterTime, Index + 1,
						 Space, EnterSpeed, MaxExitSpeed, Amin, Amax)
	end.

%% Sgm: segment's id
%% Sgm MUST be of type bent
bent_max_speed(Pilot, Sgm) when is_record(Pilot, pilot) ->
	S = utils:mnesia_read(track, Sgm),
	R = S#segment.curvature,
	Cos = math:cos(deg_to_rad(S#segment.inclination)),
	K = friction(Pilot#pilot.car_status, S#segment.rain),
	math:sqrt(K * Cos * R * ?G).

sgm_max_speed(VNext, Amin, S) ->
	math:sqrt(math:pow(VNext, 2) - 2 * Amin * S).

%% Max speed the car can reach.
%% TODO: fix the number
engine_max_speed(_F) -> 42.

%% Calculates the car's acceleration
%% F: force (brake or engine)
%% M: mass
%% Inc: inclination in rad
acceleration(F, M, Inc, CarStatus, Rain) ->
	K = 1 - (1 - friction_coeff(CarStatus, Rain)) / 2,
	K * (F / M - math:sin(Inc) * ?G).

deg_to_rad(A) ->
	math:pi() * A / 180.0.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Calculates the time needed to cover Space with a starting
%% speed of Speed and an ending speed of MaxSpeed. Amin is the
%% max deceleration of brakes (always negative) while Amax is
%% the maximum acceleration engine can supply.
calculate(Space, Speed, MaxSpeed, Amin, Amax) ->
	T1 = 2 * Space / (Speed + MaxSpeed),
	A = (MaxSpeed - Speed) / T1,
	{Time, Acc} = if
					  A < Amin ->
						  {crash, 0};
					  A > Amax ->
						  {(math:sqrt(math:pow(Speed, 2) + 8*Amax*Space) - Speed) / (2*Amax), Amax};
					  true ->
						  {T1, A}
				  end,
	case Time of
		crash -> {crash, 0};
		_ -> {Time, Speed + Acc * Time}
	end.

%% Returns null or a car_position record.
%% Index starts from 1
get_car_ahead(Sgm, Lane, Index) ->
	Q = Sgm#segment.queued_cars,
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

add_g(_, {crash, Speed}) ->
	{crash, Speed};
add_g(G, {T, Speed}) ->
	{G + T, Speed}.

%% Calculates coefficient of friction
friction(CarStatus, Rain) ->
	?FRICTION_BASE * friction_coeff(CarStatus, Rain).

%% Calculates a coefficient for the coefficient of friction
%% float between 0 and 1
friction_coeff(CarStatus, Rain) ->
	A = consumption(CarStatus#car_status.tyres_consumption),
	B = friction_tab(CarStatus#car_status.tyres_type, Rain),
	A * B.

%% Returns a float from 1 to 0.5
%% Y = (-3/500)X^2 + (1/10)X + 100
consumption(Val) ->
	Per = (-3.0 / 500.0) * math:pow(Val, 2) + Val / 10.0 + 100.0,
	Per / 100.0.

%% Returns a float between 0 and 1
%% Y = ((y2 - y1)/(x2 - x1))(X - x1) + y1
friction_tab(slick, Rain) ->
	% (0,1) (10,0.3)
	-0.07 * Rain + 1.0;
friction_tab(intermediate, Rain) ->
	% (0,0.9) (10,0.5)
	-0.04 * Rain + 0.9;
friction_tab(wet, Rain) ->
	% (0,0.7) (10,0.6)
	-0.01 * Rain + 0.7.
