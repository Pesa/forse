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

-module(physics).

%% Exported Functions
-export([simulate/11,
		 bent_max_speed/2,
		 sgm_max_speed/3,
		 engine_max_speed/1,
		 acceleration/5,
		 deg_to_rad/1]).

-include("db_schema.hrl").

%% Earth's gravity.
-define(g, 9.80665).

-type calc_result() :: {ok, Time :: float(), Speed :: float()}
					 | {'fail', Reason :: atom()}.


%% ====================================================================
%% External functions
%% ====================================================================

-spec simulate(#segment{}, lane(), lane(), float(), pos_integer(),
			   number(), float(), float(), float(), float(), float()) -> calc_result().
simulate(Sgm, EnterLane, ExitLane, EnterTime, Index, Space,
		 EnterSpeed, MaxExitSpeed, Amin, Amax, SkillC) ->
	G = if
			EnterLane == ExitLane -> 0;
			true -> ?LANE_CHANGE_TIME
		end,
	K = track:get_car_ahead(Sgm, ExitLane, Index),
	GK = if
			 K#car_position.enter_lane == K#car_position.exit_lane -> 0;
			 true -> ?LANE_CHANGE_TIME
		 end,
	case K of
		null ->
			add_g(G, calculate(Space, EnterSpeed, MaxExitSpeed, Amin, Amax, SkillC));
		_ when EnterLane == K#car_position.enter_lane
		  orelse EnterTime + G > K#car_position.enter_t + GK ->
			case add_g(G, calculate(Space, EnterSpeed, MaxExitSpeed, Amin, Amax, SkillC)) of
				{ok, T, _S} when EnterTime + T < K#car_position.exit_t + ?TIME_EPSILON ->
					MinTime = K#car_position.exit_t + ?TIME_EPSILON - EnterTime - G,
					AvgSpeed = Space / MinTime,
					ExitSpeed = EnterSpeed + 2 * (AvgSpeed - EnterSpeed),
					add_g(G, calculate(Space, EnterSpeed, ExitSpeed, Amin, Amax, SkillC));
				Else ->
					Else
			end;
		_ ->
			simulate(Sgm, EnterLane, ExitLane, EnterTime, Index + 1,
					 Space, EnterSpeed, MaxExitSpeed, Amin, Amax, SkillC)
	end.

-spec bent_max_speed(#car_status{}, #segment{}) -> float().
bent_max_speed(CarStatus, #segment{curvature = R} = S) when R /= 0 ->
	Cos = math:cos(deg_to_rad(S#segment.inclination)),
	K = friction(CarStatus, S#segment.rain),
	math:sqrt(K * Cos * R * ?g).

-spec sgm_max_speed(float(), float(), number()) -> float().
sgm_max_speed(VNext, Amin, SgmLength)->
	math:sqrt(math:pow(VNext, 2) + 2 * erlang:abs(Amin) * SgmLength).

%% Maximum speed the car can reach.
-spec engine_max_speed(number()) -> float().
engine_max_speed(F) ->
	0.00085 * F + 74.5.

%% Calculates the car's acceleration.
%% F: force (brake or engine)
%% M: mass
%% Inclination: in rad
-spec acceleration(number(), number(), float(), #car_status{},
				   rain_amount()) -> float().
acceleration(F, M, Inclination, CarStatus, Rain) ->
	K = 1 - (1 - friction_coeff(CarStatus, Rain)) / 2,
	K * (F / M - math:sin(Inclination) * ?g).

-spec deg_to_rad(number()) -> float().
deg_to_rad(Angle) when is_number(Angle) ->
	Angle * math:pi() / 180.0.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Calculates the time needed to cover Space with a starting
%% speed of Speed and an ending speed of MaxSpeed.
%% Amin: maximum deceleration of brakes (always negative)
%% Amax: maximum acceleration the engine can supply
%% SkillCoeff: float from 0.9 to 1.0
-spec calculate(number(), float(), float(), float(), float(), float()) -> calc_result().
calculate(0, Speed, _MaxSpeed, _Amin, _Amax, _SkillCoeff) ->
	{ok, 0.0, Speed};
calculate(_Space, _Speed, _MaxSpeed, _Amin, Amax, _SkillCoeff) when Amax =< 0 ->
	{fail, 'insufficient engine power'};
calculate(Space, Speed, MaxSpeed, Amin, Amax, SkillCoeff) when Amin =< 0 ->
	T1 = 2 * Space / (Speed + MaxSpeed),
	A = (MaxSpeed - Speed) / T1,
	SAmax = SkillCoeff * Amax,
	Result = if
				 A < (?ACCEL_TOLERANCE + SkillCoeff + 0.1) * Amin ->
					 {fail, 'crash'};
				 A > SAmax ->
					 Sqrt = math:sqrt(4 * math:pow(Speed, 2) + 8 * SAmax * Space),
					 Eq = 2 * Space / (Speed + 0.5 * sgn(Speed) * Sqrt),
					 {ok, Eq, SAmax};
				 true ->
					 {ok, T1, A}
			 end,
	case Result of
		{ok, Time, Accel} when Speed + Accel * Time >= 0 ->
			{ok, Time, Speed + Accel * Time};
		{ok, Time, _} ->
			{ok, Time, 0.0};
		Else ->
			Else
	end.

-spec add_g(number(), calc_result()) -> calc_result().
add_g(G, {ok, T, S}) ->
	{ok, G + T, S};
add_g(_G, T) ->
	T.

%% Calculates coefficient of friction
-spec friction(#car_status{}, rain_amount()) -> float().
friction(CarStatus, Rain) ->
	?FRICTION_BASE * friction_coeff(CarStatus, Rain).

%% Calculates a coefficient (between 0 and 1) for the base friction.
-spec friction_coeff(#car_status{}, rain_amount()) -> float().
friction_coeff(CarStatus, Rain) ->
	A = consumption(CarStatus#car_status.tyres_consumption),
	B = friction_tab(CarStatus#car_status.tyres_type, Rain),
	A * B.

%% Returns a float from 1 to 0.5
%% Y = (-3/500)X^2 + (1/10)X + 100
-spec consumption(float()) -> float().
consumption(Val) ->
	Per = (-3.0 / 500.0) * math:pow(Val, 2) + Val / 10.0 + 100.0,
	Per / 100.0.

%% Returns a float between 0 and 1
%% Y = ((y2 - y1)/(x2 - x1))(X - x1) + y1
-spec friction_tab(tyres(), rain_amount()) -> float().
friction_tab(slick, Rain) ->
	% (0,1) (10,0.3)
	-0.07 * Rain + 1.0;
friction_tab(intermediate, Rain) ->
	% (0,0.9) (10,0.5)
	-0.04 * Rain + 0.9;
friction_tab(wet, Rain) ->
	% (0,0.7) (10,0.6)
	-0.01 * Rain + 0.7.

%% Returns the sign of the argument N.
-spec sgn(number()) -> 1 | -1.
sgn(N) when is_number(N) ->
	case N >= 0 of
		true -> 1;
		false -> -1
	end.
