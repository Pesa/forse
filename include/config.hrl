%%% ==========================
%%%  Configuration parameters
%%% ==========================

%% Name of the pre-elaboration table associated with pilot Id.
-define(PREELAB_TABLE(Id), utils:build_id_atom("preelab_", Id)).

%% Length of a segment in meters.
-define(SEGMENT_LENGTH, 5).

%% Time in seconds a car needs to move to the adjacent lane.
-define(LANE_CHANGE_TIME, 0.5).

%% Coefficient of friction of new slick tyres when rain == 0.
%% Source: http://www.roymech.co.uk/Useful_Tables/Tribology/co_of_frict.htm#coef
-define(FRICTION_BASE, 0.8).

%% Base fuel consumption per segment (in liters).
-define(FUEL_PER_SGM, 0.0042).

%% Specific gravity of fuel.
-define(FUEL_SPEC_GRAVITY, 0.7).

%% Speed limit within the pitlane (in m/s).
-define(PIT_MAX_SPEED, 22.5).

%% Time in seconds needed to change tyres.
-define(TYRES_CHANGE, 4.5).

%% Refuelling speed (in liters per second).
-define(REFUEL_SPEED, 12.0).

%% Maximum capacity of a car's fuel tank (in liters).
-define(TANK_DIM, 120.0).

%% Acceleration tolerance in percentage.
-define(ACCEL_TOLERANCE, 0.05).
