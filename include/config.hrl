%%% ==========================
%%%  Configuration parameters
%%% ==========================

%% Name of the pre-elaboration table associated with pilot Id.
-define(PREELAB_TABLE(Id), utils:build_id_atom("preelab_", Id)).

%% Length of a segment in meters.
-define(SEGMENT_LENGTH, 5).

%% Time in seconds a car needs to move to the adjacent lane.
-define(LANE_CHANGE_TIME, 0.5).

% Coefficient of friction of new slick tyres when rain == 0.
% TODO: http://www.roymech.co.uk/Useful_Tables/Tribology/co_of_frict.htm#coef
-define(FRICTION_BASE, 10).

-define(FUEL_SPECIFIC_GRAVITY, 0.7).

-define(G, 9.81).

%% Speed limit in the pitlane.
-define(PIT_SPEED_LIM, 42).

%% Time in seconds needed to change tyres
-define(TYRES_CHANGE, 5.0).
%% Time needed to put one liter of fuel in a car
-define(TIME_PER_L, 0.083). % 12 l/s

%% Maximum capacity of a car's fuel tank (in liters).
-define(TANK_DIM, 120.0).

%% Base fuel consumption per segment
-define(L_PER_SGM, 0.0042).
