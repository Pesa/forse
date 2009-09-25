%%% ==========================
%%%  Configuration parameters
%%% ==========================

-include("db_schema.hrl").

-define(GET_SETTING(Key),
		begin
			H = utils:mnesia_read(setting, Key),
			H#setting.value
		end).

% Length of a segment in meters.
-define(SEGMENT_LENGTH, 5).

% Time in milliseconds a car needs to move to the adjacent lane.
-define(LANE_CHANGE_TIME, 500).

% Coefficient of friction of new slick tyres when rain == 0.
% TODO: http://www.roymech.co.uk/Useful_Tables/Tribology/co_of_frict.htm#coef
-define(FRICTION_BASE, 10).

-define(FUEL_SPECIFIC_GRAVITY, 0.7).

-define(G, 9.81).

% TODO: Speed limit in the pitlane.
-define(PIT_SPEED_LIM, 42).

% TODO Fix the value
-define(TYRES_CHANGE, 5000).

-define(TIME_PER_L, 83). % 12 l/s

% Maximum capacity of a car's fuel tank (in liters).
-define(TANK_DIM, 120).

-define(L_PER_SGM, 0.0042).
