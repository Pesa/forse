%% -----------------------
%%  Configuration parameters macros
%% -----------------------
-include("db_schema.hrl").
%% Time in milliseconds a car needs to move 
%% to the adjacent lane
-define(LANE_CHANGE_TIME, 500).

%% sgm_number: number of segments in track

-define(GET_SETTING(Key),
		begin
			H = utils:mnesia_read(setting, Key),
			H#setting.value
		end
		).

%% Represents the coefficient of friction of new slick tyres 
%% with rain == 0

%% http://www.roymech.co.uk/Useful_Tables/Tribology/co_of_frict.htm#coef TODO
-define(FRICTION_BASE, 10).

-define(G, 9.81).

%% TODO Fix value
-define(FUEL_SPECIFIC_GRAVITY, 0.7).

%% TODO Fix value
-define(PIT_SPEED_LIM, 42).

%% TODO Fix the value
-define(TYRES_CHANGE, 5000).
-define(TIME_PER_L, 83). %% 12 l/s

-define(TANK_DIM, 120).

-define(L_PER_SGM, 0.0042)