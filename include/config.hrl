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
			[H] = mnesia:read(setting, Key),
			H#setting.value
		end
		).