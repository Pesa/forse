-include("config.hrl").

%%% =====================================
%%%  Common utility and debugging macros
%%% =====================================

-define(GLOBAL_NAME, {global, ?MODULE}).
-define(LOCAL_NAME, {local, ?MODULE}).

-define(ERR(Msg), io:format("[~s] ERROR: ~p~n", [?MODULE_STRING, Msg])).
-define(WARN(Msg), io:format("[~s] WARNING: ~p~n", [?MODULE_STRING, Msg])).

-ifdef(debug).
-define(DBG(Msg), io:format("[~s] ~p~n", [?MODULE_STRING, Msg])).
-else.
-define(DBG(Msg), true).
-endif.

%%% ==========================================
%%%  Global messages and notifications format
%%% ==========================================

%% ------------------------------------------------------------
%% callback
%% mod: module's name (atom)
%% func: function's name (atom), must belong to mod
%% args: list of arguments to mod:func
%% ------------------------------------------------------------
-record(callback, {mod, func, args}).

%% ------------------------------------------------------------
%% car_status
%% fuel: amount of fuel left
%% tyres_consumption: how much the tyres have been worn out
%%					  (float from 0 to 100)
%% tyres_type: type of car's tyres
%%			   ('slick' | 'intermediate' | 'wet')
%% ------------------------------------------------------------
-record(car_status, {fuel = ?TANK_DIM,
					 tyres_consumption = 0.0,
					 tyres_type = slick}).

%% ------------------------------------------------------------
%% pitstop_ops
%% fuel: amount of fuel added
%% tyres: type of tyres installed
%%		  ('slick' | 'intermediate' | 'wet')
%% ------------------------------------------------------------
-record(pitstop_ops, {fuel, tyres}).

%% ------------------------------------------------------------
%% next_pitstop
%% lap: lap in which the car should stop at the pits
%% stops_count: number of pit stops the car had done when this
%%				message was sent, according to the team
%% ------------------------------------------------------------
-record(next_pitstop, {lap, stops_count}).

%% ------------------------------------------------------------
%% chrono_notif
%% car: ID of the car this notification refers to
%% lap: number of the current (or just ended) lap
%% intermediate: number of the intermediate that has just been completed
%% time: time in seconds spent by the car to go through the intermediate
%% max_speed: maximum speed (in m/s) reached by the car in the intermediate
%% status: car status at the end of the intermediate
%% ------------------------------------------------------------
-record(chrono_notif, {car,
					   lap,
					   intermediate,
					   time,
					   max_speed,
					   status = #car_status{}}).

%% ------------------------------------------------------------
%% pitstop_notif
%% car: ID of the car this notification refers to
%% ops: operations executed on the car
%% ------------------------------------------------------------
-record(pitstop_notif, {car,
						ops = #pitstop_ops{}}).

%% ------------------------------------------------------------
%% surpass_notif
%% surpasser: ID of the surpassing car
%% surpassed: ID of the surpassed car
%% ------------------------------------------------------------
-record(surpass_notif, {surpasser, surpassed}).

%% ------------------------------------------------------------
%% retire_notif
%% car: ID of the retired car
%% ------------------------------------------------------------
-record(retire_notif, {car, reason}).

%% ------------------------------------------------------------
%% weather_notif
%% changes: list of weather_change records containing
%%			information about weather changes
%% ------------------------------------------------------------
-record(weather_notif, {changes = []}).

%% ------------------------------------------------------------
%% weather_change
%% segment: ID of the segment in which the weather changed
%% old_weather: how the weather was like before this change
%% new_weather: how the weather is like after this change
%% ------------------------------------------------------------
-record(weather_change, {segment, old_weather, new_weather}).
