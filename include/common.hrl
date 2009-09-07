%% -----------------------
%%  Common utility macros
%% -----------------------

-define(GLOBAL_NAME, {global, ?MODULE}).
-define(LOCAL_NAME, {local, ?MODULE}).

-define(ERR(Msg), io:format("[~s] ERROR: ~p~n", [?MODULE_STRING, Msg])).
-define(WARN(Msg), io:format("[~s] WARNING: ~p~n", [?MODULE_STRING, Msg])).

-ifdef(debug).
-define(DBG(Msg), io:format("[~s] ~p~n", [?MODULE_STRING, Msg])).
-else.
-define(DBG(Msg), true).
-endif.


%% ------------------------
%%  Global messages format
%% ------------------------

-record(car_status, {fuel, tyres}).
-record(pitstop_ops, {fuel, tyres}).
-record(next_pitstop, {lap, gen_lap}).

-record(chrono_notif, {car,
					   lap,
					   intermediate,
					   time,
					   max_speed,
					   status = #car_status{}}).
-record(pitstop_notif, {car, ops = #pitstop_ops{}}).
-record(surpass_notif, {surpasser, surpassed}).
-record(weather_notif, {new_weather, sector}).
