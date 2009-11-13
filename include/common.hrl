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

%%% =============================
%%%  Common types specifications
%%% =============================

-type car() :: pos_integer().
-type conf() :: {Key :: atom(), Value :: term()}.
-type conflist() :: [conf()].
-type race_event() :: 'started' | 'paused' | 'resumed' | 'ended'.
-type rain_amount() :: 0..10.
-type sgm_id() :: non_neg_integer().
-type sgm_type() :: 'normal' | 'pre_pitlane' | 'post_pitlane' | 'pitlane'
					| 'pitstop' | 'intermediate' | 'finish_line'.
-type tyres() :: 'slick' | 'intermediate' | 'wet'.

%%% ==========================================
%%%  Global messages and notifications format
%%% ==========================================

%% ------------------------------------------------------------
%% callback
%% mod: module's name
%% func: function's name, must belong to mod
%% args: list of arguments to mod:func
%% ------------------------------------------------------------
-record(callback, {mod	:: module(),
				   func	:: atom(),
				   args	:: [term()]}).

%% ------------------------------------------------------------
%% car_status
%% fuel: amount of fuel left
%% tyres_consumption: how much the tyres have been worn out
%%					  (float from 0 to 100)
%% tyres_type: type of car's tyres
%% ------------------------------------------------------------
-record(car_status, {fuel				= ?TANK_DIM	:: float(),
					 tyres_consumption	= 0.0		:: float(),
					 tyres_type			= slick		:: tyres()}).

%% ------------------------------------------------------------
%% pitstop_ops
%% fuel: amount of fuel added
%% tyres: type of tyres installed
%% ------------------------------------------------------------
-record(pitstop_ops, {fuel	:: float(),
					  tyres	:: tyres()}).

%% ------------------------------------------------------------
%% next_pitstop
%% lap: lap in which the car should stop at the pits
%% stops_count: number of pit stops the car had done when this
%%				message was sent, according to the team
%% ------------------------------------------------------------
-record(next_pitstop, {lap			:: non_neg_integer(),
					   stops_count	:: non_neg_integer()}).

%% ------------------------------------------------------------
%% chrono_notif
%% car: ID of the car this notification refers to
%% lap: number of the current (or just ended) lap
%% intermediate: number of the intermediate that has just been completed
%% time: time in seconds spent by the car to go through the intermediate
%% max_speed: maximum speed (in m/s) reached by the car in the intermediate
%% status: car status at the end of the intermediate
%% ------------------------------------------------------------
-record(chrono_notif, {car			:: car(),
					   lap			:: non_neg_integer(),
					   intermediate	:: pos_integer(),
					   time			:: float(),
					   max_speed	:: float(),
					   status		= #car_status{}	:: #car_status{}}).

%% ------------------------------------------------------------
%% pitstop_notif
%% car: ID of the car this notification refers to
%% ops: operations executed on the car
%% ------------------------------------------------------------
-record(pitstop_notif, {car						:: car(),
						ops	= #pitstop_ops{}	:: #pitstop_ops{}}).

%% ------------------------------------------------------------
%% race_notif
%% event: what happened
%% ------------------------------------------------------------
-record(race_notif, {event	:: race_event()}).

%% ------------------------------------------------------------
%% surpass_notif
%% surpasser: ID of the surpassing car
%% surpassed: ID of the surpassed car
%% ------------------------------------------------------------
-record(surpass_notif, {surpasser	:: car(),
						surpassed	:: car()}).

%% ------------------------------------------------------------
%% retire_notif
%% car: ID of the retired car
%% reason: reason for retirement
%% ------------------------------------------------------------
-record(retire_notif, {car		:: car(),
					   reason	:: atom()}).

%% ------------------------------------------------------------
%% weather_change
%% segment: ID of the segment in which the weather changed
%% old_weather: how the weather was like before this change
%% new_weather: how the weather is like after this change
%% ------------------------------------------------------------
-record(weather_change, {segment		:: non_neg_integer(),
						 old_weather	:: rain_amount(),
						 new_weather	:: rain_amount()}).

%% ------------------------------------------------------------
%% weather_notif
%% changes: list of weather_change records containing
%%			information about weather changes
%% ------------------------------------------------------------
-record(weather_notif, {changes	= []	:: [#weather_change{}]}).
