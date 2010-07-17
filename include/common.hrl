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

-type car()				:: pos_integer().
-type car_state()		:: 'ready' | 'running' | 'pitstop'
						 | 'ended' | {'retired', Reason :: atom()}.
-type conf()			:: {Key :: atom(), Value :: term()}.
-type conflist()		:: [conf()].
-type intermediate()	:: pos_integer().
-type lane()			:: pos_integer() | -1 | -2.
-type lap()				:: non_neg_integer().
-type pitstop_lap()		:: pos_integer() | 'now' | 'undefined'.
-type race_event()		:: 'started' | 'paused' | 'resumed'
						 | 'finished' | 'terminated'.
-type race_state()		:: 'initialized' | 'running' | 'paused'
						 | 'finished' | 'terminated'.
-type rain_amount()		:: 0..10.
-type sector()			:: {'straight', Length :: pos_integer(),
							MinLane :: lane(), MaxLane :: lane(),
							Inclination :: float(), Rain :: rain_amount()}
						 | {'left' | 'right', Length :: pos_integer(),
							CurveRadius :: float(), MinLane :: lane(),
							MaxLane :: lane(), Inclination :: float(),
							Rain :: rain_amount()}
						 | {'finish_line'} | {'intermediate'}
						 | {'pitlane_entrance'} | {'pitlane_exit'}.
-type sgm_id()			:: non_neg_integer().
-type sgm_type()		:: 'normal' | 'pre_pitlane' | 'post_pitlane'
						 | 'pitlane' | 'pitstop' | 'intermediate'
						 | 'finish_line'.
-type skill()			:: 1..10.
-type start_result()	:: 'ignore' | {'error', Error :: term()}
						 | {'ok', Pid :: pid()}.
-type team()			:: pos_integer().
-type time()			:: number().
-type tyres()			:: 'slick' | 'intermediate' | 'wet'.


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

-type token_reply()	:: 'done'
					 | {'requeue', time(), #callback{}}.

%% ------------------------------------------------------------
%% subscriber
%% cb: callback that must be used to notify the subscriber
%% opts: list of backend-dependent subscription options
%% ------------------------------------------------------------
-record(subscriber, {cb		:: #callback{},
					 opts	:: [atom()]}).

%% ------------------------------------------------------------
%% car_status
%% fuel: amount of fuel left
%% tyres_consumption: how much the tyres have been worn out (from 0.0 to 100.0)
%% tyres_type: type of car's tyres
%% ------------------------------------------------------------
-record(car_status, {fuel				= ?TANK_DIM	:: float(),
					 tyres_consumption	= 0.0		:: float(),
					 tyres_type			= slick		:: tyres()}).

%% ------------------------------------------------------------
%% consumption
%% ------------------------------------------------------------
-record(consumption, {car 				:: car(),
					  intermediate		:: intermediate() | 'start',
					  lap				:: lap() | 'start',
					  fuel				:: float(),
					  tyres_consumption	:: float(),
					  tyres_type		:: tyres()}).

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
-record(next_pitstop, {lap			:: pitstop_lap(),
					   stops_count	:: non_neg_integer()}).

%% ------------------------------------------------------------
%% car_state_notif
%% car: ID of the car this notification refers to
%% state: new state of the car
%% ------------------------------------------------------------
-record(car_state_notif, {car	:: car(),
						  state	:: car_state()}).

%% ------------------------------------------------------------
%% chrono_notif
%% car: ID of the car this notification refers to
%% lap: number of the current (or just ended) lap
%% intermediate: number of the intermediate that has just been completed
%% time: time in seconds spent by the car to go through the intermediate
%% max_speed: maximum speed (in m/s) reached by the car in the intermediate
%% status: car status at the end of the intermediate
%% ------------------------------------------------------------
-record(chrono_notif, {car							:: car(),
					   lap							:: lap(),
					   intermediate					:: intermediate(),
					   time							:: float(),
					   max_speed					:: float(),
					   status		= #car_status{}	:: #car_status{}}).

%% ------------------------------------------------------------
%% config_notif
%% app: name of the application this notification refers to
%% config: application's configuration
%% ------------------------------------------------------------
-record(config_notif, {app		:: atom(),
					   config	:: conflist() | tuple()}).

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
%% weather_change
%% segment: ID of the segment in which the weather changed
%% old_weather: how the weather was like before this change
%% new_weather: how the weather is like after this change
%% ------------------------------------------------------------
-record(weather_change, {segment		:: sgm_id(),
						 old_weather	:: rain_amount(),
						 new_weather	:: rain_amount()}).

%% ------------------------------------------------------------
%% weather_notif
%% changes: information about weather changes
%% ------------------------------------------------------------
-record(weather_notif, {changes	= []	:: [#weather_change{}]}).

-type any_notif()	:: #car_state_notif{} | #chrono_notif{} | #config_notif{}
					 | #pitstop_notif{} | #race_notif{} | #surpass_notif{}
					 | #weather_notif{}.
