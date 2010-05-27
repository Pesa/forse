-module(scheduler_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("common.hrl").

start(_Type, StartArgs) ->
	scheduler_sup:start_link(StartArgs).

stop(_State) ->
	event_dispatcher:notify(#race_notif{event = terminated}).
