-module(bootstrap_srv_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	bootstrap_srv_sup:start_link().

stop(_State) ->
	ok.
