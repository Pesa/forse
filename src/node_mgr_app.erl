-module(node_mgr_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	node_mgr_sup:start_link().

stop(_State) ->
	ok.
