-module(forse).

-behaviour(application).

-export([start/2, stop/1]).

%% --------------------------------------------------------------------
%% Function: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
	forse_sup:start_link().

%% --------------------------------------------------------------------
%% Function: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
	ok.
