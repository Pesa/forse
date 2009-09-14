-module(dispatcher_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include("common.hrl").


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	supervisor:start_link(?GLOBAL_NAME, ?MODULE, []).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Dispatcher = {event_dispatcher,
				  {event_dispatcher, start_link, []},
				  transient, 5000, worker,
				  [event_dispatcher]},
	Backends = [
				{debug_log,
				 {debug_log_backend, start_link, []},
				 transient, 5000, worker,
				 [debug_log_backend]},
				{race_info,
				 {race_info_backend, start_link, []},
				 transient, 5000, worker,
				 [race_info_backend]},
				{team,
				 {team_backend, start_link, []},
				 transient, 5000, worker,
				 [team_backend]},
				{weather,
				 {weather_backend, start_link, []},
				 transient, 5000, worker,
				 [weather_backend]}
			   ],
	{ok, {{one_for_one, 20, 30},
		  Backends ++ [Dispatcher]}}.
