-module(weather_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).


%% ====================================================================
%% External functions
%% ====================================================================

start_link(Config) when is_list(Config) ->
	supervisor:start_link(?MODULE, Config).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(Config) ->
	Weather = {weather,
			   {weather, start_link, [Config]},
			   permanent, 5000, worker,
			   [weather]},
	{ok, {{one_for_one, 20, 30}, [Weather]}}.
