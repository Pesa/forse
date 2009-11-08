-module(car_sup).

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
	{id, Id} = lists:keyfind(id, 1, Config),
	Car = {utils:build_id_atom("car_", Id),
		   {car, start_link, [Config]},
		   transient, 5000, worker,
		   [car]},
	{ok, {{one_for_one, 20, 30}, [Car]}}.
