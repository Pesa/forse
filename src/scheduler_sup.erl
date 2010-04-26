-module(scheduler_sup).

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
	{speedup, Speedup} = lists:keyfind(speedup, 1, Config),
	Scheduler = {scheduler,
				 {scheduler, start_link, [Speedup]},
				 permanent, 5000, worker,
				 [scheduler]},
	Helper = {scheduler_helper,
			  {scheduler_helper, start_link, []},
			  permanent, 5000, worker,
			  [scheduler_helper]},
	{ok, {{one_for_one, 20, 30},
		  [Helper, Scheduler]}}.
