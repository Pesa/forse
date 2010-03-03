-module(node_mgr_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	supervisor:start_link(?MODULE, []).


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
	NodeManager = {node_manager,
				   {node_manager, start_link, []},
				   permanent, 5000, worker,
				   [node_manager]},
	{ok, {{one_for_one, 20, 30}, [NodeManager]}}.
