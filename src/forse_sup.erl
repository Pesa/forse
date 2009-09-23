-module(forse_sup).

%%% ====================================================================
%%%  The top-level FORSE supervisor
%%% ====================================================================

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
	% TODO: implement the complete startup sequence
	% (gen_leaders?)
	% init mnesia
	% build track table
	% scheduler
	% dispatcher_sup
	% weather
	% teams and cars (how?)
	Children = [
				{scheduler,
				 {scheduler, start_link, []},
				 permanent, 5000, worker,
				 [scheduler]},
				{dispatcher,
				 {dispatcher_sup, start_link, []},
				 permanent, 5000, supervisor,
				 [dispatcher_sup]},
				{weather,
				 {weather, start_link, []},
				 permanent, 5000, worker,
				 [weather]}
			   ],
	{ok, {{one_for_one, 20, 30},
		  Children}}.


%% ====================================================================
%% Internal functions
%% ====================================================================
