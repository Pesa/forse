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
	% gen_leaders?
	% init mnesia OK
	% scheduler OK
	% dispatcher_sup OK
	% weather TODO (missing initial config)
	% team_sup OK
	% build track table OK
	Scheduler = [{scheduler,
				  {scheduler, start_link, []},
				  permanent, 5000, worker,
				  [scheduler]}],
	Dispatcher = [{dispatcher,
				   {dispatcher_sup, start_link, []},
				   permanent, infinity, supervisor,
				   [dispatcher_sup]}],
	Weather = [{weather,
				{weather, start_link, []},
				permanent, 5000, worker,
				[weather]}],
	ToChildSpecs = fun(C, Id) ->
						   {{utils:build_id_atom("team_sup_", Id),
							 {team_sup, start_link, [Id, C]},
							 permanent, infinity, supervisor,
							 [team_sup]},
							Id + 1}
				   end,
	TeamsConfig = [],
	{Teams, _} = lists:mapfoldl(ToChildSpecs, 1, TeamsConfig),
	{ok, {{one_for_one, 20, 30},
		  Scheduler ++ Dispatcher
			  ++ Weather ++ Teams}}.
