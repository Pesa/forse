-module(team_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).


%% ====================================================================
%% External functions
%% ====================================================================

start_link(TeamId, Config) when is_integer(TeamId),
								is_list(Config) ->
	supervisor:start_link(?MODULE, {TeamId, Config}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init({Id, Config}) ->
	Team = {utils:build_id_atom("team_", Id),
			{team, start_link, [[{id, Id} | Config]]},
			permanent, 5000, worker,
			[team]},
	Cars = case lists:keyfind(cars, 1, Config) of
			   {cars, CarsConfig} ->
				   lists:map(fun(C) ->
									 {id, CarId} = lists:keyfind(id, 1, C),
									 {utils:build_id_atom("car_", CarId),
									  {car, start_link, [[{team, Id} | C]]},
									  transient, 5000, worker,
									  [car]}
							 end, CarsConfig);
			   false -> []
		   end,
	{ok, {{one_for_one, 20, 30},
		  [Team | Cars]}}.
