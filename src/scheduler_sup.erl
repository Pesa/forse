%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
%%                      Daniele Battaglia <dbat.fk@gmail.com>
%%
%%  This file is part of FORSE.
%%
%%  FORSE is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  FORSE is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
