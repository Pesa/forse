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

-module(node_manager).

-behaviour(gen_server).

%% External exports
-export([start/0,
		 start_link/0,
		 configure/1,
		 start_app/2,
		 stop_app/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-record(state, {}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start() -> 'ok' | {'error', Reason :: term()}.

start() ->
	application:start(?MODULE).

-spec start_link() -> start_result().

start_link() ->
	gen_server:start_link(?LOCAL_NAME, ?MODULE, [], []).

-spec configure(conflist()) -> 'ok' | {'error', Reason :: term()}.

configure(SupportedApps) when is_list(SupportedApps) ->
	gen_server:call(?MODULE, {configure, SupportedApps}, infinity).

-spec start_app(node(), tuple()) -> 'ok' | {'error', Reason :: term()}.

start_app(Node, AppSpec) when is_atom(Node) ->
	gen_server:call({?MODULE, Node}, {start_app, AppSpec}, infinity).

-spec stop_app(node(), atom()) -> 'ok' | {'error', Reason :: term()}.

stop_app(Node, App) when is_atom(Node), is_atom(App) ->
	gen_server:call({?MODULE, Node}, {stop_app, App}, infinity).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({configure, SupportedApps}, _From, State) ->
	LocalHost = list_to_atom(net_adm:localhost()),
	OtherHosts = case net_adm:host_file() of
					 {error, _} -> [];
					 Hosts -> Hosts
				 end,
	net_adm:world_list([LocalHost | OtherHosts]),
	global:sync(),
	Reply = bootstrap_server:add_node(SupportedApps),
	{reply, Reply, State};

handle_call({start_app, AppSpec}, _From, State) ->
	Reply = case application:load(AppSpec) of
				ok ->
					application:start(element(2, AppSpec));
				{error, Reason} ->
					{error, Reason}
			end,
	{reply, Reply, State};

handle_call({stop_app, App}, _From, State) ->
	Reply = case application:stop(App) of
				ok ->
					application:unload(App);
				{error, Reason} ->
					{error, Reason}
			end,
	{reply, Reply, State};

handle_call(Msg, From, State) ->
	?WARN({"unhandled call", Msg, "from", From}),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	?WARN({"unhandled cast", Msg}),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?WARN({"unhandled info", Info}),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%% --------------------------------------------------------------------
%% Function: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
