-module(node_manager).

-behaviour(gen_server).

%% External exports
-export([start/0,
		 start_link/0,
		 configure/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-record(state, {started_apps = [] :: [atom()]}).


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

handle_call({load_app, AppSpec, MainNode, FailoverNodes}, _From, State) ->
	App = element(2, AppSpec),
	Dist = {App, [MainNode, list_to_tuple(FailoverNodes)]},
	Reply = application:load(AppSpec, Dist),
	{reply, Reply, State};

handle_call({start_app, App}, _From, State) when is_atom(App) ->
	Reply = application:start(App),
	NewApps = [App | State#state.started_apps],
	{reply, Reply, State#state{started_apps = NewApps}};

handle_call(stop_apps, _From, State) ->
	lists:foreach(fun(App) ->
						  application:stop(App)
				  end, State#state.started_apps),
	{reply, ok, State#state{started_apps = []}};

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
