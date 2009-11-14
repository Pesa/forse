-module(event_dispatcher).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 subscribe/2,
		 notify/1]).

%% Backends exports
-export([notify_init/2,
		 notify_update/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-define(CHRONO_OBS, [debug_log_backend, team_backend, race_info_backend]).
-define(CONFIG_OBS, [team_backend, race_info_backend, weather_backend]).
-define(PITSTOP_OBS, [debug_log_backend, race_info_backend]).
-define(RACE_OBS, [debug_log_backend, race_info_backend, weather_backend]).
-define(RETIRE_OBS, [debug_log_backend, team_backend, race_info_backend]).
-define(SURPASS_OBS, [debug_log_backend, race_info_backend]).
-define(WEATHER_OBS, [debug_log_backend, team_backend, weather_backend]).

-record(state, {}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link() -> start_result().

start_link() ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, [], []).

-spec subscribe(atom(), #callback{}) -> {'error', Error :: atom()} | 'ok'.

subscribe(Service, Callback) when is_record(Callback, callback) ->
	gen_server:call(?GLOBAL_NAME, {subscribe, Service, Callback}, infinity).

-spec notify(any_notif()) -> 'ok'.

notify(Msg) ->
	gen_server:call(?GLOBAL_NAME, Msg, infinity).


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

handle_call({subscribe, Service, Callback}, _From, State) ->
	case service_map(Service) of
		not_found ->
			{reply, {error, service_not_found}, State};
		Backend ->
			gen_server:cast(Backend, {subscribe, Callback}),
			{reply, ok, State}
	end;

handle_call(Msg, _From, State) ->
	if
		is_record(Msg, chrono_notif) ->
			internal_dispatching(Msg, ?CHRONO_OBS);
		is_record(Msg, config_notif) ->
			internal_dispatching(Msg, ?CONFIG_OBS);
		is_record(Msg, pitstop_notif) ->
			internal_dispatching(Msg, ?PITSTOP_OBS);
		is_record(Msg, race_notif) ->
			internal_dispatching(Msg, ?RACE_OBS);
		is_record(Msg, retire_notif) ->
			internal_dispatching(Msg, ?RETIRE_OBS);
		is_record(Msg, surpass_notif) ->
			internal_dispatching(Msg, ?SURPASS_OBS);
		is_record(Msg, weather_notif) ->
			internal_dispatching(Msg, ?WEATHER_OBS);
		true ->
			?WARN({"unhandled call", Msg})
	end,
	{reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
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


%% --------------------------------------------------------------------
%% Functions exported to backends
%% --------------------------------------------------------------------

-spec notify_init(term(), [#callback{}]) -> [#callback{}].

notify_init(InitMsg, Callbacks) ->
	do_notify({init, InitMsg}, Callbacks).

-spec notify_update(term(), [#callback{}]) -> [#callback{}].

notify_update(UpdateMsg, Callbacks) ->
	do_notify({update, UpdateMsg}, Callbacks).


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% Applies each callback in Callbacks list, adding Msg as last argument.
% Returns an updated callback list, without the ones that have failed.
-spec do_notify(term(), [#callback{}]) -> [#callback{}].

do_notify(Msg, Callbacks) when is_list(Callbacks) ->
	Fun = fun(#callback{mod = M, func = F, args = A} = CB, Acc) ->
				  case catch apply(M, F, A ++ [Msg]) of
					  {'EXIT', _} -> Acc;
					  _ -> [CB | Acc]
				  end
		  end,
	lists:reverse(lists:foldl(Fun, [], Callbacks)).

% Casts Msg to each process in the Destinations list.
-spec internal_dispatching(any_notif(), [atom()]) -> 'ok'.

internal_dispatching(Msg, Destinations) when is_list(Destinations) ->
	lists:foreach(fun(D) -> gen_server:cast(D, Msg) end, Destinations).

% Mapping from services to dispatcher backends.
-spec service_map(atom()) -> atom().

service_map(Service) ->
	case Service of
		debug_log -> debug_log_backend;
		team -> team_backend;
		race_info -> race_info_backend;
		weather -> weather_backend;
		_ -> not_found
	end.
