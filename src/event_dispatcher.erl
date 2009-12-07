-module(event_dispatcher).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 subscribe/2, subscribe/3,
		 notify/1]).

%% Backends exports
-export([add_subscriber/3,
		 notify_init/2, notify_init/3,
		 notify_update/2, notify_update/3]).

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

-spec subscribe(atom(), #callback{}) -> 'ok' | {'error', Error :: atom()}.

subscribe(Service, Callback) when is_record(Callback, callback) ->
	gen_server:call(?GLOBAL_NAME, {subscribe, Service, {Callback, []}}, infinity).

-spec subscribe(atom(), #callback{}, [atom()]) -> 'ok' | {'error', Error :: atom()}.

subscribe(Service, Callback, Options) when is_record(Callback, callback), is_list(Options) ->
	case Options of
		[] ->
			ok;
		_ ->
			gen_server:call(?GLOBAL_NAME, {subscribe, Service, {Callback, Options}}, infinity)
	end.

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
handle_call({subscribe, Service, {CB, Opts}}, _From, State) ->
	case service_map(Service) of
		not_found ->
			{reply, {error, 'service not found'}, State};
		Backend ->
			Subscriber = #subscriber{cb = CB, opts = Opts},
			gen_server:cast(Backend, {subscribe, Subscriber}),
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

-spec add_subscriber(#subscriber{}, [#subscriber{}], conflist()) -> [#subscriber{}].

add_subscriber(S, Subscribers, DataToSync)
  when is_record(S, subscriber), is_list(Subscribers), is_list(DataToSync) ->
	Sync = fun(_Elem, []) ->
				   [];
			  (Elem, Acc) ->
				   notify_init(Elem, Acc)
		   end,
	case lists:foldl(Sync, [S], DataToSync) of
		[] ->
			Subscribers;
		[NewS] ->
			case lists:member(NewS, Subscribers) of
				true -> Subscribers;
				false -> [NewS | Subscribers]
			end
	end.

-spec notify_init(term(), [#subscriber{}]) -> [#subscriber{}].

notify_init(InitMsg, Subscribers) ->
	do_notify({init, InitMsg}, Subscribers).

-spec notify_init(atom(), term(), [#subscriber{}]) -> [#subscriber{}].

notify_init(Type, InitMsg, Subscribers) ->
	do_notify(Type, {init, InitMsg}, Subscribers).

-spec notify_update(term(), [#subscriber{}]) -> [#subscriber{}].

notify_update(UpdateMsg, Subscribers) ->
	do_notify({update, UpdateMsg}, Subscribers).

-spec notify_update(atom(), term(), [#subscriber{}]) -> [#subscriber{}].

notify_update(Type, UpdateMsg, Subscribers) ->
	do_notify(Type, {update, UpdateMsg}, Subscribers).


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec do_notify(term(), [#subscriber{}]) -> [#subscriber{}].

do_notify(Msg, Subscribers) when is_list(Subscribers) ->
	lists:filter(fun(#subscriber{cb = CB}) ->
						 apply_callback(CB, Msg)
				 end, Subscribers).

-spec do_notify(atom(), term(), [#subscriber{}]) -> [#subscriber{}].

do_notify(MsgType, Msg, Subscribers) when is_atom(MsgType),
										  is_list(Subscribers) ->
	F = fun(#subscriber{cb = CB, opts = Opts}) ->
				Member = lists:member(MsgType, Opts),
				if
					Opts == [];
					Member ->
						apply_callback(CB, Msg);
					true ->
						true
				end
		end,
	lists:filter(F, Subscribers).

-spec apply_callback(#callback{}, term()) -> boolean().

apply_callback(#callback{mod = M, func = F, args = A} = CB, Msg) ->
	case catch apply(M, F, A ++ [Msg]) of
		{'EXIT', _} ->
			false;
		{badrpc, _} ->
			false;
		ok ->
			true;
		null ->
			true;
		Else ->
			?WARN({CB, "returned unexpected value", Else}),
			false
	end.

%% Casts Msg to each process in the Destinations list.
-spec internal_dispatching(any_notif(), [atom()]) -> 'ok'.

internal_dispatching(Msg, Destinations) when is_list(Destinations) ->
	lists:foreach(fun(D) -> gen_server:cast(D, Msg) end, Destinations).

%% Mapping from services to dispatcher backends.
-spec service_map(atom()) -> atom().

service_map(Service) ->
	case Service of
		debug_log -> debug_log_backend;
		team -> team_backend;
		race_info -> race_info_backend;
		weather -> weather_backend;
		_ -> not_found
	end.
