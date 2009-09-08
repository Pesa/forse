-module(event_dispatcher).

-behaviour(gen_server).

%% External exports
-export([start_link/0, 
		 subscribe/1,
		 log/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-define(PITSTOP_OBS, [debug_log_backend, team_backend, race_info_backend]).
-define(CHRONO_OBS, [debug_log_backend, team_backend, race_info_backend]).
-define(SURPASS_OBS, [debug_log_backend, team_backend, race_info_backend]).
-define(WEATHER_OBS, [debug_log_backend, weather_backend, team_backend, race_info_backend]).

-record(state, {}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, [], []).

subscribe(Service) ->
	gen_server:call(?GLOBAL_NAME, {subscribe, Service}, infinity).

log(Msg) ->
	gen:server_call(?GLOBAL_NAME, Msg, infinity).


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

% Subscription message

handle_call({subscribe, Service}, From, State) ->
    % aggiunge il parametro From al 
	% backend indicato in Service
	gen_server:cast(service_map(Service), {subscribe, From}),
    {reply, ok, State};

% Race messages

handle_call(Msg, _From, State) when is_record(Msg, chrono_notif)->
	internal_dispatching(Msg, ?CHRONO_OBS),
	{reply, ok, State};

handle_call(Msg, _From, State) when is_record(Msg, pitstop_notif) ->
	internal_dispatching(Msg, ?PITSTOP_OBS),
	{reply, ok, State};

handle_call(Msg, _From, State) when is_record(Msg, surpass_notif)->
	internal_dispatching(Msg, ?SURPASS_OBS),
	{reply, ok, State};

handle_call(Msg, _From, State) when is_record(Msg, weather_notif) ->
	internal_dispatching(Msg, ?WEATHER_OBS),
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
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: service_map/1
%% Purpose: Map from services to dispatcher backends
%% Returns: atom
%% --------------------------------------------------------------------
service_map(Service) ->
	case Service of
		debug_log -> debug_log_backend;
		team -> team_backend;
		race_info -> race_info_backend;
		weather -> weather_backend;
		_ -> not_found
	end.

%% --------------------------------------------------------------------
%% Func: internal_dispatching/2
%% Purpose: Casts Msg to all processes in the list
%% --------------------------------------------------------------------
internal_dispatching(Msg, [Head | Tail]) ->
	gen_server:cast(Head, Msg),
	internal_dispatching(Msg, Tail);
internal_dispatching(_Msg, []) ->
	ok.
