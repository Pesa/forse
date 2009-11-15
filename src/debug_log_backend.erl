-module(debug_log_backend).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-record(state, {observers = []}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link() -> start_result().

start_link() ->
	gen_server:start_link(?LOCAL_NAME, ?MODULE, [], []).


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
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({subscribe, Callback}, State) when is_record(Callback, callback) ->
	NewObs = State#state.observers ++ [Callback],
	{noreply, State#state{observers = NewObs}};

handle_cast(Msg, State) when is_record(Msg, chrono_notif);
							 is_record(Msg, pitstop_notif);
							 is_record(Msg, surpass_notif);
							 is_record(Msg, race_notif);
							 is_record(Msg, retire_notif);
							 is_record(Msg, weather_notif) ->
	NewObs = event_dispatcher:notify_update(to_string(Msg), State#state.observers),
	{noreply, State#state{observers = NewObs}}.

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
%% Internal functions
%% --------------------------------------------------------------------

%% Converts a notification record into a human-readable string.

-spec to_string(any_notif()) -> string().

to_string(#chrono_notif{car = C, lap = Lap, intermediate = Inter, time = T, max_speed = S}) ->
	lists:concat(["Car ", C, " has gone through intermediate ", Inter, " of lap ", Lap,
				  " in ", T, " seconds, with a maximum speed of ", S * 3.6, " Km/h."]);
to_string(#pitstop_notif{car = C, ops = #pitstop_ops{fuel = F, tyres = Ty}}) ->
	lists:concat(["Pitstop for car ", C, ": ", F, " liters of fuel have been added and ",
				  Ty, " tyres have been installed."]);
to_string(#surpass_notif{surpasser = Surpasser, surpassed = Surpassed}) ->
	lists:concat(["Car ", Surpasser, " surpassed car ", Surpassed, "."]);
to_string(#race_notif{event = E}) ->
	lists:concat(["Race ", E, "."]);
to_string(#retire_notif{car = C, reason = R}) ->
	lists:concat(["Car ", C, " retired: ", R, "."]);
to_string(#weather_notif{changes = Changes}) ->
	F = fun(#weather_change{segment = S, new_weather = New}, Acc) ->
				lists:concat([Acc, "\t", New, " in segment ", S, "\n"])
		end,
	lists:concat(["\nWeather changed to:\n", lists:foldl(F, "", Changes)]).
