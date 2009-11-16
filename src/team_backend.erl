-module(team_backend).

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

-record(state, {observers	= []	:: [#callback{}],
				rain_sum			:: non_neg_integer()}).


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
	NewCB = event_dispatcher:notify_init({rain_sum, State#state.rain_sum}, [Callback]),
	NewObs = State#state.observers ++ NewCB,
	{noreply, State#state{observers = NewObs}};

handle_cast(Msg, State) when is_record(Msg, chrono_notif) ->
	NewObs = event_dispatcher:notify_update(Msg, State#state.observers),
	{noreply, State#state{observers = NewObs}};

handle_cast(#config_notif{app = track, config = Config}, State) ->
	{initial_rain_sum, RainSum} = lists:keyfind(initial_rain_sum, 1, Config),
	NewObs = event_dispatcher:notify_init({rain_sum, RainSum},
										  State#state.observers),
	{noreply, State#state{observers = NewObs,
						  rain_sum = RainSum}};
handle_cast(Msg, State) when is_record(Msg, config_notif) ->
	% ignore config_notif from apps other than track
	{noreply, State};

handle_cast(Msg, State) when is_record(Msg, retire_notif) ->
	% TODO: probabilmente solo le GUI sono interessate ai ritiri
	{noreply, State};

handle_cast(#weather_notif{changes = Changes}, State) ->
	F = fun(#weather_change{old_weather = W1, new_weather = W2}, Sum) ->
				Sum + W2 - W1
		end,
	NewRainSum = lists:foldl(F, State#state.rain_sum, Changes),
	NewObs = event_dispatcher:notify_init({rain_sum, NewRainSum},
										  State#state.observers),
	{noreply, State#state{observers = NewObs,
						  rain_sum = NewRainSum}}.

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
