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
	%TODO read observers list from mnesia in case of crash
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
							 is_record(Msg, weather_notif) ->
	lists:foreach(
	  fun(#callback{mod = M, func = F, args = A}) ->
			  % TODO: detect dead subscriber
			  apply(M, F, [{update, to_string(Msg)} | A])
	  end,
	  State#state.observers),
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

to_string(#chrono_notif{car = C, lap = Lap, intermediate = Inter, time = T, max_speed = S}) ->
	atom_to_list(C) ++ " has gone through intermediate " ++ integer_to_list(Inter) ++
		" of lap " ++ integer_to_list(Lap) ++ " in " ++ float_to_list(T) ++
		" milliseconds, with a maximum speed of " ++ float_to_list(S) ++ " Km/h";
to_string(#pitstop_notif{car = C, ops = #pitstop_ops{fuel = _Fuel, tyres = _Tyres}}) ->
	atom_to_list(C) ++ " stopped at the pits";
to_string(#surpass_notif{surpasser = Surpasser, surpassed = Surpassed}) ->
	atom_to_list(Surpasser) ++ " surpassed " ++ atom_to_list(Surpassed);
to_string(#weather_notif{new_weather = W, sector = S}) ->
	"Weather changed to " ++ atom_to_list(W) ++ " in sector " ++ integer_to_list(S).
