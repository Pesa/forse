-module(scheduler).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 set_speedup/1,
		 start_simulation/0,
		 pause_simulation/0,
		 queue_work/2]).

%% Private exports
-export([give_token/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-record(timing, {timer,
				 start = 0,
				 expiry}).
-record(state, {running = false,
				token_available = true,
				speedup = 1,
				timing_info = #timing{},
				workqueue = []}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, [], []).

set_speedup(NewSpeedup) when is_integer(NewSpeedup) ->
	gen_server:call(?GLOBAL_NAME, {speedup, NewSpeedup}).

start_simulation() ->
	gen_server:call(?GLOBAL_NAME, start).

pause_simulation() ->
	gen_server:call(?GLOBAL_NAME, pause).

queue_work(Time, Callback) when is_record(Callback, callback) ->
	gen_server:call(?GLOBAL_NAME, {enqueue, Time, Callback}, infinity).


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
	% TODO: initialize speedup from config argument
	% and restart the timer if it's a failover case.
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
handle_call({speedup, NewSpeedup}, _From, State) ->
	?DBG({"changing speedup factor to", NewSpeedup}),
	{reply, ok, State#state{speedup = NewSpeedup}};

handle_call(start, _From, State) when not State#state.running ->
	?DBG("starting/resuming execution ..."),
	NewState = State#state{running = true},
	{reply, ok, process_next(NewState)};
handle_call(start, _From, State) ->
	% the scheduler is already running
	{reply, ok, State};

handle_call(pause, _From, State) when State#state.running ->
	?DBG("pausing execution ..."),
	NewTiming = reset_timing(State#state.timing_info),
	{reply, ok, State#state{running = false,
							timing_info = NewTiming}};
handle_call(pause, _From, State) ->
	% the scheduler is already paused
	{reply, ok, State};

handle_call({enqueue, Time, Callback}, _From, State) ->
	?DBG({"enqueuing new work", Callback, "at time", Time}),
	% enqueue the new callback
	NewQueue = insert({Time, Callback}, State#state.workqueue),
	% update the timer if needed
	NewTiming = case State#state.running of
					true ->
						recalculate_timer(State#state.timing_info,
										  NewQueue, State#state.speedup);
					false ->
						State#state.timing_info
				end,
	{reply, ok, State#state{timing_info = NewTiming,
							workqueue = NewQueue}};

handle_call(done, _From, State) ->
	?DBG("got the token back."),
	NewState = State#state{token_available = true},
	{reply, ok, process_next(NewState)};

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
handle_info({timeout, _Timer, wakeup}, State) ->
	Timing = State#state.timing_info,
	?DBG({"timer started at", Timing#timing.start, "expired at", Timing#timing.expiry}),
	NewState = State#state{timing_info = reset_timing(Timing)},
	{noreply, process_next(NewState)};

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


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% Processes the next item on the workqueue, if all preconditions are met.
process_next(#state{timing_info = Timing} = State)
  when State#state.running,
	   State#state.token_available,
	   Timing#timing.timer == undefined ->
	case State#state.workqueue of
		[{Time, Callback} | Tail] ->
			Args = [Time | Callback#callback.args],
			% check whether a new timer can be started
			NewTiming = case Tail of
							[{NextTime, _} | _] ->
								new_timer(Time, NextTime, State#state.speedup);
							[] ->
								#timing{start = erlang:max(Time, Timing#timing.start)}
						end,
			% send the token by invoking the provided callback in a separate process
			% FIXME: should probably be converted to a supervised gen_server
			spawn_link(?MODULE, give_token, [Callback#callback{args = Args}]),
			State#state{token_available = false,
						timing_info = NewTiming,
						workqueue = Tail};
		[] ->
			?DBG("empty workqueue."),
			State
	end;
process_next(State) ->
	?DBG("preconditions not satisfied."),
	State.

% Sends the token to the worker identified by the arguments.
give_token(#callback{mod = M, func = F, args = A} = CB) ->
	?DBG({"sending token to", CB}),
	case apply(M, F, A) of
		{requeue, Time, NewCB} when is_number(Time), Time >= 0,
									is_record(NewCB, callback) ->
			queue_work(Time, NewCB);
		done ->
			ok;
		Else ->
			?WARN({CB, "returned unexpected value", Else})
	end,
	% give the token back to the main scheduler process
	gen_server:call(?GLOBAL_NAME, done, infinity).

% Starts a new timer to expire at Expiry.
new_timer(Now, Expiry, Speedup) ->
	?DBG({"starting timer at", Now, "expiring at", Expiry}),
	SleepAmount = (Expiry - Now) / Speedup,
	#timing{timer = start_timer(SleepAmount),
			start = Now,
			expiry = Expiry}.

% Adjusts the expiration time of the currently pending timer, if necessary.
recalculate_timer(#timing{timer = undefined, start = Start}, [{NextTime, _} | _], Speedup) ->
	new_timer(Start, NextTime, Speedup);
recalculate_timer(#timing{expiry = NextTime} = Timing, [{NextTime, _} | _], _Speedup) ->
	Timing;
recalculate_timer(#timing{timer = Timer, expiry = Expiry}, [{NextTime, _} | _], Speedup) ->
	?DBG({"adjusting timer to expire at", NextTime}),
	RemainingTime = erlang:cancel_timer(Timer),
	SleepAmount = RemainingTime - ((Expiry - NextTime) / Speedup),
	#timing{timer = start_timer(SleepAmount),
			start = Expiry - RemainingTime * Speedup,
			expiry = NextTime}.

% Starts a timer which fires after SleepAmount seconds.
start_timer(SleepAmount) ->
	erlang:start_timer(erlang:max(0, round(SleepAmount * 1000)), self(), wakeup).

% Cancels any pending timer.
reset_timing(#timing{timer = Timer, start = Start}) ->
	case Timer of
		undefined ->
			ok;
		_ ->
			erlang:cancel_timer(Timer),
			?DBG("timer canceled.")
	end,
	#timing{start = Start}.

% Inserts {Time, Callback} in the workqueue.
insert({Time, Callback}, List) ->
	[ X || X <- List, element(1, X) =< Time ]
	++ [{Time, Callback}] ++
	[ X || X <- List, element(1, X) > Time ].
