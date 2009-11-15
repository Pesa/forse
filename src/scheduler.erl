-module(scheduler).

-behaviour(gen_server).

%% External exports
-export([start_link/1,
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

-type timer_ref() :: any().
-type workqueue() :: [{time(), #callback{}}].

-record(timing, {timer			:: timer_ref(),
				 start	= 0.0	:: time(),
				 expiry			:: time()}).
-record(state, {running			= false		:: boolean(),
				started			= false		:: boolean(),
				token_available	= true		:: boolean(),
				speedup						:: number(),
				timing_info		= #timing{}	:: #timing{},
				workqueue		= []		:: workqueue()}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link(number()) -> start_result().

start_link(Speedup) when is_number(Speedup), Speedup > 0 ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, Speedup, []).

-spec set_speedup(number()) -> 'ok'.

set_speedup(NewSpeedup) when is_number(NewSpeedup), NewSpeedup > 0 ->
	gen_server:call(?GLOBAL_NAME, {speedup, NewSpeedup}).

-spec start_simulation() -> 'ok'.

start_simulation() ->
	gen_server:call(?GLOBAL_NAME, start).

-spec pause_simulation() -> 'ok'.

pause_simulation() ->
	gen_server:call(?GLOBAL_NAME, pause).

-spec queue_work(time(), #callback{}) -> 'ok'.

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
init(Speedup) ->
	% TODO: restart the timer if it's a failover case
	{ok, #state{speedup = Speedup}}.

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

handle_call(start, _From, State) when not State#state.started ->
	?DBG("starting the simulation ..."),
	NewState = State#state{running = true, started = true},
	{reply, ok, process_next(NewState)};
handle_call(start, _From, State) when not State#state.running ->
	?DBG("resuming execution ..."),
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
-spec process_next(#state{}) -> #state{}.

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
-spec give_token(#callback{}) -> 'ok'.

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
-spec new_timer(time(), time(), number()) -> #timing{}.

new_timer(Now, Expiry, Speedup) ->
	?DBG({"starting timer at", Now, "expiring at", Expiry}),
	SleepAmount = (Expiry - Now) / Speedup,
	#timing{timer = start_timer(SleepAmount),
			start = Now,
			expiry = Expiry}.

% Adjusts the expiration time of the currently pending timer, if necessary.
-spec recalculate_timer(#timing{}, workqueue(), number()) -> #timing{}.

recalculate_timer(#timing{timer = undefined, start = Start}, [{NextTime, _} | _], Speedup) ->
	new_timer(Start, NextTime, Speedup);
recalculate_timer(#timing{expiry = NextTime} = Timing, [{NextTime, _} | _], _Speedup) ->
	Timing;
recalculate_timer(#timing{timer = Timer, expiry = Expiry} = Timing, [{NextTime, _} | _], Speedup) ->
	case erlang:cancel_timer(Timer) of
		false ->
			?DBG("not adjusting an already expired timer."),
			Timing;
		RemainingTime ->
			?DBG({"adjusting timer to expire at", NextTime}),
			% RemainingTime is expressed in milliseconds
			SleepAmount = RemainingTime / 1000 - (Expiry - NextTime) / Speedup,
			#timing{timer = start_timer(SleepAmount),
					start = Expiry - RemainingTime / 1000 * Speedup,
					expiry = NextTime}
	end.

% Starts a timer which fires after SleepAmount seconds.
-spec start_timer(time()) -> timer_ref().

start_timer(SleepAmount) ->
	erlang:start_timer(erlang:max(0, round(SleepAmount * 1000)), self(), wakeup).

% Cancels any pending timer.
-spec reset_timing(#timing{}) -> #timing{}.

reset_timing(#timing{timer = Timer, start = Start}) ->
	case Timer of
		undefined ->
			ok;
		_ ->
			case erlang:cancel_timer(Timer) of
				false -> ok;
				_ -> ?DBG("timer canceled.")
			end
	end,
	#timing{start = Start}.

% Inserts {Time, Callback} in the workqueue.
-spec insert({time(), #callback{}}, workqueue()) -> workqueue().

insert({Time, Callback}, List) ->
	[ X || X <- List, element(1, X) =< Time ]
	++ [{Time, Callback}] ++
	[ X || X <- List, element(1, X) > Time ].
