-module(scheduler).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 set_speedup/1,
		 start_simulation/0,
		 pause_simulation/0,
		 queue_work/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-define(GLOBAL_NAME, {global, ?MODULE}).

-record(timing, {timer,
				 start,
				 expiry,
				 speedup = 1}).
-record(state, {running = false,
				token_available = true,
				timing_info = #timing{},
				workqueue = []}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, [], []).

set_speedup(NewSpeedup) ->
	gen_server:call(?GLOBAL_NAME, {speedup, NewSpeedup}).

start_simulation() ->
	gen_server:call(?GLOBAL_NAME, {start}).

pause_simulation() ->
	gen_server:call(?GLOBAL_NAME, {pause}).

queue_work(Time, {Mod, Fun, Args}) ->
	gen_server:call(?GLOBAL_NAME, {enqueue, Time, {Mod, Fun, Args}}, infinity).


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
	?DBG("initializing ..."),
	% TODO: populate speedup and workqueue from config file
	% usando file:consult(...)
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
	NewTiming = (State#state.timing_info)#timing{speedup = NewSpeedup},
	{reply, ok, State#state{timing_info = NewTiming}};

handle_call({start}, _From, State) when not State#state.running ->
	?DBG("starting/resuming execution ..."),
	NewState = State#state{running = true},
	{reply, ok, process_next(NewState)};
handle_call({start}, _From, State) ->
	% the scheduler is already running
	{reply, ok, State};

handle_call({pause}, _From, State) ->
	?DBG("pausing execution ..."),
	NewTiming = reset_timing(State#state.timing_info),
	{reply, ok, State#state{running = false,
							timing_info = NewTiming}};

handle_call({enqueue, Time, {M, F, A}}, _From, State) ->
	?DBG({"enqueuing new work", {M, F, A}, "at time", Time}),
	% enqueue the new work
	NewQueue = insert({Time, M, F, A}, State#state.workqueue),
	% update the timer if needed
	NewTiming = recalculate_timer(State#state.timing_info, NewQueue),
	{reply, ok, State#state{timing_info = NewTiming,
							workqueue = NewQueue}};

handle_call({done}, _From, State) ->
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

handle_info(Msg, State) ->
	?WARN({"unhandled info", Msg}),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	?DBG({"shutting down with reason", Reason}),
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

process_next(State) when State#state.running
					andalso State#state.token_available
					andalso (State#state.timing_info)#timing.timer == undefined ->
	% all the preconditions are met: process the next item on the workqueue
	[{Time, M, F, A} | Tail] = State#state.workqueue,
	% start a new timer
	NewTiming = new_timer(State#state.timing_info, State#state.workqueue),
	% send the token by invoking the provided callback in a separate process
	?DBG({"sending token to", {M, F, A}, "at time", Time}),
	spawn_link(?MODULE, give_token, [M, F, [Time] ++ A]),
	State#state{token_available = false,
				timing_info = NewTiming,
				workqueue = Tail};
process_next(State) ->
	?DBG("preconditions not satisfied."),
	State.

give_token(Mod, Fun, Args) ->
	case apply(Mod, Fun, Args) of
		{requeue, Time, {M, F, A}} ->
			queue_work(Time, {M, F, A});
		done ->
			true;
		Else ->
			?WARN({"worker", {Mod, Fun, Args}, "returned unexpected value", Else})
	end,
	work_done().

work_done() ->
	gen_sever:call(?GLOBAL_NAME, {done}, infinity).

new_timer(Timing, [{CurrentTime, _}, {NextTime, _} | _]) ->
	?DBG({"starting timer at", CurrentTime, "expiring at", NextTime}),
	SleepAmount = (NextTime - CurrentTime) div Timing#timing.speedup,
	Timing#timing{timer = start_timer(SleepAmount),
				  start = CurrentTime,
				  expiry = NextTime};
new_timer(Timing, [{CurrentTime, _}]) ->
	Timing#timing{timer = undefined,
				  start = CurrentTime,
				  expiry = undefined}.

recalculate_timer(#timing{timer = undefined, start = Start, speedup = Speedup}, [{NextTime, _} | _]) ->
	new_timer(#timing{speedup = Speedup}, [{Start, unused}, {NextTime, unused}]);
recalculate_timer(#timing{expiry = NextTime} = Timing, [{NextTime, _} | _]) ->
	Timing;
recalculate_timer(#timing{timer = Timer, expiry = Expiry, speedup = Speedup}, [{NextTime, _} | _]) ->
	?DBG({"adjusting timer to expire at", NextTime}),
	RemainingTime = erlang:cancel_timer(Timer),
	SleepAmount = RemainingTime - ((Expiry - NextTime) div Speedup),
	#timing{timer = start_timer(SleepAmount),
			start = Expiry - RemainingTime * Speedup,
			expiry = NextTime,
			speedup = Speedup}.

start_timer(SleepAmount) ->
	erlang:start_timer(erlang:max(0, SleepAmount), ?MODULE, wakeup).

reset_timing(#timing{timer = Timer, speedup = Speedup}) ->
	?DBG("  resetting timing info ..."),
	if Timer /= undefined ->
		erlang:cancel_timer(Timer),
		?DBG("    timer canceled.")
	end,
	#timing{speedup = Speedup}.

insert({Time, M, F, A}, List) when is_list(List) ->
	[ X || X <- List, element(1, X) =< Time ]
	++ [{Time, M, F, A}] ++
	[ X || X <- List, element(1, X) > Time ].
