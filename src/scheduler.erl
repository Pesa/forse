%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
%%                      Daniele Battaglia <dbat.fk@gmail.com>
%%
%%  This file is part of FORSE.
%%
%%  FORSE is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  FORSE is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(scheduler).

-behaviour(gen_server).

%% External exports
-export([start_link/1,
		 set_speedup/1,
		 start_simulation/0,
		 pause_simulation/0,
		 queue_work/2]).

%% Private exports
-export([work_done/0]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("common.hrl").

-type workqueue() :: [{time(), #callback{}}].

-record(timing, {timer			:: reference(),
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

start_link(Speedup) when is_integer(Speedup), Speedup > 0 ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, Speedup, []).

-spec set_speedup(number()) -> 'ok'.

set_speedup(NewSpeedup) when is_integer(NewSpeedup), NewSpeedup > 0 ->
	gen_server:call(?GLOBAL_NAME, {speedup, NewSpeedup}).

-spec start_simulation() -> 'ok'.

start_simulation() ->
	gen_server:call(?GLOBAL_NAME, start).

-spec pause_simulation() -> 'ok'.

pause_simulation() ->
	gen_server:call(?GLOBAL_NAME, pause).

-spec queue_work(time(), #callback{}) -> 'ok'.

queue_work(Time, Callback)
  when is_number(Time), Time >= 0, is_record(Callback, callback) ->
	gen_server:call(?GLOBAL_NAME, {enqueue, Time, Callback}, infinity).

-spec work_done() -> 'ok'.

work_done() ->
	gen_server:call(?GLOBAL_NAME, done).


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
	C = [{speedup, Speedup}],
	event_dispatcher:notify(#config_notif{app = ?MODULE, config = C}),
	% FTNOTE: restart the timer if it's a failover case
	{ok, #state{speedup = Speedup / 100}}.

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
	C = [{speedup, NewSpeedup}],
	event_dispatcher:notify(#config_notif{app = ?MODULE, config = C}),
	{reply, ok, State#state{speedup = NewSpeedup / 100}};

handle_call(start, _From, State) when not State#state.started ->
	event_dispatcher:notify(#race_notif{event = started}),
	NewState = State#state{running = true, started = true},
	{reply, ok, process_next(NewState)};
handle_call(start, _From, State) when not State#state.running ->
	event_dispatcher:notify(#race_notif{event = resumed}),
	NewState = State#state{running = true},
	{reply, ok, process_next(NewState)};
handle_call(start, _From, State) ->
	% the scheduler is already running
	{reply, ok, State};

handle_call(pause, _From, State) when State#state.running ->
	event_dispatcher:notify(#race_notif{event = paused}),
	NewTiming = reset_timing(State#state.timing_info),
	{reply, ok, State#state{running = false,
							timing_info = NewTiming}};
handle_call(pause, _From, State) ->
	% the scheduler is already paused
	{reply, ok, State};

handle_call({enqueue, Time, Callback}, _From, State) ->
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
	%?DBG("got the token back."),
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
		[{CurrentJobTime, Callback} | Tail] ->
			% be careful not to go back in time
			Now = erlang:max(CurrentJobTime, Timing#timing.start),
			% check whether a new timer can be started
			NewTiming = case Tail of
							[{NextJobTime, _} | _] ->
								new_timer(Now, NextJobTime, State#state.speedup);
							[] ->
								#timing{start = Now}
						end,
			% send the token by invoking the provided callback in a separate process
			Args = [Now | Callback#callback.args],
			scheduler_helper:give_token(Callback#callback{args = Args}),
			State#state{token_available = false,
						timing_info = NewTiming,
						workqueue = Tail};
		[] ->
			%?DBG("empty workqueue."),
			State
	end;
process_next(State) ->
	%?DBG("preconditions not satisfied."),
	State.

% Adjusts the expiration time of the currently pending timer, if necessary.
-spec recalculate_timer(#timing{}, workqueue(), number()) -> #timing{}.

recalculate_timer(#timing{timer = undefined, start = Start}, [{NextTime, _} | _], Speedup) ->
	new_timer(Start, NextTime, Speedup);
recalculate_timer(#timing{expiry = NextTime} = Timing, [{NextTime, _} | _], _Speedup) ->
	Timing;
recalculate_timer(#timing{timer = Timer} = Timing, [{NextTime, _} | _], Speedup) ->
	case erlang:cancel_timer(Timer) of
		false ->
			%?DBG("not adjusting an already expired timer."),
			Timing;
		MsecsLeft ->
			%?DBG({"adjusting timer to expire at", NextTime}),
			SecsLeft = MsecsLeft / 1000,
			Now = Timing#timing.expiry - SecsLeft * Speedup,
			new_timer(Now, NextTime, Speedup)
	end.

% Starts a new timer to expire at Expiry.
-spec new_timer(time(), time(), number()) -> #timing{}.

new_timer(Now, Expiry, Speedup) ->
	SleepAmount = (Expiry - Now) / Speedup,
	#timing{timer = start_timer(SleepAmount),
			start = Now,
			expiry = erlang:max(Now, Expiry)}.

% Starts a timer which fires after SleepAmount seconds.
-spec start_timer(time()) -> reference().

start_timer(SleepAmount) when is_number(SleepAmount) ->
	erlang:start_timer(erlang:max(0, round(SleepAmount * 1000)), self(), wakeup).

% Cancels any pending timer.
-spec reset_timing(#timing{}) -> #timing{}.

reset_timing(#timing{timer = undefined, start = Start}) ->
	#timing{start = Start};
reset_timing(#timing{timer = Timer, start = Start}) ->
	erlang:cancel_timer(Timer),
	#timing{start = Start}.

% Inserts {Time, Callback} in the workqueue.
-spec insert({time(), #callback{}}, workqueue()) -> workqueue().

insert({Time, Callback}, List) ->
	[ X || X <- List, element(1, X) =< Time ]
	++ [{Time, Callback}] ++
	[ X || X <- List, element(1, X) > Time ].
