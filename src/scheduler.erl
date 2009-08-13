-module(scheduler).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 start_simulation/0,
		 pause_simulation/0,
		 queue_work/2,
		 work_done/0,
		 work_done/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-define(GLOBAL_NAME, {global, ?MODULE}).

-record(state, {running,
				slowdown,
				current_timer,
				token_owner,
				workqueue}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, [], []).

start_simulation() ->
	gen_server:call(?GLOBAL_NAME, {start}).

pause_simulation() ->
	gen_server:call(?GLOBAL_NAME, {pause}).

queue_work(Time, {Mod, Fun, Args}) ->
	gen_server:call(?GLOBAL_NAME, {enqueue, Time, {Mod, Fun, Args}}, infinity).

work_done() ->
	gen_sever:call(?GLOBAL_NAME, {done}, infinity).

work_done(Time, {Mod, Fun, Args}) ->
	queue_work(Time, {Mod, Fun, Args}),
	work_done().


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
	% TODO: populate slowdown and workqueue from config file
	%file:consult(...),
	{ok, #state{running = false, workqueue = []}}.

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
handle_call({start}, _From, State) when not State#state.running ->
	NewState = State#state{running = true},
	if
		State#state.token_owner == undefined ->
			% we have the token: process the next item on the workqueue
			{reply, ok, process_next(NewState)};
		true ->
			% someone else has the token: don't do anything
			{reply, ok, NewState}
	end;
handle_call({start}, _From, State) ->
	% the scheduler is already running
	{reply, ok, State};

handle_call({pause}, _From, State) ->
	if State#state.current_timer /= undefined ->
		erlang:cancel_timer(State#state.current_timer)
	end,
	NewState = State#state{running = false, current_timer = undefined},
	{reply, ok, NewState};

handle_call({enqueue, Time, {M, F, A}}, _From, State) ->
	CurrentList = State#state.workqueue,
	NewState = #state{workqueue = insert({Time, M, F, A}, CurrentList)},
	% TODO: refresh timer
	{reply, ok, NewState};

handle_call({done}, _From, State) ->
	{reply, ok, State#state{token_owner = undefined}}.

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
handle_info({timeout, _Timer, wakeup}, State) when State#state.running ->
	{noreply, process_next(State)};
handle_info({timeout, _Timer, wakeup}, State) ->
	{noreply, State#state{current_timer = undefined}}.

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
%%% Internal functions
%% --------------------------------------------------------------------

insert({Time, M, F, A}, List) when is_list(List) ->
	[ X || X <- List, element(1, X) =< Time ]
	++ [{Time, M, F, A}] ++
	[ X || X <- List, element(1, X) > Time ].

process_next(State) when is_record(State, state) ->
	[{Time, M, F, A} | Tail] = State#state.workqueue,
	NewState = State#state{current_timer = setup_timer(Time, Tail),
						   token_owner = {Time, M, F, A},
						   workqueue = Tail},
	% send the token by invoking the provided callback
	apply(M, F, [Time] ++ A),
	NewState.

setup_timer(CurrentTime, [{NextTime, _} | _]) ->
	% TODO: implement slowdown factor
	erlang:start_timer(NextTime - CurrentTime, ?MODULE, wakeup);
setup_timer(_CurrentTime, []) ->
	undefined.
