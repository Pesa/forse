-module(scheduler).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 start_simulation/0,
		 pause_simulation/0,
		 queue_work/2,
		 return_token/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-record(state, {running,
				slowdown,
				current_timer,
				token_owner,
				workqueue}).


%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

start_simulation() ->
	gen_server:call(?MODULE, {start}).

pause_simulation() ->
	gen_server:call(?MODULE, {pause}).

queue_work(Time, {Mod, Fun, Args}) ->
	gen_server:call(?MODULE, {queue_work, Time, {Mod, Fun, Args}}).

return_token(NextTime) ->
	gen_sever:call(?MODULE, {return_token, NextTime}).


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
	ok = timer:start(),
	% TODO: populate slowdown and workqueue from config file
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
		timer:cancel(State#state.current_timer)
	end,
	NewState = State#state{running = false, current_timer = undefined},
	{reply, ok, NewState};

handle_call({queue_work, Time, {M, F, A}}, _From, State) ->
	CurrentList = State#state.workqueue,
	NewState = #state{workqueue = insert({Time, M, F, A}, CurrentList)},
	{reply, ok, NewState};

handle_call({return_token, NextTime}, _From, State) ->
	{OldTime, M, F, A} = State#state.token_owner,
	UpdatedQueue = insert({NextTime, M, F, A}, State#state.workqueue),
	NewState = State#state{token_owner = undefined, workqueue = UpdatedQueue},
	if
		State#state.running ->
			[{HeadTime, _} | _] = UpdatedQueue,
			% TODO: implement slowdown factor
			{ok, Timer} = timer:send_after(HeadTime - OldTime, timeout),
			{reply, ok, NewState#state{current_timer = Timer}};
		true ->
			{reply, ok, NewState}
	end.

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
handle_info(timeout, State) when State#state.running ->
	% TODO ???
	{noreply, process_next(State)};
handle_info(timeout, State) ->
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
%%% Internal functions
%% --------------------------------------------------------------------

insert({Time, M, F, A}, List) ->
	[ X || X <- List, element(1, X) =< Time ]
	++ [{Time, M, F, A}] ++
	[ X || X <- List, element(1, X) > Time ].

process_next(State) ->
	[{Time, M, F, A} | Tail] = State#state.workqueue,
	% send the token by invoking the provided callback
	apply(M, F, [Time] ++ A),
	State#state{token_owner = {Time, M, F, A}, workqueue = Tail}.
