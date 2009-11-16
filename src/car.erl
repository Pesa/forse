-module(car).

-behaviour(gen_server).

%% External exports
-export([start_link/1,
		 move/2,
		 retire/1,
		 invalidate_preelab/1,
		 set_next_pitstop/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-define(CAR_NAME(Id), {global, utils:build_id_atom("car_", Id)}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link(conflist()) -> start_result().

start_link(Config) when is_list(Config) ->
	{id, CarId} = lists:keyfind(id, 1, Config),
	gen_server:start_link(?CAR_NAME(CarId), ?MODULE, Config, []).

-spec move(time(), car()) -> token_reply().

move(_Time, CarId) ->
	gen_server:call(?CAR_NAME(CarId), move, infinity).

-spec retire(car()) -> 'ok'.

retire(CarId) ->
	gen_server:call(?CAR_NAME(CarId), retire, infinity).

-spec invalidate_preelab(car()) -> 'ok'.

invalidate_preelab(CarId) ->
	gen_server:call(?CAR_NAME(CarId), invalidate_preelab, infinity).

-spec set_next_pitstop(car(), #next_pitstop{}) -> 'ok'.

set_next_pitstop(CarId, PitStop) when is_record(PitStop, next_pitstop) ->
	gen_server:cast(?CAR_NAME(CarId), PitStop).

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
init(Config) ->
	State = lists:foldl(
			  fun({id, Id}, P) ->
					  P#pilot{id = Id};
				 ({name, Name}, P) ->
					  P#pilot{name = Name};
				 ({skill, Skill}, P) ->
					  P#pilot{skill = Skill};
				 ({weight, Weight}, P) ->
					  P#pilot{weight = Weight};
				 ({team, Team}, P) ->
					  P#pilot{team = Team};
				 (_, P) -> P
			  end, #pilot{}, Config),
	% FIXME: remove the following line when switching to ft_gen_server
	{atomic, ok} = mnesia:sync_transaction(fun() -> mnesia:write(State) end),
	scheduler:queue_work(0, #callback{mod = ?MODULE, func = move,
									  args = [State#pilot.id]}),
	{ok, State}.

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
handle_call(move, _From, State) ->
	Id = State#pilot.id,
	State1 = case State#pilot.lane of
				 undefined ->
					 {SgmId, Lane} = track:where_am_i(Id),
					 State#pilot{segment = SgmId, lane = Lane};
				 _ ->
					 State
			 end,
	State2 = if
				 State1#pilot.run_preelab ->
					 track:preelaborate(State1),
					 State1#pilot{run_preelab = false};
				 true ->
					 State1
			 end,
	PitStop = State2#pilot.next_pitstop /= -1 andalso
				State2#pilot.next_pitstop =< State2#pilot.lap,
	
	% simulation phase
	Sim = fun(Lane) ->
				  {Lane, track:simulate(State2, Lane, PitStop)}
		  end,
	EnterLane = State2#pilot.lane,
	SimRes = lists:map(Sim, [EnterLane - 1,
							 EnterLane,
							 EnterLane + 1]),
	
	Pits = lists:keyfind(pits, 2, SimRes),
	End = lists:keyfind(race_ended, 2, SimRes),
	Pred = fun({_, {fail, _Reason}}) ->
				   true;
			  (_) ->
				   false
		   end,
	Crash = lists:all(Pred, SimRes),
	Res = [ X || X <- SimRes, is_number(element(2, X)) ],
	PrePits = track:is_pre_pitlane(State2#pilot.segment),
	if
		Pits /= false ->
			{ExitLane, _} = Pits;
		End /= false ->
			{ExitLane, _} = End;
		Crash ->
			ExitLane = EnterLane;
		PrePits andalso PitStop ->
			{ExitLane, _} = lists:max(Res);
		true ->
			[{ExitLane, _} | _] = lists:keysort(2, Res)
	end,
	
	% actually move the car
	case track:move(State2, ExitLane, PitStop) of
		{NextTime, NewState} ->
			Callback = #callback{mod = ?MODULE, func = move, args = [Id]},
			Reply = {requeue, NextTime, Callback},
			{reply, Reply, NewState};
		_ ->
			% FIXME: remove the following line when switching to ft_gen_server
			{atomic, ok} = mnesia:sync_transaction(fun() -> mnesia:delete({pilot, Id}) end),
			{stop, normal, done, State2}
	end;

handle_call(retire, _From, State) ->
	{reply, ok, State#pilot{retire = true}};

handle_call(invalidate_preelab, _From, State) ->
	{reply, ok, State#pilot{run_preelab = true}};

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
handle_cast(#next_pitstop{lap = NewStop, stops_count = SC}, State) ->
	Lap = State#pilot.lap,
	OldStop = State#pilot.next_pitstop,
	NewState = if
				   State#pilot.pitstop_count /= SC ->
					   % the message is obsolete: ignore it
					   ?DBG("ignoring obsolete next_pitstop message."),
					   State;
				   OldStop == -1;
				   OldStop > Lap;
				   NewStop > Lap ->
					   ?DBG({"setting next_pitstop to", NewStop}),
					   State#pilot{next_pitstop = NewStop};
				   true ->
					   ?DBG("ignoring next_pitstop message."),
					   State
			   end,
	{noreply, NewState};

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
%% Function: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
