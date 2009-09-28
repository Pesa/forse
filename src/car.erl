-module(car).

-behaviour(gen_server).

%% External exports
-export([start_link/1,
		 move/2,
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

start_link(Config) when is_list(Config) ->
	{id, CarId} = lists:keyfind(id, 1, Config),
	gen_server:start_link(?CAR_NAME(CarId), ?MODULE, Config, []).

move(_Time, CarId) ->
	gen_server:call(?CAR_NAME(CarId), {move}, infinity).

set_next_pitstop(CarId, PitStop) when is_record(PitStop, next_pitstop) ->
	gen_server:call(?CAR_NAME(CarId), PitStop, infinity).


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
handle_call({move}, _From, State) ->
	PitStop = State#pilot.next_pitstop =< State#pilot.lap,

	Sim = fun(Elem) when is_integer(Elem)->
				  {Elem, track:simulate(State, Elem, PitStop)}
		  end,
	EnterLane = State#pilot.lane,
	SimRes = lists:map(Sim, [EnterLane -1,
							 EnterLane,
							 EnterLane + 1]),
	
	Pits = lists:keyfind(pits, 2, SimRes),
	End = lists:keyfind(race_ended, 2, SimRes),
	Pred = fun
			  ({_, crash}) ->
				   true;
			  (_) ->
				   false
		   end,
	Crash = lists:all(Pred, SimRes),
	Num = fun
				({_, Elem}) when is_number(Elem) ->
					 true;
				(_) ->
					 false
			 end,
	FRes = lists:filter(Num, SimRes),
	PrePits = track:is_pre_pitlane(State#pilot.segment),
	if
		Pits /= false ->
			{ExitLane, _} = Pits;
		End /= false ->
			{ExitLane, _} = End;
		Crash ->
			ExitLane = EnterLane;
		PrePits andalso PitStop ->
			{ExitLane, _} = lists:max(FRes); 
		true ->
			Fun = fun
					 ({_, A}, {_, B}) when is_number(A),
										   is_number(B) ->
						  A < B
				  end,
			[{ExitLane, _} | _] = lists:sort(Fun, FRes)
	end,
	
	{NextTime, NewState} = track:move(State, ExitLane, PitStop),
	if
		is_number(NextTime) ->
			Reply = {requeue, NextTime, #callback{mod = ?MODULE,
												  func = move,
												  args = [State#pilot.id]}},
			{reply, Reply, NewState};
		true ->
			%% TODO race_eneded or crash devo fermare il processo car
			{stop, normal, done, NewState}
	end;

handle_call(#next_pitstop{lap = NewStop, stops_count = SC}, _From, State) ->
	Lap = State#pilot.lap,
	OldStop = State#pilot.next_pitstop,
	NewState = if
				   State#pilot.pitstop_count /= SC ->
					   % the message is obsolete: ignore it
					   State;
				   OldStop == -1;
				   OldStop > Lap;
				   NewStop > Lap ->
					   State#pilot{next_pitstop = NewStop};
				   true ->
					   State
			   end,
	{reply, ok, NewState};

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
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
