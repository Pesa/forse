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

-define(CAR_NAME(Id), {global, id_to_name(Id)}).

-record(state, {id,
				pilot = #pilot{}}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link(CarId) ->
	gen_server:start_link(?CAR_NAME(CarId), ?MODULE, [CarId], []).

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
init([Id]) ->
	scheduler:queue_work(0, {?MODULE, move, [Id]}),
	{ok, #state{id = Id}}.

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
	% TODO: invocare qualcosa in physics
	NextTime = todo,
	Reply = {requeue, NextTime,
			 {?MODULE, move, [State#state.id]}},
	{reply, Reply, State};

handle_call(#next_pitstop{lap = Lap, gen_lap = GenLap}, _From, State) ->
	% TODO: vedi algoritmo nel quaderno
	{reply, ok, State};

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

id_to_name(Id) ->
	list_to_atom("car_" ++ integer_to_list(Id)).
