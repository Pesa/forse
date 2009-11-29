-module(race_info_backend).

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

-include("db_schema.hrl").

-record(state, {subscribers	= []	:: [#subscriber{}],
				cars_pos	= []	:: [{car(), non_neg_integer()}],
				race_state			:: race_event(),
				sectors		= []	:: [sector()]}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link() -> start_result().

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
	mnesia:subscribe({table, track, detailed}),
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
handle_cast({subscribe, S}, State) when is_record(S, subscriber) ->
	S1 = event_dispatcher:notify_init({sectors, State#state.sectors}, [S]),
	S2 = event_dispatcher:notify_init({cars_pos, State#state.cars_pos}, S1),
	S3 = event_dispatcher:notify_init({race_state, State#state.race_state}, S2),
	{noreply, State#state{subscribers = S3 ++ State#state.subscribers}};

handle_cast(Msg, State) when is_record(Msg, chrono_notif) ->
	%TODO elaborare i dati ricevuti
	{noreply, State};

handle_cast(#config_notif{app = track, config = Config}, State) ->
	{sectors, Sectors} = lists:keyfind(sectors, 1, Config),
	Subs1 = event_dispatcher:notify_init({sectors, Sectors}, State#state.subscribers),
	{starting_pos, CarsPos} = lists:keyfind(starting_pos, 1, Config),
	Subs2 = event_dispatcher:notify_init({cars_pos, CarsPos}, Subs1),
	{noreply, State#state{subscribers = Subs2,
						  cars_pos = CarsPos,
						  sectors = Sectors}};
handle_cast(Msg, State) when is_record(Msg, config_notif) ->
	{noreply, State};

handle_cast(Msg, State) when is_record(Msg, pitstop_notif) ->
	%TODO elaborare i dati ricevuti
	{noreply, State};

handle_cast(#race_notif{event = Ev}, State) ->
	NewSubs = event_dispatcher:notify_init({race_state, Ev}, State#state.subscribers),
	{noreply, State#state{subscribers = NewSubs,
						  race_state = Ev}};

handle_cast(Msg, State) when is_record(Msg, retire_notif) ->
	%TODO elaborare i dati ricevuti
	{noreply, State};

handle_cast(Msg, State) when is_record(Msg, surpass_notif) ->
	%TODO elaborare i dati ricevuti
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({mnesia_table_event, {write, track, NewSgm, OldSgms, _}}, State)
  when is_record(NewSgm, segment) ->
	OldQueue = case lists:keyfind(NewSgm#segment.id, #segment.id, OldSgms) of
				   #segment{queued_cars = Q} -> Q;
				   false -> []
			   end,
	Diff = NewSgm#segment.queued_cars -- OldQueue,
	CP = State#state.cars_pos,
	Subs = State#state.subscribers,
	{NewCP, NewSubs} = case Diff of
						   [#car_position{car_id = CarId}] ->
							   case lists:keytake(CarId, 1, CP) of
								   {value, {_, Pos}, Rest} ->
									   NewPos = Pos + NewSgm#segment.length,
									   Msg = {car_pos, CarId, NewPos},
									   {[{CarId, NewPos} | Rest],
										event_dispatcher:notify_update(Msg, Subs)};
								   false ->
									   {CP, Subs}
							   end;
						   _ ->
							   {CP, Subs}
					   end,
	{noreply, State#state{cars_pos = NewCP,
						  subscribers = NewSubs}};
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


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
