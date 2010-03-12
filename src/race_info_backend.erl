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
				cars_pos	= []	:: [{Id :: car(), Pos :: non_neg_integer(), Pit :: boolean()}],
				race_state			:: race_event(),
				sectors		= []	:: [sector()],
				pilots = []			:: [{Id :: car(), TeamID :: team(), Name :: string(), TeamName :: string() | 'undefined'}],
				teams = []			:: [{Id :: team(), Name :: string()}],
				standings = []		:: [{Id :: car(), Pos :: non_neg_integer(), Status :: 'running' | 'retired'}]}).


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
	List = [{sectors, State#state.sectors},
			{cars_pos, State#state.cars_pos},
			{race_state, State#state.race_state},
			{standings, State#state.standings},
			{names, lists:map(name, State#state.pilots)}],
	NewSubs = event_dispatcher:add_subscriber(S, State#state.subscribers, List),
	{noreply, State#state{subscribers = NewSubs}};

handle_cast(Msg, State) when is_record(Msg, chrono_notif) ->
	%TODO elaborare i dati ricevuti
	{noreply, State};

handle_cast(#config_notif{app = track, config = Config}, State) ->
	{sectors, Sectors} = lists:keyfind(sectors, 1, Config),
	Subs1 = event_dispatcher:notify_init({sectors, Sectors}, State#state.subscribers),
	{starting_pos, StartPos} = lists:keyfind(starting_pos, 1, Config),
	CarsPos = lists:map(fun({CarId, Pos}) ->
								{CarId, Pos, false}
						end, lists:reverse(lists:keysort(2, StartPos))),
	Subs2 = event_dispatcher:notify_init({cars_pos, CarsPos}, Subs1),
	% use car_pos to build standings and notify
	{Standings, _} = lists:mapfoldl(fun({CarId, _, _}, Acc) ->
											{{CarId, Acc, running}, Acc + 1}
									end, 1, CarsPos),
	
	Subs3 = event_dispatcher:notify_init({standings, Standings}, Subs2),
	{noreply, State#state{subscribers = Subs3,
						  cars_pos = CarsPos,
						  sectors = Sectors,
						  standings = Standings}};

handle_cast(#config_notif{app = car, config = Pilot}, State) ->
	TeamName = case lists:keyfind(Pilot#pilot.team, 1, State#state.teams) of
				   false ->
					   undefined;
				   {_Id, Name} ->
					   Name
			   end,
	NewPilot = {Pilot#pilot.id, Pilot#pilot.team, Pilot#pilot.name, TeamName},
	Subs = case TeamName of
			   undefined ->
				   State#state.subscribers;
			   _ ->
				   event_dispatcher:notify_update({names, [name(NewPilot)]}, State#state.subscribers)
		   end,
	{noreply, State#state{pilots = [NewPilot | State#state.pilots], subscribers = Subs}};

handle_cast(#config_notif{app = team, config = CarType}, State) ->
	SetTeam = fun({Id, T, N, undefined}, Acc) when T == CarType#car_type.id ->
					  Pilot = {Id, T, N, CarType#car_type.team_name},
					  S = event_dispatcher:notify_update({names, [name(Pilot)]}, Acc),
					  {Pilot, S};
				 (X, Acc) ->
					  {X, Acc}
			  end,
	{Pilots, Subs} = lists:mapfoldl(SetTeam, State#state.subscribers, State#state.pilots),
	Teams = [{CarType#car_type.id, CarType#car_type.team_name} | State#state.teams],
	{noreply, State#state{pilots = Pilots, teams = Teams, subscribers = Subs}};

handle_cast(Msg, State) when is_record(Msg, config_notif) ->
	{noreply, State};

handle_cast(Msg, State) when is_record(Msg, pitstop_notif) ->
	% TODO: serve questa notifica? probabilmente no...
	{noreply, State};

handle_cast(#race_notif{event = Ev}, State) ->
	NewSubs = event_dispatcher:notify_init({race_state, Ev}, State#state.subscribers),
	{noreply, State#state{subscribers = NewSubs,
						  race_state = Ev}};

handle_cast(Msg, State) when is_record(Msg, retire_notif) ->
	%TODO elaborare i dati ricevuti
	{_, RetPos, running} = lists:keyfind(Msg#retire_notif.car, 1, State#state.standings),
	Sort = lists:keysort(2, State#state.standings),
	{LRun, LRet} = lists:splitwith(fun({_, _, running}) -> true;
									  (_) -> false
								   end,
								   Sort),
	F = fun({_Id, Pos, running} = T, Acc) when Pos < RetPos ->
				{T, Acc};
		   ({Id, Pos, running}, Acc) when Pos == RetPos ->
				NewStand = {Id, erlang:length(LRun), retired},
				{NewStand, [NewStand | Acc]};
		   ({Id, Pos, running}, Acc) when Pos > RetPos ->
				NewStand = {Id, Pos - 1, running},
				{NewStand, [NewStand | Acc]}
		end,
	{NewRunStand, ChangeList} = lists:mapfoldl(F, [], LRun),
	Standings = lists:append(NewRunStand, LRet),
	Subs = event_dispatcher:notify_update({standings, ChangeList}, State#state.subscribers),
	{noreply, State#state{standings = Standings, subscribers = Subs}};

handle_cast(Msg, State) when is_record(Msg, surpass_notif) ->
	{_, SedP, SedS} = lists:keyfind(Msg#surpass_notif.surpassed, 1, State#state.standings),
	{_, SerP, SerS} = lists:keyfind(Msg#surpass_notif.surpasser, 1, State#state.standings),
	if
		SedP - 1 == SerP ->
			Ser = {Msg#surpass_notif.surpasser, SedP, SerS},
			Sed = {Msg#surpass_notif.surpasser, SerP, SedS},
			S1 = lists:keyreplace(Msg#surpass_notif.surpasser, 1, State#state.standings, Ser),
			S2 = lists:keyreplace(Msg#surpass_notif.surpassed, 1, S1, Sed),
			Subs = event_dispatcher:notify_update({standings, [Sed, Ser]}, 
												  State#state.subscribers),
			{noreply, State#state{standings = S2, subscribers = Subs}};
		true ->
			{noreply, State}
	end.

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
	CPs = State#state.cars_pos,
	Subs = State#state.subscribers,
	{NewCPs, NewSubs} = case Diff of
							[#car_position{car_id = CarId,
										   exit_lane = Lane}] ->
								case lists:keytake(CarId, 1, CPs) of
									{value, {_, Pos, _}, Rest} ->
										NewPos = Pos + NewSgm#segment.length,
										NewCP = {CarId, NewPos, Lane < 0},
										{[NewCP | Rest],
										 event_dispatcher:notify_update({car_pos, NewCP}, Subs)};
									false ->
										{CPs, Subs}
								end;
							_ ->
								{CPs, Subs}
						end,
	{noreply, State#state{cars_pos = NewCPs,
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

name({Id, _, N, TN}) ->
	{Id, N, TN}.