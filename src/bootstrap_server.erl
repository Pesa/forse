-module(bootstrap_server).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
		 add_node/1,
		 bootstrap/2,
		 read_config_files/3]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-define(BOOTSTRAP_ORDER, [event_dispatcher,
						  scheduler,
						  weather,
						  team,
						  car]).
-define(GEN_REQS(NCars, NTeams), [{event_dispatcher, 1},
								  {scheduler, 1},
								  {car, NCars},
								  {team, NTeams},
								  {weather, 1}]).
-define(TAB_DEF(Record, Nodes), [{attributes, record_info(fields, Record)},
								 {ram_copies, Nodes},
								 {record_name, Record}]).

-record(state, {bootstrapped = false,
				candidates = [],
				nodes = [],
				num_cars,
				num_teams,
				teams_config,
				track_config,
				weather_config}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, [], []).

add_node(SupportedApps) when is_list(SupportedApps) ->
	gen_server:call(?GLOBAL_NAME, {add_node, SupportedApps}, infinity).

bootstrap(Laps, Speedup) when is_integer(Laps), is_integer(Speedup) ->
	gen_server:call(?GLOBAL_NAME, {bootstrap, Laps, Speedup}, infinity).

read_config_files(TeamsFile, TrackFile, WeatherFile)
  when is_list(TeamsFile), is_list(TrackFile), is_list(WeatherFile) ->
	gen_server:call(?GLOBAL_NAME, {read_config_files, TeamsFile, TrackFile, WeatherFile}).


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
handle_call({add_node, SupportedApps}, {Pid, _Tag}, State) when not State#state.bootstrapped ->
	Node = node(Pid),
	F = fun({App, N}, Config) when is_integer(N), N > 0 ->
				NewApp = case lists:keyfind(App, 1, Config) of
							 {App, List} ->
								 {App, List ++ [{Node, N}]};
							 false ->
								 {App, [{Node, N}]}
						 end,
				lists:keystore(App, 1, Config, NewApp);
		   (_, Config) ->
				Config
		end,
	NewCandidates = lists:foldl(F, State#state.candidates, SupportedApps),
	case State#state.teams_config of
		undefined -> ok;
		_ -> check_reqs(NewCandidates, ?GEN_REQS(State#state.num_cars,
												 State#state.num_teams))
	end,
	NewNodes = State#state.nodes ++ [Node],
	{reply, ok, State#state{candidates = NewCandidates,
							nodes = NewNodes}};
handle_call({add_node, _SupportedApps}, _From, State) ->
	% new nodes cannot be added while the system is running
	{reply, {error, already_started}, State};

handle_call({bootstrap, Laps, Speedup}, _From, #state{nodes = Nodes} = State) ->
	Reqs = ?GEN_REQS(State#state.num_cars, State#state.num_teams),
	case check_reqs(State#state.candidates, Reqs) of
		true when State#state.num_cars > 0 ->
			% FIXME: how to choose the master node?
			Master = hd(Nodes),
			SplitConfig = fun(Team, {Id, T, C} = Acc) ->
								  SetTeam = fun(Car) ->
													[{team, Id} | Car]
											end,
								  case lists:keytake(cars, 1, Team) of
									  {value, {cars, CarsList}, NewTeam} ->
										  {Id + 1,
										   T ++ [[{id, Id} | NewTeam]],
										   C ++ lists:map(SetTeam, CarsList)};
									  false ->
										  % teams without cars are completely ignored
										  Acc
								  end
						  end,
			{_, Teams, Cars} = lists:foldl(SplitConfig, {1, [], []}, State#state.teams_config),
			
			% mnesia database initialization
			rpc:multicall(Nodes, mnesia, start, []),
			rpc:multicall(Nodes, mnesia, change_config, [extra_db_nodes, Nodes]),
			rpc:call(Master, mnesia, create_table, [setting, ?TAB_DEF(setting, Nodes)]),
			rpc:call(Master, mnesia, create_table, [track, ?TAB_DEF(segment, Nodes)]),
			rpc:call(Master, mnesia, create_table, [car_type, ?TAB_DEF(car_type, Nodes)]),
			rpc:call(Master, mnesia, create_table, [pilot, ?TAB_DEF(pilot, Nodes)]),
			ExtractIDs = fun(Car) ->
								 case lists:keyfind(id, 1, Car) of
									 {id, Id} ->
										 rpc:call(Master, mnesia, create_table,
												  [?PREELAB_TABLE(Id), ?TAB_DEF(speed_bound, Nodes)]),
										 Id;
									 false ->
										 % the IDs of cars are mandatory
										 throw(car_id_not_found)
								 end
						 end,
			CarsIDs = lists:map(ExtractIDs, Cars),
			
			% track & settings initialization
			% FIXME: change this when track becomes a gen_server
			rpc:call(Master, track, init, [State#state.track_config, State#state.num_teams, CarsIDs]),
			rpc:call(Master, utils, set_setting, [total_laps, Laps]),
			
			% applications initialization
			ChooseNodes = fun({App, N}) ->
								  {_, AppNodes} = lists:keyfind(App, 1, State#state.candidates),
								  {App, choose_nodes(AppNodes, N, [])}
						  end,
			NodesConfig = lists:map(ChooseNodes, Reqs),
			Start = fun(App) ->
							{_, AppNodes} = lists:keyfind(App, 1, NodesConfig),
							ZippedList = case App of
											 car ->
												 lists:zip(AppNodes, Cars);
											 event_dispatcher ->
												 AppNodes;
											 scheduler ->
												 lists:map(fun(X) ->
																   {X, Speedup}
														   end, AppNodes);
											 team ->
												 lists:zip(AppNodes, Teams);
											 weather ->
												 lists:map(fun(X) ->
																   {X, State#state.weather_config}
														   end, AppNodes)
										 end,
							lists:foreach(fun(ZippedConfig) ->
												  start_app(App, ZippedConfig, Nodes)
										  end, ZippedList)
					end,
			lists:foreach(Start, ?BOOTSTRAP_ORDER),
			
			{stop, normal, ok, State#state{bootstrapped = true}};
		_ ->
			{reply, config_error, State}
	end;

handle_call({read_config_files, TeamsFile, TrackFile, WeatherFile}, _From, State) ->
	try
		{ok, Teams} = file:consult(TeamsFile),
		{ok, [Track]} = file:consult(TrackFile),
		{ok, [Weather]} = file:consult(WeatherFile),
		% count the number of cars declared in the config file
		Count = fun(Team, Acc) ->
						case lists:keyfind(cars, 1, Team) of
							{cars, Cars} when is_list(Cars) ->
								Acc + length(Cars);
							_ ->
								Acc
						end
				end,
		{reply, ok, State#state{num_cars = lists:foldl(Count, 0, Teams),
								num_teams = length(Teams),
								teams_config = Teams,
								track_config = Track,
								weather_config = Weather}}
	catch
		error : {badmatch, _} ->
			{reply, config_error, State}
	end;

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

check_reqs(Candidates, Reqs) ->
	Sum = fun({_, N}, Acc) -> Acc + N end,
	Check = fun({App, Min}) ->
					case lists:keyfind(App, 1, Candidates) of
						{App, Nodes} ->
							MaxAvail = lists:foldl(Sum, 0, Nodes),
							if
								MaxAvail >= Min -> true;
								true -> false
							end;
						false ->
							false
					end
			end,
	case lists:all(Check, Reqs) of
		true ->
			% TODO: notify the GUI that we can proceed
			% rpc:cast(node, mod, func, [])
			true;
		false ->
			false
	end.

choose_nodes(_, 0, Config) ->
	Config;
choose_nodes([{_Node, 0} | Tail], N, Config) ->
	choose_nodes(Tail, N, Config);
choose_nodes([{Node, Avail} | Tail], N, Config) ->
	choose_nodes(Tail ++ [{Node, Avail - 1}], N - 1, [Node | Config]).

start_app(car, {MainNode, Config}, Nodes) ->
	{id, Id} = lists:keyfind(id, 1, Config),
	AppSpec = {application, utils:build_id_atom("car_", Id),
			   [{applications, [kernel, stdlib, scheduler]},
				{mod, {car_app, [Config]}}]},
	do_remote_start(AppSpec, MainNode, Nodes);
start_app(event_dispatcher, MainNode, Nodes) ->
	AppSpec = {application, event_dispatcher,
			   [{applications, [kernel, stdlib]},
				{mod, {dispatcher_app, []}}]},
	do_remote_start(AppSpec, MainNode, Nodes);
start_app(scheduler, {MainNode, Speedup}, Nodes) ->
	AppSpec = {application, scheduler,
			   [{applications, [kernel, stdlib]},
				{mod, {scheduler_app, [Speedup]}}]},
	do_remote_start(AppSpec, MainNode, Nodes);
start_app(team, {MainNode, Config}, Nodes) ->
	{id, Id} = lists:keyfind(id, 1, Config),
	AppSpec = {application, utils:build_id_atom("team_", Id),
			   [{applications, [kernel, stdlib, event_dispatcher]},
				{mod, {team_app, [Config]}}]},
	do_remote_start(AppSpec, MainNode, Nodes);
start_app(weather, {MainNode, Config}, Nodes) ->
	AppSpec = {application, weather,
			   [{applications, [kernel, stdlib, scheduler]},
				{mod, {weather_app, [Config]}}]},
	do_remote_start(AppSpec, MainNode, Nodes).

do_remote_start(AppSpec, MainNode, Nodes) ->
	App = element(2, AppSpec),
	FailoverNodes = lists:delete(MainNode, Nodes),
	Dist = {App, [MainNode, list_to_tuple(FailoverNodes)]},
	rpc:multicall(Nodes, application, load, [AppSpec, Dist]),
	rpc:multicall(Nodes, application, start, [App]).