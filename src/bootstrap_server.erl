-module(bootstrap_server).

-behaviour(gen_server).

%% External exports
-export([start/0,
		 start_link/0,
		 add_node/1,
		 bootstrap/2,
		 read_config_files/3,
		 set_gui_node/1,
		 shutdown/1]).

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

-type candidate() :: {node(), MaxInstances :: pos_integer()}.

-record(state, {bootstrapped	= false	:: boolean(),
				ready			= false	:: boolean(),
				candidates		= []	:: [{App :: atom(), [candidate()]}],
				nodes			= []	:: [node()],
				gui_node				:: node(),
				num_cars				:: non_neg_integer(),
				num_teams				:: non_neg_integer(),
				teams_config			:: conflist(),
				track_config			:: [sector()],
				weather_config			:: conflist()}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start() -> 'ok' | {'error', Reason :: term()}.

start() ->
	application:start(?MODULE).

-spec start_link() -> start_result().

start_link() ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, [], []).

-spec add_node(conflist()) -> 'ok' | {'error', Reason :: term()}.

add_node(SupportedApps) when is_list(SupportedApps) ->
	gen_server:call(?GLOBAL_NAME, {add_node, SupportedApps}, infinity).

-spec bootstrap(pos_integer(), number()) -> 'ok' | {'error', Reason :: term()}.

bootstrap(Laps, Speedup)
  when is_integer(Laps), Laps > 0, is_integer(Speedup), Speedup > 0 ->
	gen_server:call(?GLOBAL_NAME, {bootstrap, Laps, Speedup}, infinity).

-spec read_config_files(string(), string(), string()) -> 'ok' | {'error', Reason :: term()}.

read_config_files(TeamsFile, TrackFile, WeatherFile)
  when is_list(TeamsFile), is_list(TrackFile), is_list(WeatherFile) ->
	gen_server:call(?GLOBAL_NAME, {read_config_files, TeamsFile, TrackFile, WeatherFile}).

-spec set_gui_node(node()) -> 'ok'.

set_gui_node(Node) when is_atom(Node) ->
	gen_server:call(?GLOBAL_NAME, {set_gui_node, Node}).

-spec shutdown(boolean()) -> 'ok'.

shutdown(StopNodes) when is_boolean(StopNodes) ->
	gen_server:call(?GLOBAL_NAME, {shutdown, StopNodes}, infinity).


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
handle_call({add_node, _SupportedApps}, _From, State) when State#state.bootstrapped ->
	% new nodes cannot be added while the system is running
	{reply, {error, "already bootstrapped"}, State};
handle_call({add_node, SupportedApps}, {Pid, _Tag}, State) ->
	Node = node(Pid),
	
	% start monitoring the connection
	monitor_node(Node, true),
	% tell the control panel that a new node is available
	rpc:call(State#state.gui_node, control_panel, node_up, [Node]),
	
	% update the list of candidates
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
	NewState = State#state{candidates = NewCandidates,
						   nodes = State#state.nodes ++ [Node]},
	
	% re-check the requirements
	{reply, ok, NewState#state{ready = check_reqs(NewState)}};

handle_call({bootstrap, _Laps, _Speedup}, _From, State) when State#state.bootstrapped ->
	{reply, {error, "already bootstrapped"}, State};
handle_call({bootstrap, Laps, Speedup}, _From, #state{nodes = Nodes} = State)
  when State#state.ready, State#state.num_cars > 0 ->
	Master = hd(Nodes),
	
	% setup applications' configurations
	Dispatcher = [],
	Scheduler = [{speedup, Speedup}],
	{Teams, TeamsIDs, Cars, CarsIDs} = split_config(State#state.teams_config),
	Weather = State#state.weather_config,
	
	% mnesia database initialization
	rpc:multicall(Nodes, mnesia, start, []),
	rpc:multicall(Nodes, mnesia, change_config, [extra_db_nodes, Nodes]),
	rpc:call(Master, mnesia, create_table, [setting, ?TAB_DEF(setting, Nodes)]),
	rpc:call(Master, mnesia, create_table, [track, ?TAB_DEF(segment, Nodes)]),
	rpc:call(Master, mnesia, create_table, [car_type, ?TAB_DEF(car_type, Nodes)]),
	rpc:call(Master, mnesia, create_table, [pilot, ?TAB_DEF(pilot, Nodes)]),
	lists:foreach(fun(Id) ->
						  rpc:call(Master, mnesia, create_table,
								   [?PREELAB_TABLE(Id), ?TAB_DEF(speed_bound, Nodes)])
				  end, CarsIDs),
	
	% applications initialization
	ChooseNodes = fun({App, N}) ->
						  {_, AppNodes} = lists:keyfind(App, 1, State#state.candidates),
						  {App, choose_nodes(AppNodes, N, [])}
				  end,
	NodesConfig = lists:map(ChooseNodes, ?GEN_REQS(State#state.num_cars, State#state.num_teams)),
	Start = fun(App) ->
					Configs = case App of
								  car -> Cars;
								  event_dispatcher -> [Dispatcher];
								  scheduler -> [Scheduler];
								  team -> Teams;
								  weather -> [Weather]
							  end,
					AppSpecs = lists:map(fun(C) -> app_spec(App, C) end, Configs),
					{_, AppNodes} = lists:keyfind(App, 1, NodesConfig),
					start_apps(AppSpecs, AppNodes, Nodes)
			end,
	lists:foreach(Start, ?BOOTSTRAP_ORDER),
	
	% track & settings initialization
	% FTNOTE: change the following line when track becomes a gen_server
	rpc:call(Master, track, init, [State#state.track_config, TeamsIDs, CarsIDs]),
	rpc:call(Master, utils, set_setting, [running_cars, State#state.num_cars]),
	rpc:call(Master, utils, set_setting, [total_laps, Laps]),
	
	{reply, ok, State#state{bootstrapped = true}};
handle_call({bootstrap, _Laps, _Speedup}, _From, State) ->
	{reply, {error, "requirements not satisfied"}, State};

handle_call({read_config_files, _, _, _}, _From, State) when State#state.bootstrapped ->
	{reply, {error, "already bootstrapped"}, State};
handle_call({read_config_files, TeamsFile, TrackFile, WeatherFile}, _From, State) ->
	try
		% try reading the supplied configuration files
		Teams = consult(TeamsFile),
		[Track] = consult(TrackFile),
		[Weather] = consult(WeatherFile),
		
		% count the number of cars declared in the config
		% file and check that there are no duplicate IDs
		Dup = fun(Car, Acc) ->
					  case lists:keyfind(id, 1, Car) of
						  {id, Id} when is_integer(Id), Id > 0 ->
							  case lists:member(Id, Acc) of
								  true ->
									  throw(lists:concat(["duplicate car ID ", Id]));
								  false ->
									  [Id | Acc]
							  end;
						  _ ->
							  Acc
					  end
			  end,
		Count = fun(Team, {N, Acc}) ->
						case lists:keyfind(cars, 1, Team) of
							{cars, Cars} when is_list(Cars) ->
								{N + length(Cars), lists:foldl(Dup, Acc, Cars)};
							_ ->
								{N, Acc}
						end
				end,
		{NumCars, _} = lists:foldl(Count, {0, []}, Teams),
		
		NewState = State#state{num_cars = NumCars,
							   num_teams = length(Teams),
							   teams_config = Teams,
							   track_config = Track,
							   weather_config = Weather},
		
		% check if requirements are already satisfied
		Ready = check_reqs(NewState),
		{reply, ok, NewState#state{ready = Ready}}
	catch
		error: {badmatch, _} = Error ->
			{reply, {error, Error}, State};
		throw: Error ->
			{reply, {error, Error}, State}
	end;

handle_call({set_gui_node, Node}, _From, State) ->
	{reply, ok, State#state{gui_node = Node}};

handle_call({shutdown, StopNodes}, _From, State) ->
	Nodes = State#state.nodes,
	if
		StopNodes ->
			rpc:multicall(Nodes, init, stop, []),
			init:stop();
		State#state.bootstrapped ->
			gen_server:multi_call(Nodes, node_manager, stop_apps);
		true ->
			ok
	end,
	{reply, ok, State#state{bootstrapped = false}};

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
handle_info({nodedown, Node}, State) ->
	% notify the control panel that the node went down
	rpc:call(State#state.gui_node, control_panel, node_down, [Node]),
	% delete the node from the list of candidates
	Delete = fun({App, NodesList}) ->
					 {App, lists:keydelete(Node, 1, NodesList)}
			 end,
	NewState = State#state{candidates = lists:map(Delete, State#state.candidates),
						   nodes = lists:delete(Node, State#state.nodes)},
	% re-check the requirements
	{noreply, NewState#state{ready = check_reqs(NewState)}};

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

-spec app_spec(atom(), conflist()) -> tuple().

app_spec(car, Config) ->
	{id, Id} = lists:keyfind(id, 1, Config),
	{application, utils:build_id_atom("car_", Id),
	 [{applications, [kernel, stdlib, event_dispatcher, scheduler]},
	  {mod, {car_app, Config}}]};
app_spec(event_dispatcher, Config) ->
	{application, event_dispatcher,
	 [{applications, [kernel, stdlib]},
	  {mod, {dispatcher_app, Config}}]};
app_spec(scheduler, Config) ->
	{application, scheduler,
	 [{applications, [kernel, stdlib, event_dispatcher]},
	  {mod, {scheduler_app, Config}}]};
app_spec(team, Config) ->
	{id, Id} = lists:keyfind(id, 1, Config),
	{application, utils:build_id_atom("team_", Id),
	 [{applications, [kernel, stdlib, event_dispatcher]},
	  {mod, {team_app, Config}}]};
app_spec(weather, Config) ->
	{application, weather,
	 [{applications, [kernel, stdlib, scheduler]},
	  {mod, {weather_app, Config}}]}.

-spec check_reqs(#state{}) -> boolean().

check_reqs(#state{teams_config = undefined}) ->
	false;
check_reqs(State) ->
	Sum = fun({_, N}, Acc) -> Acc + N end,
	Check = fun({App, Min}) ->
					case lists:keyfind(App, 1, State#state.candidates) of
						{App, Nodes} ->
							lists:foldl(Sum, 0, Nodes) >= Min;
						false ->
							false
					end
			end,
	case lists:all(Check, ?GEN_REQS(State#state.num_cars, State#state.num_teams)) of
		true ->
			% notify the control panel that we can proceed with the bootstrap
			rpc:call(State#state.gui_node, control_panel, ready, []),
			true;
		false ->
			% tell the control panel that we aren't ready
			rpc:call(State#state.gui_node, control_panel, not_ready, []),
			false
	end.

-spec choose_nodes([candidate()], non_neg_integer(), [node()]) -> [node()].

choose_nodes(_, 0, Config) ->
	Config;
choose_nodes([{_Node, 0} | Tail], N, Config) ->
	choose_nodes(Tail, N, Config);
choose_nodes([{Node, Avail} | Tail], N, Config) ->
	choose_nodes(Tail ++ [{Node, Avail - 1}], N - 1, [Node | Config]).

-spec consult(string()) -> [term()].

consult(Filename) ->
	case file:consult(Filename) of
		{ok, Terms} ->
			Terms;
		{error, {Line, Mod, Term}} ->
			throw(file:format_error({Line, Mod, Term}));
		{error, Reason} ->
			throw(Reason)
	end.

-spec split_config(conflist()) -> {conflist(), [team()], conflist(), [car()]}.

split_config(Config) ->
	% split up cars config from teams config
	Split = fun(Team, {Id, T, C} = Acc) ->
					SetTeam = fun(Car) ->
									  [{team, Id} | Car]
							  end,
					case lists:keytake(cars, 1, Team) of
						{value, {cars, CarsList}, NewTeam} ->
							{Id + 1,
							 T ++ [[{id, Id} | NewTeam]],
							 C ++ lists:map(SetTeam, CarsList)};
						false ->
							% teams without cars are ignored
							Acc
					end
			end,
	{_, T, C} = lists:foldl(Split, {1, [], []}, Config),
	
	% extract cars IDs and check if every car has one
	ExtractIDs = fun(Car, Acc) ->
						 case lists:keyfind(id, 1, Car) of
							 {id, Id} when is_integer(Id), Id > 0 ->
								 {Id, Acc};
							 _ ->
								 {0, false}
						 end
				 end,
	{ConfigIDs, Valid} = lists:mapfoldl(ExtractIDs, true, C),
	
	{Cars, CarsIDs} = case Valid of
						  true ->
							  {C, ConfigIDs};
						  false ->
							  % not every car has a valid ID: generate a new
							  % list of IDs and assign each of them to a car
							  RandomIDs = utils:shuffle(lists:seq(1, length(C))),
							  Combine = fun(Car, Id) ->
												lists:keystore(id, 1, Car, {id, Id})
										end,
							  {lists:zipwith(Combine, C, RandomIDs), RandomIDs}
					  end,
	{T, lists:seq(1, length(T)), Cars, CarsIDs}.

-spec start_apps([tuple()], [node()], [node()]) -> 'ok'.

start_apps([AppSpec | SpecsTail], [MainNode | NodesTail], Nodes) ->
	FailoverNodes = lists:delete(MainNode, Nodes),
	gen_server:multi_call(Nodes, node_manager,
						  {load_app, AppSpec, MainNode, FailoverNodes}),
	gen_server:multi_call(Nodes, node_manager,
						  {start_app, element(2, AppSpec)}),
	start_apps(SpecsTail, NodesTail, Nodes);
start_apps([], [], _Nodes) ->
	ok.
