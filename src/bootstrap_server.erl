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

-include("common.hrl").

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
	gen_server:call(?GLOBAL_NAME, {add_node, SupportedApps}).

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
handle_call({add_node, SupportedApps}, From, State) when not State#state.bootstrapped ->
	Node = node(From),
	F = fun({App, N}, Config) when is_integer(N), N > 0 ->
				NewApp = case lists:keyfind(App, 1, Config) of
							 {App, List} ->
								 {App, List ++ [{Node, N}]};
							 false ->
								 {App, [{Node, N}]}
						 end,
				lists:keyreplace(App, 1, Config, NewApp);
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

handle_call({bootstrap, _Laps, _Speedup}, _From, State) ->
	Reqs = ?GEN_REQS(State#state.num_cars, State#state.num_teams),
	case check_reqs(State#state.candidates, Reqs) of
		true when State#state.num_cars > 0 ->
			CreateConfig = fun({App, N}) ->
								   {App, Nodes} = lists:keyfind(App, 1, State#state.candidates),
								   {App, choose_nodes(Nodes, N, [])}
						   end,
			AppsConfig = lists:map(CreateConfig, Reqs),
			track:init(State#state.track_config, State#state.num_teams),
			% TODO
			%lists:foreach(todo, ?BOOTSTRAP_ORDER),
			{stop, normal, ok, State#state{bootstrapped = true}};
		_ ->
			{reply, config_error, State}
	end;

handle_call({read_config_files, TeamsFile, TrackFile, WeatherFile}, _From, State) ->
	case file:consult(TeamsFile) of
		{ok, Teams} ->
			case file:consult(TrackFile) of
				{ok, [Track | _]} ->
					case file:consult(WeatherFile) of
						{ok, [Weather | _]} ->
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
													weather_config = Weather}};
						{error, Reason} ->
							Error = file:format_error(Reason),
							{reply, {weather_error, Error}, State}
					end;
				{error, Reason} ->
					Error = file:format_error(Reason),
					{reply, {track_error, Error}, State}
			end;
		{error, Reason} ->
			Error = file:format_error(Reason),
			{reply, {race_error, Error}, State}
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
			todo;
		false ->
			ok
	end.

choose_nodes(_, 0, Config) ->
	Config;
choose_nodes([{_Node, 0} | Tail], N, Config) ->
	choose_nodes(Tail, N, Config);
choose_nodes([{Node, Avail} | Tail], N, Config) ->
	choose_nodes(Tail ++ [{Node, Avail - 1}], N - 1, [Node | Config]).
