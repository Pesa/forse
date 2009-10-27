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

-record(state, {bootstrapped = false,
				nodes = [],
				dist_config = [],
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
	NewNodes = State#state.nodes ++ [Node],
	F = fun({App, N}, Config) ->
				NewApp = case lists:keyfind(App, 1, Config) of
							 {App, List} -> {App, List ++ [{Node, N}]};
							 false -> {App, [{Node, N}]}
						 end,
				lists:keyreplace(App, 1, Config, NewApp);
		   (_, Config) ->
				Config
		end,
	NewDist = lists:foldl(F, State#state.dist_config, SupportedApps),
	check_requirements(NewDist),
	{reply, ok, State#state{nodes = NewNodes,
							dist_config = NewDist}};
handle_call({add_node, _SupportedApps}, _From, State) ->
	% new nodes cannot be added while the system is running
	{reply, {error, already_started}, State};

handle_call({bootstrap, Laps, Speedup}, _From, State) when State#state.teams_config /= undefined ->
	% TODO
	track:init(State#state.track_config, length(State#state.teams_config)),
	{stop, normal, ok, State#state{bootstrapped = true}};
handle_call({bootstrap, _Laps, _Speedup}, _From, State) ->
	{reply, {error, not_configured}, State};

handle_call({read_config_files, TeamsFile, TrackFile, WeatherFile}, _From, State) ->
	case file:consult(TeamsFile) of
		{ok, Teams} ->
			case file:consult(TrackFile) of
				{ok, Track} ->
					case file:consult(WeatherFile) of
						{ok, Weather} ->
							{reply, ok, State#state{teams_config = Teams,
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

check_requirements(DistConfig) ->
	% TODO
	rpc:cast(node, mod, func, []).
