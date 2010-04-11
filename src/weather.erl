-module(weather).

-behaviour(gen_server).

%% External exports
-export([start_link/1,
		 apply_change/2,
		 schedule_change/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-type hms() :: {Hours	:: non_neg_integer(),
				Minutes	:: 0..59,
				Seconds	:: 0..59}.

-record(state, {}).


%% ====================================================================
%% External functions
%% ====================================================================

-spec start_link(conflist()) -> start_result().

start_link(Config) when is_list(Config) ->
	gen_server:start_link(?GLOBAL_NAME, ?MODULE, Config, []).

-spec apply_change(time(), conflist()) -> token_reply().

apply_change(_Time, NewWeather) when is_list(NewWeather) ->
	gen_server:call(?GLOBAL_NAME, {apply_change, NewWeather}, infinity).

-spec schedule_change(hms() | non_neg_integer(), conflist()) -> 'ok' | 'error'.

schedule_change(When, NewWeather) when is_list(NewWeather) ->
	gen_server:call(?GLOBAL_NAME, {schedule_change, When, NewWeather}).


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
	lists:foreach(fun register_weather_change/1, Config),
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
handle_call({apply_change, NewWeatherList}, _From, State) ->
	ChSect = fun({SectId, NewWeather}, Acc) ->
					 ChSgm = fun(SgmId) ->
									 % update the weather in one segment
									 [Segment] = mnesia:wread({track, SgmId}),
									 mnesia:write(track, Segment#segment{rain = NewWeather}, write),
									 #weather_change{segment = SgmId,
													 old_weather = Segment#segment.rain,
													 new_weather = NewWeather}
							 end,
					 % get the IDs of the segments belonging to this sector
					 Sector = utils:build_id_atom("sector_", SectId),
					 {From, To} = utils:get_setting(Sector),
					 % for each segment invoke ChSgm to change its weather
					 SectChanges = lists:map(ChSgm, lists:seq(From, To)),
					 Acc ++ SectChanges
			 end,
	GetPilots = fun(Pilot, Acc) ->
						[Pilot#pilot.id | Acc]
				end,
	T = fun() ->
				% build a list containing the IDs of all pilots
				Pilots = mnesia:foldl(GetPilots, [], pilot),
				% apply the weather changes for each sector
				Changes = lists:foldl(ChSect, [], NewWeatherList),
				{Pilots, Changes}
		end,
	case mnesia:sync_transaction(T) of
		{atomic, {Pilots, Changes}} ->
			% invalidate the pre-elaboration for every pilot
			lists:foreach(fun(Id) ->
								  car:invalidate_preelab(Id)
						  end, Pilots),
			% send the notification
			event_dispatcher:notify(#weather_notif{changes = Changes});
		{aborted, Reason} ->
			?WARN({"failed to change weather", Reason})
	end,
	{reply, done, State};

handle_call({schedule_change, When, NewWeather}, _From, State) ->
	Reply = register_weather_change({When, NewWeather}),
	{reply, Reply, State};

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

-spec register_weather_change({hms() | non_neg_integer(), conflist()}) -> 'ok' | 'error'.

register_weather_change({{H, M, S}, NewWeather}) ->
	Callback = #callback{mod = ?MODULE, func = apply_change, args = [NewWeather]},
	scheduler:queue_work(H * 3600 + M * 60 + S, Callback);
register_weather_change({Seconds, NewWeather}) when is_integer(Seconds) ->
	Callback = #callback{mod = ?MODULE, func = apply_change, args = [NewWeather]},
	scheduler:queue_work(Seconds, Callback);
register_weather_change(Other) ->
	?WARN({"invalid weather change requested", Other}),
	error.
