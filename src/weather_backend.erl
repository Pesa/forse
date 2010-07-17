%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
%%                      Daniele Battaglia <dbat.fk@gmail.com>
%%
%%  This file is part of FORSE.
%%
%%  FORSE is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  FORSE is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(weather_backend).

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

-include("common.hrl").

-type sect_id()	:: non_neg_integer().

-record(state, {subscribers	= []			:: [#subscriber{}],
				race_state	= initialized	:: race_state(),
				map			= []			:: [{sgm_id(), sect_id()}],
				sectors		= []			:: [tuple()],
				weather		= []			:: [{sect_id(), rain_amount()}]}).


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
	List = [{race_state, State#state.race_state},
			{sectors, State#state.sectors},
			{weather, State#state.weather}],
	NewSubs = event_dispatcher:add_subscriber(S, State#state.subscribers, List),
	{noreply, State#state{subscribers = NewSubs}};

handle_cast(#config_notif{app = track, config = Config}, State) ->
	{sectors_map, SectorsMap} = lists:keyfind(sectors_map, 1, Config),
	Map = lists:map(fun({Sect, {_First, Last}}) ->
							{Last, Sect}
					end, SectorsMap),
	
	{sectors, Sectors} = lists:keyfind(sectors, 1, Config),
	S = race_info_backend:preprocess_sectors(Sectors),
	Subs1 = event_dispatcher:notify_init({sectors, S}, State#state.subscribers),
	
	ExtractRain = fun({straight, _, _, _, _, Rain}, {W, Id}) ->
						  {[{Id, Rain} | W], Id + 1};
					 ({left, _, _, _, _, _, Rain}, {W, Id}) ->
						  {[{Id, Rain} | W], Id + 1};
					 ({right, _, _, _, _, _, Rain}, {W, Id}) ->
						  {[{Id, Rain} | W], Id + 1};
					 (_, Acc) ->
						  Acc
				  end,
	{Weather, _} = lists:foldl(ExtractRain, {[], 0}, Sectors),
	Subs2 = event_dispatcher:notify_init({weather, Weather}, Subs1),
	
	{noreply, State#state{subscribers = Subs2,
						  map = lists:keysort(1, Map),
						  sectors = S,
						  weather = Weather}};

handle_cast(Msg, State) when is_record(Msg, config_notif) ->
	% ignore config_notif from other apps
	{noreply, State};

handle_cast(#race_notif{event = Ev}, State) ->
	RaceState = case Ev of
					started -> running;
					resumed -> running;
					Else -> Else
				end,
	Subs = event_dispatcher:notify_init({race_state, RaceState}, State#state.subscribers),
	{noreply, State#state{subscribers = Subs,
						  race_state = RaceState}};

handle_cast(#weather_notif{changes = Changes}, State) ->
	SortedChanges = lists:keysort(#weather_change.segment, Changes),
	SectChanges = sgms_to_sectors(SortedChanges, State#state.map),
	Apply = fun({Sect, Rain}, Weather) ->
					lists:keyreplace(Sect, 1, Weather, {Sect, Rain})
			end,
	NewWeather = lists:foldl(Apply, State#state.weather, SectChanges),
	Subs = event_dispatcher:notify_update({weather, SectChanges}, State#state.subscribers),
	{noreply, State#state{subscribers = Subs,
						  weather = NewWeather}}.

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


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec sgms_to_sectors([#weather_change{}], [{sgm_id(), sect_id()}]) ->
		[{sect_id(), rain_amount()}].

sgms_to_sectors([], _) ->
	[];
sgms_to_sectors(_, []) ->
	[];
sgms_to_sectors([#weather_change{segment = Sgm} = H | T], [{UpperBound, Sect} | Map])
  when Sgm =< UpperBound ->
	Rest = lists:dropwhile(fun(#weather_change{segment = S}) ->
								   S =< UpperBound
						   end, T),
	[{Sect, H#weather_change.new_weather} | sgms_to_sectors(Rest, Map)];
sgms_to_sectors(Changes, [_ | Tail]) ->
	sgms_to_sectors(Changes, Tail).
