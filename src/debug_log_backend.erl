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

-module(debug_log_backend).

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

-record(state, {subscribers	= []	:: [#subscriber{}]}).


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
	NewSubs = event_dispatcher:add_subscriber(S, State#state.subscribers, []),
	{noreply, State#state{subscribers = NewSubs}};

handle_cast(Msg, State) when is_record(Msg, car_state_notif);
							 is_record(Msg, chrono_notif);
							 is_record(Msg, pitstop_notif);
							 is_record(Msg, surpass_notif);
							 is_record(Msg, race_notif);
							 is_record(Msg, weather_notif) ->
	NewSubs = event_dispatcher:notify_update(to_string(Msg), State#state.subscribers),
	{noreply, State#state{subscribers = NewSubs}}.

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

%% Converts a notification record into a human-readable string.

-spec to_string(any_notif()) -> string().

to_string(#chrono_notif{car = C, lap = Lap, intermediate = Inter, time = T, max_speed = S}) ->
	lists:concat(["Car ", C, " has gone through intermediate ", Inter,
				  " of lap ", Lap, " in ", utils:digits(T),
				  " seconds, with a maximum speed of ",
				  utils:digits(S * 3.6), " Km/h."]);
to_string(#pitstop_notif{car = C, ops = #pitstop_ops{fuel = F, tyres = Ty}}) ->
	lists:concat(["Pitstop for car ", C, ": ", utils:digits(F),
				  " liters of fuel have been added and ",
				  Ty, " tyres have been installed."]);
to_string(#surpass_notif{surpasser = Surpasser, surpassed = Surpassed}) ->
	lists:concat(["Car ", Surpasser, " surpassed car ", Surpassed, "."]);
to_string(#race_notif{event = E}) ->
	lists:concat(["Race ", E, "."]);
to_string(#car_state_notif{car = C, state = State}) ->
	S = case State of
			{retired, Reason} ->
				lists:concat(["retired (", Reason, ")"]);
			Else ->
				Else
		end,
	lists:concat(["Car ", C, " state changed to: ", S, "."]);
to_string(#weather_notif{changes = Changes}) ->
	F = fun(#weather_change{segment = S, new_weather = New}, Acc) ->
				lists:concat([Acc, "\t", New, " in segment ", S, "\n"])
		end,
	lists:concat(["\nWeather changed to:\n", lists:foldl(F, "", Changes)]).
