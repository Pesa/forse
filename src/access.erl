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

-module(access).

%% Exported functions
-export([check_move/5]).

%% Include files
-include("db_schema.hrl").


%% ====================================================================
%% API Functions
%% ====================================================================

% Checks if Pilot can move from EnterLane to ExitLane in Sgm.
-spec check_move(#pilot{}, #segment{}, lane(), lane(), boolean()) ->
			'go' | 'pits' | {'fail', Reason :: atom()}.
check_move(Pilot, Sgm, EnterLane, ExitLane, Pit)
  when is_record(Pilot, pilot), is_record(Sgm, segment) ->
	MaxL = Sgm#segment.max_lane,
	MinL = Sgm#segment.min_lane,
	Type = Sgm#segment.type,
	Time = Type == intermediate orelse Type == finish_line,
	AnyPitSgm = Type == pre_pitlane orelse Type == post_pitlane orelse
					Type == pitlane orelse Type == pitstop,
	Abs = if
			  EnterLane == -1 ->
				  [-2, -1, MinL];
			  EnterLane == -2 ->
				  [-2, -1];
			  EnterLane =< MinL ->
				  [-1, EnterLane, EnterLane + 1];
			  true ->
				  lists:seq(EnterLane - 1, EnterLane + 1)
		  end,
	Unreachable = not lists:member(ExitLane, Abs),
	
	if
		% Check if ExitLane exists in this segment
		ExitLane > MaxL;
		ExitLane < -2;
		ExitLane > -1 andalso ExitLane < MinL;
		Type == normal andalso ExitLane < MinL;
		ExitLane == -2 andalso (Type == pre_pitlane orelse
								Type == post_pitlane orelse
								Type == pitlane);
		% Deny lane changes in chrono segments
		Time andalso ExitLane /= EnterLane;
		% Deny access to the pit lane when not in need of a pitstop
		Type == pre_pitlane andalso ExitLane == -1 andalso not Pit;
		% Separate pit area from main track (including pre/post_pitlane rules)
		(Type == pitlane orelse Type == post_pitlane orelse Type == pitstop)
			andalso EnterLane > 0 andalso ExitLane < 0;
		AnyPitSgm andalso EnterLane < 0 andalso ExitLane > 0;
		% Check if a car crosses more than one lane
		Unreachable ->
			{fail, 'access denied'};
		% Check if it's team's own pits
		Type == pitstop andalso EnterLane == -1 ->
			T = utils:mnesia_read(car_type, Pilot#pilot.team),
			OwnPits = T#car_type.pitstop_sgm == Sgm#segment.id,
			if
				ExitLane == -2 andalso not OwnPits;
				ExitLane == -1 andalso OwnPits -> {fail, 'pitstop policy'};
				ExitLane == -2 andalso OwnPits -> pits;
				true -> go
			end;
		true -> go
	end.
