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
		%abs(ExitLane - EnterLane) > 1; TODO
		% Deny access to pit area when not in need of pitstop
		Type == pre_pitlane andalso ExitLane == -1 andalso not Pit;
		% Separates pit area from main track
		(Type == pitlane orelse Type == pitstop) andalso EnterLane > 0 andalso ExitLane < 0;
		% pre/post pitlane rules
		(Type == pre_pitlane orelse Type == post_pitlane) 
		  andalso EnterLane < 0 andalso ExitLane > 0;
		% Check if a car crosses more than a lane
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

