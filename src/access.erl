-module(access).

%% Exported functions
-export([check_move/5]).

%% Include files
-include("db_schema.hrl").


%% ====================================================================
%% API Functions
%% ====================================================================

% Checks if Pilot can move from EnterLane to ExitLane in Sgm.
-spec check_move(#pilot{}, #segment{}, integer(), integer(), boolean()) ->
				 'go' | 'pits' | {'fail', Reason :: atom()}.

check_move(Pilot, Sgm, EnterLane, ExitLane, Pit)
  when is_record(Pilot, pilot), is_record(Sgm, segment) ->
	MaxL = Sgm#segment.max_lane,
	MinL = Sgm#segment.min_lane,
	Type = Sgm#segment.type,
	PS = Type == pitstop andalso EnterLane == MaxL - 1,
	PL = Type == pitlane andalso EnterLane == MaxL,
	PrePL = Type == pre_pitlane andalso EnterLane == MaxL,
	PostPL = Type == post_pitlane andalso EnterLane == MaxL,
	Time = Type == intermediate orelse Type == finish_line,
	
	if
		ExitLane < MinL;
		ExitLane > MaxL;
		abs(ExitLane - EnterLane) > 1;
		Time andalso ExitLane /= EnterLane;
		PL andalso ExitLane /= EnterLane;
		Type == pre_pitlane andalso ExitLane == MaxL andalso not Pit;
		Type == pitlane andalso EnterLane /= MaxL andalso ExitLane == MaxL;
		Type == pitstop andalso EnterLane < MaxL - 1 andalso ExitLane >= MaxL - 1;
		ExitLane < EnterLane andalso (PrePL orelse PostPL orelse PS) ->
			{fail, 'access denied'};
		PS ->
			T = utils:mnesia_read(car_type, Pilot#pilot.team),
			OwnPits = T#car_type.pitstop_sgm == Sgm#segment.id,
			if
				ExitLane == MaxL andalso not OwnPits;
				ExitLane == MaxL - 1 andalso OwnPits -> {fail, 'pitstop policy'};
				ExitLane == MaxL andalso OwnPits -> pits;
				true -> go
			end;
		true -> go
	end.
