-module(access).

%% Exported Functions
-export([allow_move/5]).

%% Include files
-include("config.hrl").


%%
%% API Functions
%%

% Checks if Pilot can move from EnterLane to ExitLane in Sgm.
% Returns: 'run' | 'pits' | 'crash'
allow_move(Pilot, Sgm, EnterLane, ExitLane, Pit) when is_record(Pilot, pilot),
													  is_record(Sgm, segment) ->
	MaxL = Sgm#segment.max_lane,
	MinL = Sgm#segment.min_lane,
	Type = Sgm#segment.type,
	
	PS = Type == pitstop andalso EnterLane == MaxL - 1,
	PL = Type == pitlane andalso EnterLane == MaxL,
	PrePL = Type == pre_pitlane andalso EnterLane == MaxL,
	PostPL = Type == post_pitlane andalso EnterLane == MaxL,
	% TODO: si riesce a ottimizzare meglio?
	%		(per evitare la lettura dal DB)
	OwnPits = if
				  PS ->
					  T = utils:mnesia_read(car_type, Pilot#pilot.team_name),
					  T#car_type.pitstop_sgm == Sgm#segment.id;
				  true ->
					  unused
			  end,
	
	if
		ExitLane < MinL;
		ExitLane > MaxL;
		abs(ExitLane - EnterLane) > 1;
		PL andalso ExitLane /= EnterLane;
		Type == pre_pitlane andalso ExitLane == MaxL andalso not Pit;
		ExitLane < EnterLane andalso (PrePL orelse PostPL orelse PS);
		PS andalso ExitLane == MaxL andalso not OwnPits;
		PS andalso ExitLane == MaxL - 1 andalso OwnPits; 
		Type == pitlane andalso EnterLane /= MaxL andalso ExitLane == MaxL; 
		Type == pitstop andalso EnterLane < MaxL - 1 andalso ExitLane >= MaxL - 1 -> crash;
		PS andalso ExitLane == MaxL andalso OwnPits -> pits;
		true -> run
	end.
