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
	PS = Sgm#segment.type == pitstop andalso EnterLane == Sgm#segment.max_lane - 1,
	PL = Sgm#segment.type == pitlane andalso EnterLane == Sgm#segment.max_lane,
	PrePL = Sgm#segment.type == pre_pitlane andalso EnterLane == Sgm#segment.max_lane,
	PostPL = Sgm#segment.type == post_pitlane andalso EnterLane == Sgm#segment.max_lane,
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
		ExitLane < Sgm#segment.min_lane;
		ExitLane > Sgm#segment.max_lane;
		abs(ExitLane - EnterLane) > 1;
		PL andalso ExitLane /= EnterLane;
		Sgm#segment.type == pre_pitlane andalso ExitLane == Sgm#segment.max_lane andalso not Pit;
		ExitLane < EnterLane andalso (PrePL orelse PostPL orelse PS);
		PS andalso ExitLane == Sgm#segment.max_lane andalso not OwnPits;
		PS andalso ExitLane == Sgm#segment.max_lane - 1 andalso OwnPits -> crash;
		PS andalso ExitLane == Sgm#segment.max_lane andalso OwnPits -> pits;
		true -> run
	end.
