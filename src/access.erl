-module(access).


%% Include files

-include("config.hrl").

%% Exported Functions

-export([allow_move/4]).

%%
%% API Functions
%%

%% Checks if Pilot can move from EnterLane to ExitLane in Sgm
%% Returns run | pits | crash
allow_move(Pilot, Sgm, EnterLane, ExitLane) 
  when is_record(Pilot, pilot) andalso is_record(Sgm, segment) ->
	OwnsPits = are_team_pits(Pilot, Sgm),
	PS = Sgm#segment.type == pitstop andalso EnterLane == Sgm#segment.max_lane - 1,
	PL = Sgm#segment.type == pitlane andalso EnterLane == Sgm#segment.max_lane,
	if
		ExitLane < Sgm#segment.min_lane -> crash;
		ExitLane > Sgm#segment.max_lane -> crash;
		erlang:abs(ExitLane - EnterLane) > 1 -> crash;
		PL andalso ExitLane /= EnterLane -> crash;
		PS andalso ExitLane < EnterLane -> crash;
		PS andalso ExitLane == Sgm#segment.max_lane andalso not OwnsPits -> crash;
		PS andalso ExitLane == Sgm#segment.max_lane - 1 andalso OwnsPits -> crash;
		PS andalso ExitLane == Sgm#segment.max_lane andalso OwnsPits -> pits;
		true -> run
	end.



%%
%% Local Functions
%%

are_team_pits(Pilot, Sgm) ->
	[T] = mnesia:read(car_type, Pilot#pilot.team_name),
	T#car_type.pitstop_sgm == Sgm#segment.id.
