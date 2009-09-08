%% -----------------------
%%  Database tuple format
%% -----------------------


%%------------------------
%% id: unique numerical identifier
%% type: straight | pitstop | pitlane | bent | intermediate
%% TODO il traguardo e le postazioni di partenza?
%% min_lane: minimum lane index
%% max_lane: maximum lane index
%% lenght: the lenght of the segment
%% inclination: degrees
%% rain: integer from 0 (sun) to N (heavy rain)
%% curvature: guess..
%% -----------------------

-record(segment,{id,
				 type,
				 min_lane,
				 max_lane,
				 lenght,
				 inclination,
				 rain,
				 curvature}).