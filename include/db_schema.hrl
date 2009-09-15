%% -----------------------
%%  Database tuple format
%% -----------------------

-include("common.hrl").

-define(TRACK_TAB, track).

%% ------------------------
%% car_id: ref to pilot table
%% enter_t: when the car entered the current segment
%% 			(i.e. the time it exited from the previous one)
%% exit_t: when the car will leave the current segment
%% speed: exit speed
%% lane: exit lane
%% ------------------------
-record(car_position,{car_id,
					  enter_t,
					  exit_t,
					  speed,
					  enter_lane,
					  exit_lane}).

%% ------------------------
%% id: unique numerical identifier
%% type: straight | pitstop | pitlane | bent | intermediate
%% TODO: il traguardo e le postazioni di partenza?
%% min_lane: minimum lane index
%% max_lane: maximum lane index
%% length: the length of the segment
%% inclination: in degrees
%% rain: integer from 0 (sun) to 10 (heavy rain)
%% curvature: radius of curvature
%% queued_cars: list of car_position records
%% -----------------------
-record(segment,{id,
				 type,
				 min_lane,
				 max_lane,
				 length,
				 inclination,
				 rain,
				 curvature,
				 queued_cars = []}).

%% ------------------------
%% id: unique numerical identifier
%% name: pilot's name
%% skill: integer representing pilot's skill
%% weight: pilot's weight
%% car_status: current car status
%% segment: segment.id pointing to the pilot's current position
%% team_name: reference to car_type tab
%% run_preelab: set to true when the preelaboration phase must be re-run
%% -----------------------
-record(pilot,{id,
			   name,
			   skill,
			   weight,
			   car_status = #car_status{},
			   segment,
			   team_name,
			   run_preelab = true}).

%% ------------------------
%% smg_id: segment id
%% bound: maximum speed calculated at preelaboration time
%% ------------------------
-record(speed_bound, {sgm_id,
					  bound}).

%% ------------------------
%% team_name: name of the team to which the car belongs
%% brake: determines max deceleration (must be negative)
%% power: determines max speed and max acceleration
%% weight: car's weight (excluding fuel)
%% -----------------------
-record(car_type,{team_name,
				  brake,
				  power,
				  weight}).

%% ------------------------
-record(setting, {key, value}).
