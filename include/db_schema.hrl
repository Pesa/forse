%%% =======================
%%%  Database tuple format
%%% =======================

-include("common.hrl").

%% --------------------------------------------------
%% car_id: ref to pilot table
%% enter_t: when the car entered the current segment
%% 			(i.e. the time it exited from the previous one)
%% exit_t: when the car will leave the current segment
%% speed: exit speed
%% lane: exit lane
%% --------------------------------------------------
-record(car_position,{car_id,
					  enter_t,
					  exit_t,
					  speed,
					  enter_lane,
					  exit_lane}).

%% --------------------------------------------------
%% id: unique numerical identifier
%% type: segment's type, can be one of the following atoms:
%%			straight | bent | pre_pitlane | post_pitlane |
%%			pitlane | pitstop | intermediate | finish_line
%% min_lane: minimum lane index
%% max_lane: maximum lane index
%% length: the length of the segment
%% inclination: in degrees
%% rain: integer from 0 (sun) to 10 (heavy rain)
%% curvature: radius of curvature
%% queued_cars: list of car_position records
%% --------------------------------------------------
-record(segment,{id,
				 type,
				 min_lane,
				 max_lane,
				 length,
				 inclination,
				 rain,
				 curvature,
				 queued_cars = []}).

%% --------------------------------------------------
%% id: unique numerical identifier
%% name: pilot's name
%% skill: integer representing the pilot's skill
%% weight: pilot's weight
%% team: id of the pilot's team
%% car_status: current car status
%% lap: current lap
%% segment: id of the current segment
%% lane: current lane
%% max_speed: maximum speed reached in the current intermediate
%% next_pitstop: lap of the next pitstop
%% pitstop_count: number of pit stops the car has done
%% run_preelab: set to true when the pre-elaboration phase must be re-run
%% --------------------------------------------------
-record(pilot,{id,
			   name,
			   skill,
			   weight,
			   team,
			   car_status = #car_status{},
			   lap = 0,
			   segment = 0,
			   lane,
			   max_speed = 0,
			   next_pitstop = -1,
			   pitstop_count = 0,
			   run_preelab = true}).

%% --------------------------------------------------
%% id: unique numerical identifier
%% team_name: name of the team to which the car belongs
%% brake: determines max deceleration, must be negative [F = m*a]
%% power: determines max speed and max acceleration [F = m*a]
%% weight: car's weight (excluding fuel)
%% pitstop_sgm: id of the segment containing the team's pits
%% --------------------------------------------------
-record(car_type,{id,
				  team_name,
				  brake,
				  power,
				  weight,
				  pitstop_sgm}).

%% --------------------------------------------------
%% smg_id: segment id
%% bound: maximum speed calculated at pre-elaboration time
%% pit_bound: maximum speed calculated at pre-elaboration time
%% if car have to enter pitstop
%% --------------------------------------------------
-record(speed_bound,{sgm_id,
					 bound,
					 pit_bound}).

%% --------------------------------------------------
-record(setting, {key, value}).
