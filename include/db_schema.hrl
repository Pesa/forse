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
%% --------------------------------------------------
-record(car_position,{car_id				:: car(),
					  enter_t		= 0.0	:: float(),
					  exit_t		= 0.0	:: float(),
					  speed			= 0.0	:: float(),
					  enter_lane			:: lane(),
					  exit_lane				:: lane()}).

%% --------------------------------------------------
%% id: unique numerical identifier
%% type: segment's type
%% min_lane: minimum lane index
%% max_lane: maximum lane index
%% length: the length of the segment
%% inclination: in degrees
%% rain: integer from 0 (sun) to 10 (heavy rain)
%% curvature: radius of curvature
%% queued_cars: list of cars queued in this segment
%% --------------------------------------------------
-record(segment,{id						:: sgm_id(),
				 type					:: sgm_type(),
				 min_lane				:: lane(),
				 max_lane				:: lane(),
				 length			= 0		:: non_neg_integer(),
				 inclination	= 0.0	:: float(),
				 rain			= 0		:: rain_amount(),
				 curvature		= 0.0	:: float(),
				 queued_cars	= []	:: [#car_position{}]}).

%% --------------------------------------------------
%% id: unique numerical identifier
%% name: pilot's name
%% skill: integer from 1 to 10 representing the pilot's skill
%% weight: pilot's weight
%% team: id of the pilot's team
%% car_status: current car status
%% lap: current lap
%% segment: id of the current segment
%% lane: current lane
%% max_speed: maximum speed reached in the current intermediate
%% next_pitstop: lap of the next pitstop
%% pitstop_count: number of pit stops the car has done
%% retire: true when an user requested the retirement of this car
%% run_preelab: true when the pre-elaboration phase must be re-run
%% --------------------------------------------------
-record(pilot,{id								:: car(),
			   name								:: string(),
			   skill							:: skill(),
			   weight							:: float(),
			   team								:: team(),
			   car_status		= #car_status{}	:: #car_status{},
			   lap				= 0				:: lap(),
			   segment			= 0				:: sgm_id(),
			   lane								:: lane(),
			   max_speed		= 0.0			:: float(),
			   next_pitstop						:: pitstop_lap(),
			   pitstop_count	= 0				:: non_neg_integer(),
			   retire			= false			:: boolean(),
			   run_preelab		= true			:: boolean()}).

%% --------------------------------------------------
%% id: unique numerical identifier
%% team_name: name of the team to which the car belongs
%% brake: determines max deceleration, must be negative
%% power: determines max speed and max acceleration
%% weight: car's weight (excluding fuel)
%% pitstop_sgm: id of the segment containing the team's pits
%% --------------------------------------------------
-record(car_type,{id			:: team(),
				  team_name		:: string(),
				  brake			:: float(),
				  power			:: float(),
				  weight		:: float(),
				  pitstop_sgm	:: sgm_id()}).

%% --------------------------------------------------
%% smg_id: id of the segment
%% bound: maximum speed calculated at pre-elaboration time
%% pit_bound: same as bound, but it's applied only when
%%			  driving through the pitlane
%% --------------------------------------------------
-record(speed_bound,{sgm_id		:: sgm_id(),
					 bound		:: float(),
					 pit_bound	:: float()}).

%% --------------------------------------------------
-record(setting, {key	:: atom(),
				  value	:: term()}).
