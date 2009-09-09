%% -----------------------
%%  Database tuple format
%% -----------------------

%%------------------------
%% car_id: ref to pilot table
%% enter_t: the time in which the car entered in the current segment 
%% 			(the time it exited from the previous one)
%% exit_t: the timein which the car will exit from the current segment
%% speed: exit speed
%% lane: exit lane
%%------------------------

-record(car_position,{car_id,
					  enter_t,
					  exit_t,
					  speed,
					  lane}).

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
%% queued_cars: list of car_position
%% -----------------------

-record(segment,{id,
				 type,
				 min_lane,
				 max_lane,
				 lenght,
				 inclination,
				 rain,
				 curvature,
				 queued_cars = []}).

%%------------------------
%% id: unique numerical identifier
%% name: pilot's name
%% skill: integer representing pilot's skill
%% weight: pilot's weight
%% fuel: fuel left
%% tyres_consumption: TODO define value range and type
%% tyres_type: slick | intermediate | wet
%% team_name: reference to car_type tab
%% -----------------------

-record(pilot,{id,
			   name,
			   skill,
			   weight,
			   fuel,
			   tyres_consumption,
			   tyres_type,
			   team_name}).

%%------------------------
%% id: unique numerical identifier
%% brake: determines max deceleration (must be negative)
%% power: determines max speed and max acceleration
%% weight: car's weight (fuel not included)
%% team_name: guess...
%% -----------------------

-record(car_type,{team_name,
				  brake,
				  power,
				  weight}).