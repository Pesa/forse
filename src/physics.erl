-module(physics).

%%
%% Include files
%%

-include("db_schema.hrl").
-include("config.hrl").

%%
%% Exported Functions
%%
-export([get_car_ahead/3]).

%%
%% API Functions
%%

%% returns null or a car_position record
%% Index starts from 1

get_car_ahead(Sgm, Lane, Index) ->
	[R | _Tail] = mnesia:read(?TRACK_TAB, Sgm),
	Q = R#segment.queued_cars,

	Filter = fun
				(#car_position{exit_lane=Lane}) -> 
					 true;
				(_) -> false
			 end,
	
	Sort = fun
			  (Elem1, Elem2) -> Elem1#car_position.exit_t > Elem2#car_position.exit_t
		   end,

	Slist = lists:sort(Sort, lists:filter(Filter, Q)),
	
	if
		length(Slist) >= Index -> lists:nth(Index, Slist);
		true -> null
	end.
	



%%
%% Local Functions
%%

%% Calculates the time needed to cover Space with a starting
%% speed of Speed and an ending speed of MaxSpeed. Amin is the 
%% max deceleration of brakes (always negative) while Amax is 
%% the maximum acceleration engine can supply. 

calculate_time(Space, Speed, MaxSpeed, Amin, Amax) ->
	T1 = 2*Space / (Speed + MaxSpeed),
	A = (MaxSpeed - Speed)/T1,
	if
		A < Amin -> crash; %% TODO l'auto sbara
		A > Amax -> (math:sqrt(math:pow(Speed,2) + 8*Amax*Space) - Speed)/(2*Amax); 
		true -> T1
	end.
