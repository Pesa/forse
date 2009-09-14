-module(physics).

%%
%% Include files
%%

-include("db_schema.hrl").
-include("config.hrl").

%%
%% Exported Functions
%%
-export([simulate/2,
		 preelaborate/1]).

%%
%% API Functions
%%


%%TODO Dove metto l'inclinazione della pista? nel calcolo di Amax Amin o altrove?

%% Calculates the time needed by Car to cover the next segment
%% exiting from it in ExitLane lane.
%% Pilot: id in pilot table
%% ExitLane: guess...

simulate(Pilot, ExitLane) ->
	[P | _Tail] = mnesia:read(pilot, Pilot),
	Sgm = next_segment(P#pilot.segment),
	[S2 | _] = mnesia:read(?TRACK_TAB, P#pilot.segment),
	Car = find_pilot(Pilot, S2#segment.queued_cars),
	EnterLane = Car#car_position.exit_lane,
	EnterTime = Car#car_position.exit_t,
	
	[S | _] = mnesia:read(?TRACK_TAB, Sgm),
	Space = S#segment.length,
	EnterSpeed = Car#car_position.speed,
	
	[Bound | _] = mnesia:read(preelab_tab_name(Pilot), Sgm),
	MaxExitSpeed = Bound#speed_bound.bound,
	%%TODO mettere a posto sti valori
	Amin = 0,
	Amax = 0,
	
	simulate_rec(Sgm, EnterLane, ExitLane, EnterTime, 1,
				 Space, EnterSpeed, MaxExitSpeed, Amin, Amax).

%% Calculates max speed the car with id equal to Pilot can have
%% in each segment of the track.

preelaborate(Pilot) -> 
	[P | _] = mnesia:read(pilot, Pilot),
	[C | _] = mnesia:read(car_type, P#pilot.team_name),
	
	%% la tabella con la preelaborazione si chiamerà preelab_tab_name(Pilot)
	%% con righe di tipo speed_bound
	%% TODO controlla se esiste la tabella, altrimenti la crea
	%% preelabora
	
	ok.

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

%%TODO mettere a posto gli args.. sono troppi....

simulate_rec(Sgm, EnterLane, ExitLane, EnterTime, Index, 
			 Space, EnterSpeed, MaxExitSpeed, Amin, Amax) ->
	G = if
			EnterLane == ExitLane -> 0;
			true -> ?LANE_CHANGE_TIME
		end,
	
	K = get_car_ahead(Sgm, ExitLane, Index),
	
	%%TODO trattare il caso in cui calculate_time ritorni crash...
	
	case K of
		null -> 
			G + calculate_time(Space, EnterSpeed, MaxExitSpeed, Amin, Amax);
		_ when EnterLane == K#car_position.enter_lane ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			G + calculate_time(Space, EnterSpeed, MaxSpeed, Amin, Amax);
		_ when EnterTime + G > K#car_position.enter_t + ?LANE_CHANGE_TIME ->
			MaxSpeed = lists:min([K#car_position.speed, MaxExitSpeed]),
			G + calculate_time(Space, EnterSpeed, MaxSpeed, Amin, Amax);
		_ ->
			simulate_rec(Sgm, EnterLane, ExitLane, EnterTime, Index + 1,
						 Space, EnterSpeed, MaxExitSpeed, Amin, Amax)
	end.


%% Given a segment's id it calculates next segment's id
%% TODO
next_segment(Id) -> 0.

%%Extract car_position with car_id == Pilot from Queue
find_pilot(Pilot, [H | T]) ->
	if 
		Pilot == H#car_position.car_id -> H;
		true -> find_pilot(Pilot, T)
	end;
find_pilot(_, []) ->
	null.

%% Returns the name of the preelaboration table
%% associated with Pilot
preelab_tab_name(Pilot) ->
	list_to_atom("pilot_" ++ integer_to_list(Pilot)).
	