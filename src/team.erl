-module(team).

-behaviour(gen_server).

%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("db_schema.hrl").

-define(TEAM_NAME(Id), {global, utils:build_id_atom("team_", Id)}).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Config) when is_list(Config) ->
	{id, TeamId} = lists:keyfind(id, 1, Config),
	gen_server:start_link(?TEAM_NAME(TeamId), ?MODULE, Config, []).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Config) ->
	CarType = lists:foldl(
				fun({id, Id}, CT) ->
						CT#car_type{id = Id};
				   ({team_name, Name}, CT) ->
						CT#car_type{team_name = Name};
				   ({brake, Brake}, CT) ->
						CT#car_type{brake = Brake};
				   ({power, Power}, CT) ->
						CT#car_type{power = Power};
				   ({weight, Weight}, CT) ->
						CT#car_type{weight = Weight};
				   (_, CT) -> CT
				end, #car_type{}, Config),
	T = fun() ->
				mnesia:write(CarType)
		end,
	{atomic, ok} = mnesia:sync_transaction(T),
	{ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%% --------------------------------------------------------------------
%% Function: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
