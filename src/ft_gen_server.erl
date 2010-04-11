%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(ft_gen_server).

%%% ------------------------------------------------------------
%%%
%%% Fault-tolerant version of the standard gen_server module.
%%%
%%% The user module should export:
%%%
%%%   init(Args)  
%%%     ==> {ok, State}
%%%         {ok, State, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   handle_call(Msg, {From, Tag}, State)
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}  
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_cast(Msg, State)
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State} 
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, State) Info is e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State} 
%%%              Reason = normal | shutdown | Term, terminate(State) is called
%%%
%%%   terminate(Reason, State) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> ok
%%%
%%%
%%% The work flow (of the server) can be described as follows:
%%%
%%%   User module                          Generic
%%%   -----------                          -------
%%%     start            ----->             start
%%%     init             <-----              .
%%%
%%%                                         loop
%%%     handle_call      <-----              .
%%%                      ----->             reply
%%%
%%%     handle_cast      <-----              .
%%%
%%%     handle_info      <-----              .
%%%
%%%     terminate        <-----              .
%%%
%%%                      ----->             reply
%%%
%%%
%%% ---------------------------------------------------

%% API
-export([start_link/3, start_link/4,
		 atomic_call/2, atomic_call/3,
		 atomic_cast/2,
		 reply/2,
		 multi_call/2, multi_call/3, multi_call/4,
		 wake_hib/5]).

-export([behaviour_info/1]).

%% System exports
-export([system_continue/3,
		 system_terminate/4,
		 system_code_change/4,
		 format_status/2]).

%% Internal exports
-export([init_it/6, print_event/3]).

-import(error_logger, [format/2]).

-define(state_table(ProcName), utils:build_id_atom("ft_state_", ProcName)).


%%%=========================================================================
%%%  API
%%%=========================================================================

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
	[{init,1},{handle_call,3},{handle_cast,2},{handle_info,2},
	 {terminate,2},{code_change,3}];
behaviour_info(_Other) ->
	undefined.

%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, atom()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------
start_link(Mod, Args, Options) ->
	% TODO: the Name-less variant is probably unused: check and remove.
	ft_gen:start_link(?MODULE, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
	ft_gen:start_link(?MODULE, Name, Mod, Args, Options).

%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on timeouts) ?).
%% ----------------------------------------------------------------- 
atomic_call(Name, Request) ->
	case catch ft_gen:call(Name, '$atomic_gen_call', Request) of
		{ok, Res} ->
			Res;
		{'EXIT', Reason} ->
			exit({Reason, {?MODULE, call, [Name, Request]}})
	end.

atomic_call(Name, Request, Timeout) ->
	case catch ft_gen:call(Name, '$atomic_gen_call', Request, Timeout) of
		{ok, Res} ->
			Res;
		{'EXIT', Reason} ->
			exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
	end.

%% -----------------------------------------------------------------
%% Make a cast to a generic server.
%% -----------------------------------------------------------------
atomic_cast({global, Name}, Request) ->
	% TODO: send() isn't reliable enough, implement our protocol
	catch global:send(Name, cast_msg(Request)),
	ok;
atomic_cast({Name, Node} = Dest, Request) when is_atom(Name), is_atom(Node) ->
	do_cast(Dest, Request);
atomic_cast(Dest, Request) when is_atom(Dest) ->
	do_cast(Dest, Request);
atomic_cast(Dest, Request) when is_pid(Dest) ->
	do_cast(Dest, Request).

do_cast(Dest, Request) -> 
	do_send(Dest, cast_msg(Request)),
	ok.

cast_msg(Request) ->
	{'$atomic_gen_cast', Request}.

%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------
reply({To, Tag}, Reply) ->
	% TODO: convert into a call/atomic_cast
	catch To ! {Tag, Reply}.

%%% -----------------------------------------------------------------
%%% Make a call to servers at several nodes.
%%% Returns: {[Replies],[BadNodes]}
%%% A Timeout can be given
%%% 
%%% A middleman process is used in case late answers arrives after
%%% the timeout. If they would be allowed to glog the callers message
%%% queue, it would probably become confused. Late answers will 
%%% now arrive to the terminated middleman and so be discarded.
%%% -----------------------------------------------------------------
% TODO: remove?
multi_call(Name, Req)
  when is_atom(Name) ->
	do_multi_call([node() | nodes()], Name, Req, infinity).

multi_call(Nodes, Name, Req) 
  when is_list(Nodes), is_atom(Name) ->
	do_multi_call(Nodes, Name, Req, infinity).

multi_call(Nodes, Name, Req, infinity) ->
	do_multi_call(Nodes, Name, Req, infinity);
multi_call(Nodes, Name, Req, Timeout) 
  when is_list(Nodes), is_atom(Name), is_integer(Timeout), Timeout >= 0 ->
	do_multi_call(Nodes, Name, Req, Timeout).


%%%========================================================================
%%% gen callback functions
%%%========================================================================

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
	init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
	Name = name(Name0),
	Debug = debug_options(Name, Options),
	Type = case utils:table_exists(?state_table(Name)) of
			   true -> {failover, read_state(Name)};
			   false -> normal
		   end,
	case catch Mod:init(Type, Args) of
		% TODO: save the (new) state
		{ok, State} ->
			proc_lib:init_ack(Starter, {ok, self()}), 	    
			loop(Parent, Name, State, Mod, infinity, Debug);
		{ok, State, Timeout} ->
			proc_lib:init_ack(Starter, {ok, self()}), 	    
			loop(Parent, Name, State, Mod, Timeout, Debug);
		{stop, Reason} ->
			%% For consistency, we must make sure that the
			%% registered name (if any) is unregistered before
			%% the parent process is notified about the failure.
			%% (Otherwise, the parent process could get
			%% an 'already_started' error if it immediately
			%% tried starting the process again.)
			unregister_name(Name0),
			proc_lib:init_ack(Starter, {error, Reason}),
			exit(Reason);
		ignore ->
			unregister_name(Name0),
			proc_lib:init_ack(Starter, ignore),
			exit(normal);
		{'EXIT', Reason} ->
			unregister_name(Name0),
			proc_lib:init_ack(Starter, {error, Reason}),
			exit(Reason);
		Else ->
			Error = {bad_return_value, Else},
			proc_lib:init_ack(Starter, {error, Error}),
			exit(Error)
	end.

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

unregister_name({local,Name}) ->
	_ = (catch unregister(Name));
unregister_name({global,Name}) ->
	_ = global:unregister_name(Name);
unregister_name(Pid) when is_pid(Pid) ->
	Pid.


%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------
loop(Parent, Name, State, Mod, hibernate, Debug) ->
	proc_lib:hibernate(?MODULE, wake_hib, [Parent, Name, State, Mod, Debug]);
loop(Parent, Name, State, Mod, Time, Debug) ->
	Msg = receive
			  Input ->
				  Input
			  after Time ->
				  timeout
		  end,
	decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, false).

wake_hib(Parent, Name, State, Mod, Debug) ->
	Msg = receive
			  Input ->
				  Input
		  end,
	decode_msg(Msg, Parent, Name, State, Mod, hibernate, Debug, true).

decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, Hib) ->
	case Msg of
		{system, From, Req} ->
			sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
								  [Name, State, Mod, Time], Hib);
		{'EXIT', Parent, Reason} ->
			terminate(Reason, Name, Msg, Mod, State, Debug);
		_Msg when Debug =:= [] ->
			handle_msg(Msg, Parent, Name, State, Mod);
		_Msg ->
			Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
									  Name, {in, Msg}),
			handle_msg(Msg, Parent, Name, State, Mod, Debug1)
	end.

%%% ---------------------------------------------------
%%% Send/receive functions
%%% ---------------------------------------------------
do_send(Dest, Msg) ->
	% TODO: send() isn't reliable enough
	case catch erlang:send(Dest, Msg, [noconnect]) of
		noconnect ->
			% TODO: this is async, which is bad for us
			spawn(erlang, send, [Dest, Msg]);
		Other ->
			Other
	end.

do_multi_call(Nodes, Name, Req, infinity) ->
	Tag = make_ref(),
	Monitors = send_nodes(Nodes, Name, Tag, Req),
	rec_nodes(Tag, Monitors, Name, undefined);
do_multi_call(Nodes, Name, Req, Timeout) ->
	Tag = make_ref(),
	Caller = self(),
	Receiver =
		spawn(
		  fun() ->
				  %% Middleman process. Should be unsensitive to regular
				  %% exit signals. The sychronization is needed in case
				  %% the receiver would exit before the caller started
				  %% the monitor.
				  process_flag(trap_exit, true),
				  Mref = erlang:monitor(process, Caller),
				  receive
					  {Caller,Tag} ->
						  Monitors = send_nodes(Nodes, Name, Tag, Req),
						  TimerId = erlang:start_timer(Timeout, self(), ok),
						  Result = rec_nodes(Tag, Monitors, Name, TimerId),
						  exit({self(),Tag,Result});
					  {'DOWN',Mref,_,_,_} ->
						  %% Caller died before sending us the go-ahead.
						  %% Give up silently.
						  exit(normal)
				  end
		  end),
	Mref = erlang:monitor(process, Receiver),
	Receiver ! {self(),Tag},
	receive
		{'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
			Result;
		{'DOWN',Mref,_,_,Reason} ->
			%% The middleman code failed. Or someone did 
			%% exit(_, kill) on the middleman process => Reason==killed
			exit(Reason)
	end.

send_nodes(Nodes, Name, Tag, Req) ->
	send_nodes(Nodes, Name, Tag, Req, []).

send_nodes([Node|Tail], Name, Tag, Req, Monitors)
  when is_atom(Node) ->
	Monitor = start_monitor(Node, Name),
	%% Handle non-existing names in rec_nodes.
	catch {Name, Node} ! {'$atomic_gen_call', {self(), {Tag, Node}}, Req},
	send_nodes(Tail, Name, Tag, Req, [Monitor | Monitors]);
send_nodes([_Node|Tail], Name, Tag, Req, Monitors) ->
	%% Skip non-atom Node
	send_nodes(Tail, Name, Tag, Req, Monitors);
send_nodes([], _Name, _Tag, _Req, Monitors) -> 
	Monitors.

%% Against old nodes:
%% If no reply has been delivered within 2 secs. (per node) check that
%% the server really exists and wait for ever for the answer.
%%
%% Against contemporary nodes:
%% Wait for reply, server 'DOWN', or timeout from TimerId.

rec_nodes(Tag, Nodes, Name, TimerId) -> 
	rec_nodes(Tag, Nodes, Name, [], [], 2000, TimerId).

rec_nodes(Tag, [{N,R}|Tail], Name, Badnodes, Replies, Time, TimerId ) ->
	receive
		{'DOWN', R, _, _, _} ->
			rec_nodes(Tag, Tail, Name, [N|Badnodes], Replies, Time, TimerId);
		{{Tag, N}, Reply} ->  %% Tag is bound !!!
			unmonitor(R), 
			rec_nodes(Tag, Tail, Name, Badnodes, 
					  [{N,Reply}|Replies], Time, TimerId);
		{timeout, TimerId, _} ->	
			unmonitor(R),
			%% Collect all replies that already have arrived
			rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
	end;
rec_nodes(Tag, [N|Tail], Name, Badnodes, Replies, Time, TimerId) ->
	%% R6 node
	receive
		{nodedown, N} ->
			monitor_node(N, false),
			rec_nodes(Tag, Tail, Name, [N|Badnodes], Replies, 2000, TimerId);
		{{Tag, N}, Reply} ->  %% Tag is bound !!!
			receive {nodedown, N} -> ok after 0 -> ok end,
			monitor_node(N, false),
			rec_nodes(Tag, Tail, Name, Badnodes,
					  [{N,Reply}|Replies], 2000, TimerId);
		{timeout, TimerId, _} ->	
			receive {nodedown, N} -> ok after 0 -> ok end,
			monitor_node(N, false),
			%% Collect all replies that already have arrived
			rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies)
		after Time ->
			case rpc:call(N, erlang, whereis, [Name]) of
				Pid when is_pid(Pid) -> % It exists try again.
					rec_nodes(Tag, [N|Tail], Name, Badnodes,
							  Replies, infinity, TimerId);
				_ -> % badnode
					receive {nodedown, N} -> ok after 0 -> ok end,
					monitor_node(N, false),
					rec_nodes(Tag, Tail, Name, [N|Badnodes],
							  Replies, 2000, TimerId)
			end
	end;
rec_nodes(_, [], _, Badnodes, Replies, _, TimerId) ->
	case catch erlang:cancel_timer(TimerId) of
		false ->  % It has already sent it's message
			receive
				{timeout, TimerId, _} -> ok
				after 0 ->
					ok
			end;
		_ -> % Timer was cancelled, or TimerId was 'undefined'
			ok
	end,
	{Replies, Badnodes}.

%% Collect all replies that already have arrived
rec_nodes_rest(Tag, [{N,R}|Tail], Name, Badnodes, Replies) ->
	receive
		{'DOWN', R, _, _, _} ->
			rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies);
		{{Tag, N}, Reply} -> %% Tag is bound !!!
			unmonitor(R),
			rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N,Reply}|Replies])
		after 0 ->
			unmonitor(R),
			rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
	end;
rec_nodes_rest(Tag, [N|Tail], Name, Badnodes, Replies) ->
	%% R6 node
	receive
		{nodedown, N} ->
			monitor_node(N, false),
			rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies);
		{{Tag, N}, Reply} ->  %% Tag is bound !!!
			receive {nodedown, N} -> ok after 0 -> ok end,
			monitor_node(N, false),
			rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N,Reply}|Replies])
		after 0 ->
			receive {nodedown, N} -> ok after 0 -> ok end,
			monitor_node(N, false),
			rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
	end;
rec_nodes_rest(_Tag, [], _Name, Badnodes, Replies) ->
	{Replies, Badnodes}.


%%% ---------------------------------------------------
%%% Monitor functions
%%% ---------------------------------------------------

start_monitor(Node, Name) when is_atom(Node), is_atom(Name) ->
	if node() =:= nonode@nohost, Node =/= nonode@nohost ->
		   Ref = make_ref(),
		   self() ! {'DOWN', Ref, process, {Name, Node}, noconnection},
		   {Node, Ref};
	   true ->
		   case catch erlang:monitor(process, {Name, Node}) of
			   {'EXIT', _} ->
				   %% Remote node is R6
				   monitor_node(Node, true),
				   Node;
			   Ref when is_reference(Ref) ->
				   {Node, Ref}
		   end
	end.

%% Cancels a monitor started with Ref=erlang:monitor(_, _).
unmonitor(Ref) when is_reference(Ref) ->
	erlang:demonitor(Ref),
	receive
		{'DOWN', Ref, _, _, _} ->
			true
		after 0 ->
			true
	end.

%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------

dispatch({'$atomic_gen_cast', Msg}, Mod, State) ->
	Mod:handle_cast(Msg, State);
dispatch(Info, Mod, State) ->
	Mod:handle_info(Info, State).

handle_msg({'$atomic_gen_call', From, Msg}, Parent, Name, State, Mod) ->
	case catch Mod:handle_call(Msg, From, State) of
		% TODO: save the new state in mnesia and
		%		add a "Msg Processed" journal log entry.
		{reply, Reply, NState} ->
			reply(From, Reply),
			loop(Parent, Name, NState, Mod, infinity, []);
		{reply, Reply, NState, Time1} ->
			reply(From, Reply),
			loop(Parent, Name, NState, Mod, Time1, []);
		{noreply, NState} ->
			loop(Parent, Name, NState, Mod, infinity, []);
		{noreply, NState, Time1} ->
			loop(Parent, Name, NState, Mod, Time1, []);
		{stop, Reason, Reply, NState} ->
			{'EXIT', R} = 
				(catch terminate(Reason, Name, Msg, Mod, NState, [])),
			reply(From, Reply),
			exit(R);
		Other -> handle_common_reply(Other, Parent, Name, Msg, Mod, State)
	end;
handle_msg(Msg, Parent, Name, State, Mod) ->
	Reply = (catch dispatch(Msg, Mod, State)),
	handle_common_reply(Reply, Parent, Name, Msg, Mod, State).

handle_msg({'$atomic_gen_call', From, Msg}, Parent, Name, State, Mod, Debug) ->
	case catch Mod:handle_call(Msg, From, State) of
		% TODO: idem
		{reply, Reply, NState} ->
			Debug1 = reply(Name, From, Reply, NState, Debug),
			loop(Parent, Name, NState, Mod, infinity, Debug1);
		{reply, Reply, NState, Time1} ->
			Debug1 = reply(Name, From, Reply, NState, Debug),
			loop(Parent, Name, NState, Mod, Time1, Debug1);
		{noreply, NState} ->
			Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, Name,
									  {noreply, NState}),
			loop(Parent, Name, NState, Mod, infinity, Debug1);
		{noreply, NState, Time1} ->
			Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, Name,
									  {noreply, NState}),
			loop(Parent, Name, NState, Mod, Time1, Debug1);
		{stop, Reason, Reply, NState} ->
			{'EXIT', R} = 
				(catch terminate(Reason, Name, Msg, Mod, NState, Debug)),
			reply(Name, From, Reply, NState, Debug),
			exit(R);
		Other ->
			handle_common_reply(Other, Parent, Name, Msg, Mod, State, Debug)
	end;
handle_msg(Msg, Parent, Name, State, Mod, Debug) ->
	Reply = (catch dispatch(Msg, Mod, State)),
	handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug).

handle_common_reply(Reply, Parent, Name, Msg, Mod, State) ->
	case Reply of
		% TODO: idem
		{noreply, NState} ->
			loop(Parent, Name, NState, Mod, infinity, []);
		{noreply, NState, Time1} ->
			loop(Parent, Name, NState, Mod, Time1, []);
		{stop, Reason, NState} ->
			terminate(Reason, Name, Msg, Mod, NState, []);
		{'EXIT', What} ->
			terminate(What, Name, Msg, Mod, State, []);
		_ ->
			terminate({bad_return_value, Reply}, Name, Msg, Mod, State, [])
	end.

handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug) ->
	case Reply of
		% TODO: idem
		{noreply, NState} ->
			Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, Name,
									  {noreply, NState}),
			loop(Parent, Name, NState, Mod, infinity, Debug1);
		{noreply, NState, Time1} ->
			Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, Name,
									  {noreply, NState}),
			loop(Parent, Name, NState, Mod, Time1, Debug1);
		{stop, Reason, NState} ->
			terminate(Reason, Name, Msg, Mod, NState, Debug);
		{'EXIT', What} ->
			terminate(What, Name, Msg, Mod, State, Debug);
		_ ->
			terminate({bad_return_value, Reply}, Name, Msg, Mod, State, Debug)
	end.

reply(Name, {To, Tag}, Reply, State, Debug) ->
	reply({To, Tag}, Reply),
	sys:handle_debug(Debug, {?MODULE, print_event}, Name, 
					 {out, Reply, To, State}).


%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------

system_continue(Parent, Debug, [Name, State, Mod, Time]) ->
	loop(Parent, Name, State, Mod, Time, Debug).

-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time]) ->
	terminate(Reason, Name, [], Mod, State, Debug).

system_code_change([Name, State, Mod, Time], _Module, OldVsn, Extra) ->
	case catch Mod:code_change(OldVsn, State, Extra) of
		{ok, NewState} -> {ok, [Name, NewState, Mod, Time]};
		Else -> Else
	end.


%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
	case Msg of
		{'$atomic_gen_call', {From, _Tag}, Call} ->
			io:format(Dev, "*DBG* ~p got call ~p from ~w~n",
					  [Name, Call, From]);
		{'$atomic_gen_cast', Cast} ->
			io:format(Dev, "*DBG* ~p got cast ~p~n",
					  [Name, Cast]);
		_ ->
			io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
	end;
print_event(Dev, {out, Msg, To, State}, Name) ->
	io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n", 
			  [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
	io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
	io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

-spec terminate(term(), atom(), _, module(), term(), _) -> no_return().

terminate(Reason, Name, Msg, Mod, State, Debug) ->
	% TODO: togliere il callback terminate?
	case catch Mod:terminate(Reason, State) of
		{'EXIT', R} ->
			error_info(R, Name, Msg, State, Debug),
			exit(R);
		_ ->
			case Reason of
				normal ->
					exit_cleanup(Name, normal);
				shutdown ->
					exit_cleanup(Name, shutdown);
				{shutdown, takeover} = Shutdown ->
					% TODO: come si fa a ottenere questa Reason?
					%		exit(Pid, {shutdown, takeover})
					exit(Shutdown);
				{shutdown, _} = Shutdown ->
					exit_cleanup(Name, Shutdown);
				_ ->
					error_info(Reason, Name, Msg, State, Debug),
					exit(Reason)
			end
	end.

exit_cleanup(Name, Reason) ->
	mnesia:delete_table(?state_table(Name)),
	exit(Reason).

error_info(_Reason, application_controller, _Msg, _State, _Debug) ->
	%% OTP-5811 Don't send an error report if it's the system process
	%% application_controller which is terminating - let init take care
	%% of it instead
	ok;
error_info(Reason, Name, Msg, State, Debug) ->
	Reason1 = 
		case Reason of
			{undef,[{M,F,A}|MFAs]} ->
				case code:is_loaded(M) of
					false ->
						{'module could not be loaded',[{M,F,A}|MFAs]};
					_ ->
						case erlang:function_exported(M, F, length(A)) of
							true ->
								Reason;
							false ->
								{'function not exported',[{M,F,A}|MFAs]}
						end
				end;
			_ ->
				Reason
		end,
	format("** Generic server ~p terminating \n"
		   "** Last message in was ~p~n"
		   "** When Server state == ~p~n"
		   "** Reason for termination == ~n** ~p~n",
		   [Name, Msg, State, Reason1]),
	sys:print_log(Debug),
	ok.


%%% ---------------------------------------------------
%%% State handling functions.
%%% ---------------------------------------------------

read_state(ProcName) ->
	Tab = ?state_table(ProcName),
	{state, State} = utils:mnesia_read(Tab, state),
	State.

%write_state(ProcName, State) ->
%	Tab = ?state_table(ProcName),
%	T = fun() ->
%				mnesia:write(Tab, {state, State}, sticky_write)
%		end,
%	mnesia:activity(sync_transaction, T),
%	ok.


%%% ---------------------------------------------------
%%% Misc. functions.
%%% ---------------------------------------------------

opt(Op, [{Op, Value}|_]) ->
	{ok, Value};
opt(Op, [_|Options]) ->
	opt(Op, Options);
opt(_, []) ->
	false.

debug_options(Name, Opts) ->
	case opt(debug, Opts) of
		{ok, Options} -> dbg_options(Name, Options);
		_ -> dbg_options(Name, [])
	end.

dbg_options(Name, []) ->
	Opts = 
		case init:get_argument(generic_debug) of
			error ->
				[];
			_ ->
				[log, statistics]
		end,
	dbg_opts(Name, Opts);
dbg_options(Name, Opts) ->
	dbg_opts(Name, Opts).

dbg_opts(Name, Opts) ->
	case catch sys:debug_options(Opts) of
		{'EXIT',_} ->
			format("~p: ignoring erroneous debug options - ~p~n",
				   [Name, Opts]),
			[];
		Dbg ->
			Dbg
	end.


%%% ---------------------------------------------------
%%% Status information
%%% ---------------------------------------------------

format_status(Opt, StatusData) ->
	[PDict, SysState, Parent, Debug, [Name, State, Mod, _Time]] = StatusData,
	NameTag = if is_pid(Name) ->
					 pid_to_list(Name);
				 is_atom(Name) ->
					 Name
			  end,
	Header = lists:concat(["Status for generic server ", NameTag]),
	Log = sys:get_debug(log, Debug, []),
	Specfic = 
		case erlang:function_exported(Mod, format_status, 2) of
			true ->
				case catch Mod:format_status(Opt, [PDict, State]) of
					{'EXIT', _} -> [{data, [{"State", State}]}];
					Else -> Else
				end;
			_ ->
				[{data, [{"State", State}]}]
		end,
	[{header, Header},
	 {data, [{"Status", SysState},
			 {"Parent", Parent},
			 {"Logged events", Log}]} |
		 Specfic].
