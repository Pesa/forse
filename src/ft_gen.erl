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
-module(ft_gen).

%%%-----------------------------------------------------------------
%%% Fault-tolerant version of the gen module.
%%%-----------------------------------------------------------------

-export([start_link/4, start_link/5,
		 debug_options/1,
		 call/3, call/4,
		 reply/2]).

-export([init_it/6, init_it/7]).

-define(default_timeout, 5000).

-type emgr_name()	:: {'local', atom()} | {'global', term()}.
-type start_ret()	:: {'ok', pid()} | 'ignore' | {'error', term()}.
-type debug_flag()	:: 'trace' | 'log' | 'statistics' | 'debug'
					 | {'logfile', string()}.
-type option()		:: {'timeout', timeout()}
					 | {'debug', [debug_flag()]}
					 | {'spawn_opt', [proc_lib:spawn_option()]}.
-type options()		:: [option()].


%%-----------------------------------------------------------------
%% Starts a generic process.
%% start(GenMod, LinkP, Mod, Args, Options)
%% start(GenMod, LinkP, Name, Mod, Args, Options)
%%    GenMod = atom(), callback module implementing the 'real' fsm
%%    Name = {local, atom()} | {global, term()}
%%    Args = term(), init arguments (to Mod:init/1)
%%    Options = [{timeout, Timeout} | {debug, [Flag]} | {spawn_opt, OptionList}]
%%      Flag = trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} | ignore |{error, Reason} |
%%          {error, {already_started, Pid}} |
%%    The 'already_started' is returned only if Name is given 
%%-----------------------------------------------------------------

-spec start_link(module(), emgr_name(), module(), term(), options()) ->
	start_ret().

start_link(GenMod, Name, Mod, Args, Options) ->
	case where(Name) of
		undefined ->
			do_spawn(GenMod, Name, Mod, Args, Options);
		Pid ->
			{error, {already_started, Pid}}
	end.

-spec start_link(module(), module(), term(), options()) -> start_ret().

start_link(GenMod, Mod, Args, Options) ->
	% TODO: the Name-less variant is probably unused: check and remove.
	do_spawn(GenMod, Mod, Args, Options).

%%-----------------------------------------------------------------
%% Spawns the process and links to it, possibly at another node.
%%-----------------------------------------------------------------
do_spawn(GenMod, Mod, Args, Options) ->
	% TODO: the Name-less variant is probably unused: check and remove.
	Node = choose_node(),
	Pid = proc_lib:spawn_opt(Node, ?MODULE, init_it,
							 [GenMod, self(), self(), Mod, Args, Options],
							 [link]),
	sync_wait(Pid).

do_spawn(GenMod, Name, Mod, Args, Options) ->
	Node = choose_node(),
	Pid = proc_lib:spawn_opt(Node, ?MODULE, init_it,
							 [GenMod, self(), self(), Name, Mod, Args, Options],
							 [link]),
	sync_wait(Pid).

%%-----------------------------------------------------------------
%% Chooses a node among the alive ones where the process
%% must be (re-)spawned.
%%-----------------------------------------------------------------
choose_node() ->
	% TODO: implement me!
	todo.

%%-----------------------------------------------------------------
%% Waits for an ack message from the spawned process or returns
%% an error if the process died. Adapted from proc_lib:sync_wait/2.
%%-----------------------------------------------------------------
sync_wait(Pid) ->
	receive
		{ack, Pid, Return} ->
			Return;
		{'EXIT', Pid, Reason} ->
			{error, Reason}
	end.

%%-----------------------------------------------------------------
%% Initiate the new process.
%% Register the name using the Rfunc function
%% Calls the Mod:init/Args function.
%% Finally an acknowledge is sent to Parent and the main
%% loop is entered.
%%-----------------------------------------------------------------
init_it(GenMod, Starter, Parent, Mod, Args, Options) ->
	init_it2(GenMod, Starter, Parent, self(), Mod, Args, Options).

init_it(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
	case name_register(Name) of
		true ->
			init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options);
		{false, Pid} ->
			proc_lib:init_ack(Starter, {error, {already_started, Pid}})
	end.

init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
	GenMod:init_it(Starter, Parent, Name, Mod, Args, Options).

%%-----------------------------------------------------------------
%% Makes a synchronous call to a generic process.
%% Request is sent to the Pid, and the response must be
%% {Tag, _, Reply}.
%%-----------------------------------------------------------------
call(Process, Label, Request) -> 
	call(Process, Label, Request, ?default_timeout).

%% Local or remote by pid
call(Pid, Label, Request, Timeout) 
  when is_pid(Pid), Timeout =:= infinity;
	   is_pid(Pid), is_integer(Timeout), Timeout >= 0 ->
	do_call(Pid, Label, Request, Timeout);
%% Local by name
call(Name, Label, Request, Timeout) 
  when is_atom(Name), Timeout =:= infinity;
	   is_atom(Name), is_integer(Timeout), Timeout >= 0 ->
	case whereis(Name) of
		Pid when is_pid(Pid) ->
			do_call(Pid, Label, Request, Timeout);
		undefined ->
			exit(noproc)
	end;
%% Global by name
call({global, _Name}=Process, Label, Request, Timeout)
  when Timeout =:= infinity;
	   is_integer(Timeout), Timeout >= 0 ->
	case where(Process) of
		Pid when is_pid(Pid) ->
			Node = node(Pid),
			try do_call(Pid, Label, Request, Timeout)
			catch
				exit:{nodedown, Node} ->
					%% A nodedown not yet detected by global,
					%% pretend that it was.
					exit(noproc)
			end;
		undefined ->
			exit(noproc)
	end;
%% Local by name in disguise
call({Name, Node}, Label, Request, Timeout)
  when Node =:= node(), Timeout =:= infinity;
	   Node =:= node(), is_integer(Timeout), Timeout >= 0 ->
	call(Name, Label, Request, Timeout);
%% Remote by name
call({_Name, Node}=Process, Label, Request, Timeout)
  when is_atom(Node), Timeout =:= infinity;
	   is_atom(Node), is_integer(Timeout), Timeout >= 0 ->
	if
		node() =:= nonode@nohost ->
			exit({nodedown, Node});
		true ->
			do_call(Process, Label, Request, Timeout)
	end.

do_call(Process, Label, Request, Timeout) ->
	%% We trust the arguments to be correct, i.e
	%% Process is either a local or remote pid,
	%% or a {Name, Node} tuple (of atoms) and in this 
	%% case this node (node()) _is_ distributed and Node =/= node().
	Node = case Process of
			   {_S, N} when is_atom(N) ->
				   N;
			   _ when is_pid(Process) ->
				   node(Process)
		   end,
	% TODO: check in mnesia if we already accomplished this call
	%		if so: return the cached response immediately;
	%		if not && this call has never been tried before:
	%			generate a new CallID and add a "Sending Msg" entry to the journal;
	%		otherwise: retry the same call using the saved CallID.
	try erlang:monitor(process, Process) of
		Mref ->
			%% If the monitor/2 call failed to set up a connection to a
			%% remote node, we don't want the '!' operator to attempt
			%% to set up the connection again. (If the monitor/2 call
			%% failed due to an expired timeout, '!' too would probably
			%% have to wait for the timeout to expire.) Therefore,
			%% use erlang:send/3 with the 'noconnect' option so that it
			%% will fail immediately if there is no connection to the
			%% remote node.
			catch erlang:send(Process, {Label, {self(), Mref}, Request},
							  [noconnect]),
			wait_resp_mon(Node, Mref, Timeout)
	catch
		error:_ ->
			%% Node (C/Java?) is not supporting the monitor.
			%% The other possible case -- this node is not distributed
			%% -- should have been handled earlier.
			%% Do the best possible with monitor_node/2.
			%% This code may hang indefinitely if the Process 
			%% does not exist. It is only used for featureweak remote nodes.
			monitor_node(Node, true),
			receive
				{nodedown, Node} -> 
					monitor_node(Node, false),
					exit({nodedown, Node})
				after 0 -> 
					Tag = make_ref(),
					Process ! {Label, {self(), Tag}, Request},
					wait_resp(Node, Tag, Timeout)
			end
	end.

wait_resp_mon(Node, Mref, Timeout) ->
	receive
		{Mref, Reply} ->
			erlang:demonitor(Mref, [flush]),
			% TODO: log "Reply OK" to the journal
			{ok, Reply};
		{'DOWN', Mref, _, _, noconnection} ->
			exit({nodedown, Node});
		{'DOWN', Mref, _, _, Reason} ->
			exit(Reason)
		after Timeout ->
			erlang:demonitor(Mref, [flush]),
			exit(timeout)
	end.

wait_resp(Node, Tag, Timeout) ->
	receive
		{Tag, Reply} ->
			monitor_node(Node, false),
			% TODO: log "Reply OK" to the journal
			{ok, Reply};
		{nodedown, Node} ->
			monitor_node(Node, false),
			exit({nodedown, Node})
		after Timeout ->
			monitor_node(Node, false),
			exit(timeout)
	end.

%%
%% Send a reply to the client.
%%
reply({To, Tag}, Reply) ->
	% TODO: what's this?
	Msg = {Tag, Reply},
	try To ! Msg catch _:_ -> Msg end.

%%%-----------------------------------------------------------------
%%%  Misc. functions.
%%%-----------------------------------------------------------------
where({global, Name}) -> global:safe_whereis_name(Name);
where({local, Name})  -> whereis(Name).

name_register({local, Name} = LN) ->
	try register(Name, self()) of
		true -> true
	catch
		error:_ ->
			{false, where(LN)}
	end;
name_register({global, Name} = GN) ->
	case global:register_name(Name, self()) of
		yes -> true;
		no -> {false, where(GN)}
	end.

opt(Op, [{Op, Value}|_]) ->
	{ok, Value};
opt(Op, [_|Options]) ->
	opt(Op, Options);
opt(_, []) ->
	false.

debug_options(Opts) ->
	case opt(debug, Opts) of
		{ok, Options} -> sys:debug_options(Options);
		_ -> []
	end.
