-module(utils).

%% Exported functions
-export([build_id_atom/2,
		 get_setting/1,
		 set_setting/2,
		 mnesia_read/2,
		 table_exists/1]).

-include("db_schema.hrl").

%%% ==========================
%%%  Common utility functions
%%% ==========================

%% Returns an atom which is the result of appending Suffix to Prefix.
build_id_atom(Prefix, Suffix) when is_list(Prefix), is_atom(Suffix) ->
	list_to_atom(Prefix ++ atom_to_list(Suffix));
build_id_atom(Prefix, Suffix) when is_list(Prefix), is_integer(Suffix) ->
	list_to_atom(Prefix ++ integer_to_list(Suffix));
build_id_atom(Prefix, Suffix) when is_list(Prefix), is_list(Suffix) ->
	list_to_atom(Prefix ++ Suffix).

%% Returns the value associated with the setting Key.
get_setting(Key) ->
	H = utils:mnesia_read(setting, Key),
	H#setting.value.

%% Sets the value of the setting Key.
set_setting(Key, Value) ->
	S = #setting{key = Key, value = Value},
	T = fun() ->
				mnesia:write(setting, S, write)
		end,
	{atomic, ok} = mnesia:sync_transaction(T).

%% Wraps a mnesia:read/2 in a transaction context.
%% If the transaction fails, an exception is raised.
mnesia_read(Tab, Key) ->
	T = fun() ->
				[R] = mnesia:read(Tab, Key),
				R
		end,
	{atomic, Res} = mnesia:transaction(T),
	Res.

%% Returns true if a mnesia table named TableName
%% exists, false otherwise.
table_exists(TableName) ->
	lists:member(TableName, mnesia:system_info(tables)).
