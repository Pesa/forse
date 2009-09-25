-module(utils).

%% Exported functions
-export([build_id_atom/2,
		 get_setting/1,
		 mnesia_read/2]).

-include("db_schema.hrl").

%%% ==========================
%%%  Common utility functions
%%% ==========================

% Returns an atom created by appending Id to Prefix.
build_id_atom(Prefix, Id) when is_list(Prefix),
							   is_integer(Id) ->
	list_to_atom(Prefix ++ integer_to_list(Id)).

% Returns the value associated with the setting Key.
get_setting(Key) ->
	H = utils:mnesia_read(setting, Key),
	H#setting.value.

% Wraps a mnesia:read/2 in a transaction context.
% If the transaction fails, an exception is raised.
mnesia_read(Tab, Key) ->
	F = fun() ->
				[R] = mnesia:read(Tab, Key),
				R
		end,
	case mnesia:transaction(F) of
		{atomic, Res} -> Res
	end.
