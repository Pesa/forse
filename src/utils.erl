-module(utils).

%% Exported Functions
-export([build_id_atom/2,
		 mnesia_read/2]).


%%% ==========================
%%%  Common utility functions
%%% ==========================

% Returns an atom created by appending Id to Prefix.
build_id_atom(Prefix, Id) when is_list(Prefix),
							   is_integer(Id) ->
	list_to_atom(Prefix ++ integer_to_list(Id)).

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
