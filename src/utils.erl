-module(utils).

%% Exported Functions
-export([mnesia_read/2]).


%%% ==========================
%%%  Common utility functions
%%% ==========================

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
