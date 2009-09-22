-module(utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([mnesia_read/2]).

%%
%% API Functions
%%

%%% =======================
%%%  Common utility funs
%%% =======================

%% If transaction fails raises exception
mnesia_read(Tab, Key) ->
	Fun = fun() ->
				  [R] = mnesia:read(Tab, Key),
				  R
		  end,
	{T, Res} = mnesia:transaction(Fun),
	case T of
		%% if T == aborted raises an exception
		atomic -> Res
	end.



%%
%% Local Functions
%%

