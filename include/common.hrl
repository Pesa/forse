-define(ERR(Msg), io:format("[~s] ERROR: ~p~n", [?MODULE_STRING, Msg])).
-define(WARN(Msg), io:format("[~s] WARNING: ~p~n", [?MODULE_STRING, Msg])).

-ifdef(debug).
-define(DBG(Msg), io:format("[~s] ~p~n", [?MODULE_STRING, Msg])).
-else.
-define(DBG(Msg), true).
-endif.
