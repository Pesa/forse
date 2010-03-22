-module(utils).

%% Exported functions
-export([build_id_atom/2,
		 digits/1,
		 get_setting/1,
		 set_setting/2,
		 mnesia_read/2,
		 table_exists/1]).

-include("db_schema.hrl").


%%% ==========================
%%%  Common utility functions
%%% ==========================


%% Returns an atom which is the result of appending Suffix to Prefix.
-spec build_id_atom(string(), atom() | non_neg_integer() | string()) -> atom().

build_id_atom(Prefix, Suffix) when is_list(Prefix), is_atom(Suffix) ->
	list_to_atom(Prefix ++ atom_to_list(Suffix));
build_id_atom(Prefix, Suffix) when is_list(Prefix), is_integer(Suffix) ->
	list_to_atom(Prefix ++ integer_to_list(Suffix));
build_id_atom(Prefix, Suffix) when is_list(Prefix), is_list(Suffix) ->
	list_to_atom(Prefix ++ Suffix).

%% Returns a string that accurately represents the given
%% integer or float using a conservative amount of digits.
-spec digits(number()) -> string().

digits(N) when is_integer(N) ->
	integer_to_list(N);
digits(0.0) ->
	"0.0";
digits(Float) ->
	{Frac, Exp} = frexp(Float),
	Exp1 = Exp - 53,
	Frac1 = trunc(abs(Frac) * (1 bsl 53)),
	[Place | Digits] = digits1(Float, Exp1, Frac1),
	R = insert_decimal(Place, [$0 + D || D <- Digits]),
	case Float < 0 of
		true ->
			[$- | R];
		_ ->
			R
	end.

%% Returns the value associated with the setting Key.
-spec get_setting(atom()) -> term().

get_setting(Key) ->
	H = utils:mnesia_read(setting, Key),
	H#setting.value.

%% Sets the value of the setting Key.
-spec set_setting(atom(), term()) -> 'ok'.

set_setting(Key, Value) ->
	S = #setting{key = Key, value = Value},
	T = fun() ->
				mnesia:write(S)
		end,
	{atomic, ok} = mnesia:sync_transaction(T),
	ok.

%% Wraps a mnesia:read/2 in a transaction context.
%% If the transaction fails, an exception is raised.
-spec mnesia_read(atom(), atom() | non_neg_integer()) -> term().

mnesia_read(Tab, Key) ->
	T = fun() ->
				mnesia:read(Tab, Key)
		end,
	{atomic, [Result]} = mnesia:transaction(T),
	Result.

%% Returns true if a mnesia table named TableName
%% exists, false otherwise.
-spec table_exists(atom()) -> boolean().

table_exists(TableName) ->
	lists:member(TableName, mnesia:system_info(tables)).


%% ------------------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------------------
%% The following code is taken from mochinum, a module of the MochiWeb project.
%% http://mochiweb.googlecode.com/svn/trunk/src/mochinum.erl
%% ------------------------------------------------------------------------------

%% IEEE 754 Float exponent bias
-define(FLOAT_BIAS, 1022).
-define(MIN_EXP, -1074).
-define(BIG_POW, 4503599627370496).

frexp(F) ->
	frexp1(unpack(F)).

frexp1({_Sign, 0, 0}) ->
	{0.0, 0};
frexp1({Sign, 0, Frac}) ->
	Exp = log2floor(Frac),
	<<Frac1:64/float>> = <<Sign:1, ?FLOAT_BIAS:11, (Frac-1):52>>,
	{Frac1, -(?FLOAT_BIAS) - 52 + Exp};
frexp1({Sign, Exp, Frac}) ->
	<<Frac1:64/float>> = <<Sign:1, ?FLOAT_BIAS:11, Frac:52>>,
	{Frac1, Exp - ?FLOAT_BIAS}.

unpack(Float) ->
	<<Sign:1, Exp:11, Frac:52>> = <<Float:64/float>>,
	{Sign, Exp, Frac}.

digits1(Float, Exp, Frac) ->
	Round = ((Frac band 1) =:= 0),
	case Exp >= 0 of
		true ->
			BExp = 1 bsl Exp,
			case (Frac /= ?BIG_POW) of
				true ->
					scale((Frac * BExp * 2), 2, BExp, BExp,
						  Round, Round, Float);
				false ->
					scale((Frac * BExp * 4), 4, (BExp * 2), BExp,
						  Round, Round, Float)
			end;
		false ->
			case (Exp == ?MIN_EXP) orelse (Frac /= ?BIG_POW) of
				true ->
					scale((Frac * 2), 1 bsl (1 - Exp), 1, 1,
						  Round, Round, Float);
				false ->
					scale((Frac * 4), 1 bsl (2 - Exp), 2, 1,
						  Round, Round, Float)
			end
	end.

scale(R, S, MPlus, MMinus, LowOk, HighOk, Float) ->
	Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
	case Est >= 0 of
		true ->
			fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est,
				  LowOk, HighOk);
		false ->
			Scale = int_pow(10, -Est),
			fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est,
				  LowOk, HighOk)
	end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
	TooLow = case HighOk of
				 true ->
					 (R + MPlus) >= S;
				 false ->
					 (R + MPlus) > S
			 end,
	case TooLow of
		true ->
			[(K + 1) | generate(R, S, MPlus, MMinus, LowOk, HighOk)];
		false ->
			[K | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)]
	end.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
	D = R0 div S,
	R = R0 rem S,
	TC1 = case LowOk of
			  true ->
				  R =< MMinus;
			  false ->
				  R < MMinus
		  end,
	TC2 = case HighOk of
			  true ->
				  (R + MPlus) >= S;
			  false ->
				  (R + MPlus) > S
		  end,
	case TC1 of
		false ->
			case TC2 of
				false ->
					[D | generate(R * 10, S, MPlus * 10, MMinus * 10,
								  LowOk, HighOk)];
				true ->
					[D + 1]
			end;
		true ->
			case TC2 of
				false ->
					[D];
				true ->
					case R * 2 < S of
						true ->
							[D];
						false ->
							[D + 1]
					end
			end
	end.

insert_decimal(0, S) ->
	"0." ++ S;
insert_decimal(Place, S) when Place > 0 ->
	L = length(S),
	case Place - L of
		0 ->
			S ++ ".0";
		N when N < 0 ->
			{S0, S1} = lists:split(L + N, S),
			S0 ++ "." ++ S1;
		N when N < 6 ->
			% more places than digits
			S ++ lists:duplicate(N, $0) ++ ".0";
		_ ->
			insert_decimal_exp(Place, S)
	end;
insert_decimal(Place, S) when Place > -6 ->
	"0." ++ lists:duplicate(abs(Place), $0) ++ S;
insert_decimal(Place, S) ->
	insert_decimal_exp(Place, S).

insert_decimal_exp(Place, S) ->
	[C | S0] = S,
	S1 = case S0 of
			 [] ->
				 "0";
			 _ ->
				 S0
		 end,
	Exp = case Place < 0 of
			  true ->
				  "e-";
			  false ->
				  "e+"
		  end,
	[C] ++ "." ++ S1 ++ Exp ++ integer_to_list(abs(Place - 1)).

int_ceil(X) ->
	T = trunc(X),
	case (X - T) of
		Neg when Neg < 0 -> T;
		Pos when Pos > 0 -> T + 1;
		_ -> T
	end.

int_pow(_X, 0) ->
	1;
int_pow(X, N) when N > 0 ->
	int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
	R * X;
int_pow(X, N, R) ->
	int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

log2floor(Int) ->
	log2floor(Int, 0).

log2floor(0, N) ->
	N;
log2floor(Int, N) ->
	log2floor(Int bsr 1, 1 + N).
