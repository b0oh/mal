-module(step0_repl).
-export([repl/0]).
-import(core, [readline/1, println/1]).

-type bin() :: unicode:unicode_binary().
-type expr() :: bin().

-spec rstrip(String :: bin(), Char :: char()) -> bin().
rstrip(<<>>, _) ->
    <<>>;
rstrip(<<Ch/utf8>>, C) when Ch =:= C ->
    <<>>;
rstrip(<<Ch/utf8>>, _) ->
    <<Ch/utf8>>;
rstrip(<<Ch/utf8, Rest/binary>>, C) ->
    <<Ch/utf8, (rstrip(Rest, C))/binary>>.

-spec read(String :: bin()) -> expr().
read(String) ->
    String.

-spec eval(Expr :: expr()) -> expr().
eval(Expr) ->
    Expr.

-spec print(Expr :: expr()) -> bin().
print(Expr) ->
    Expr.

-spec rep(String :: bin()) -> bin().
rep(String) ->
    print(eval(read(String))).

-spec repl() -> ok.
repl() ->
    case readline(<<"user> ">>) of
        eof ->
            println(<<"">>);
        Line ->
            println(rep(rstrip(Line, $\n))),
            repl()
    end.
