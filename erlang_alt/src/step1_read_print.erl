-module(step1_read_print).
-export([repl/0]).
-import(core, [readline/1, println/1]).

-type bin() :: unicode:unicode_binary().
-type expr() :: reader:expr().

-spec read(String :: bin()) -> expr().
read(String) ->
    case reader:read_str(String) of
        {ok, Expr} -> Expr;
        {error, Error} -> throw({error, Error})
    end.

-spec eval(Expr :: expr()) -> expr().
eval(Expr) ->
    Expr.

-spec print(Expr :: expr()) -> bin().
print(Expr) ->
    printer:print_expr(Expr).

-spec rep(String::bin()) -> bin().
rep(String) ->
    print(eval(read(String))).

-spec repl() -> 'ok'.
repl() ->
    case readline(<<"user> ">>) of
        eof ->
            println(<<"">>);
        Line ->
            Res = try rep(Line)
            catch throw:{error, Error} ->
                ["error: ", Error]
            end,
            println(Res),
            repl()
    end.
