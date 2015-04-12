-module(step1_read_print).
-include("mal.hrl").
-import(core, [readline/1, println/1]).
-export([repl/0]).

-spec repl() -> ok.
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

-spec rep(String :: bin()) -> bin().
rep(String) ->
    print(eval(read(String))).

-spec read(String :: bin()) -> expr().
read(String) ->
    case reader:read_str(String) of
        {ok, Expr} -> Expr;
        {error, Error} -> throw({error, Error})
    end.

-spec print(Expr :: expr()) -> bin().
print(Expr) ->
    printer:print_expr(Expr).

-spec eval(Expr :: expr()) -> expr().
eval(Expr) ->
    Expr.
