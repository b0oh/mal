-module(step2_eval).
-include("mal.hrl").
-import(core, [readline/1, println/1]).
-export([repl/1]).

-type env() :: env:env().

-spec repl(Env :: env()) -> ok.
repl(Env) ->
    case readline(<<"user> ">>) of
        eof ->
            println(<<"">>);
        Line ->
            Res = try rep(Line, Env)
            catch throw:{error, Error} ->
                ["error: ", Error]
            end,
            println(Res),
            repl(Env)
    end.

-spec rep(String :: bin(), Env :: env()) -> bin().
rep(String, Env) ->
    print(eval(read(String), Env)).

-spec read(String :: bin()) -> expr().
read(String) ->
    case reader:read_str(String) of
        {ok, Expr} -> Expr;
        {error, Error} -> throw({error, Error})
    end.

-spec print(Expr :: expr()) -> bin().
print(Expr) ->
    printer:print_expr(Expr).

-spec eval(Expr :: expr(), Env :: env()) -> expr().
eval(Expr, _) when is_integer(Expr) ->
    Expr;
eval(Expr, _) when is_float(Expr) ->
    Expr;
eval(Expr, _) when is_binary(Expr) ->
    Expr;
eval(Expr, Env) when is_atom(Expr) ->
    case atom_to_list(Expr) of
        [$: | _] ->
            Expr;
        _ ->
            case env:lookup(Expr, Env) of
                {ok, Value} -> Value;
                not_found -> throw({error, ["'", atom_to_list(Expr), "' not found"]})
            end
    end;
eval({vector, Vec}, Env) ->
    {vector, eval_seq(Vec, Env)};
eval(Expr, Env) when is_map(Expr) ->
    maps:fold(fun(K, V, Acc) -> maps:put(eval(K, Env), eval(V, Env), Acc) end, #{}, Expr);
eval([Fun | Args], Env) ->
    F = eval(Fun, Env),
    A = eval_seq(Args, Env),
    apply(F, A).

-spec eval_seq(Exprs :: [expr()], Env :: env()) -> [expr()].
eval_seq(Exprs, Env) ->
    [eval(Expr, Env) || Expr <- Exprs].
