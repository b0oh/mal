-module(step3_env).
-include("mal.hrl").
-import(core, [readline/1, println/1]).
-export([repl/1]).

-type env() :: env:env().

-spec repl(Env :: env()) -> ok.
repl(Env0) ->
    case readline(<<"user> ">>) of
        eof ->
            println(<<"">>);
        Line ->
            {Res, Env1} =
                try rep(Line, Env0)
                catch throw:{error, Error} ->
                     {["error: ", Error], Env0}
                end,
            println(Res),
            repl(Env1)
    end.

-spec rep(String :: bin(), Env :: env()) -> {bin(), env()}.
rep(String, Env0) ->
    {Expr, Env1} = eval(read(String), Env0),
    {print(Expr), Env1}.

-spec read(String :: bin()) -> expr().
read(String) ->
    case reader:read_str(String) of
        {ok, Expr} -> Expr;
        {error, Error} -> throw({error, Error})
    end.

-spec print(Expr :: expr()) -> bin().
print(Expr) ->
    printer:print_expr(Expr).

-spec eval(Expr :: expr(), Env :: env()) -> {expr(), env()}.
eval(Expr, Env) when is_integer(Expr) ->
    {Expr, Env};
eval(Expr, Env) when is_float(Expr) ->
    {Expr, Env};
eval(Expr, Env) when is_binary(Expr) ->
    {Expr, Env};
eval(Expr, Env) when is_atom(Expr) ->
    case atom_to_list(Expr) of
        [$: | _] ->
            {Expr, Env};
        _ ->
            case env:lookup(Expr, Env) of
                {ok, Value} -> {Value, Env};
                not_found -> throw({error, ["'", atom_to_list(Expr), "' not found"]})
            end
    end;
eval({vector, Vec}, Env) ->
    {{vector, eval_seq(Vec, Env)}, Env};
eval(Expr, Env) when is_map(Expr) ->
    Map = maps:fold(fun(K, V, Acc) -> maps:put(eval_(K, Env), eval_(V, Env), Acc) end, #{}, Expr),
    {Map, Env};
eval(['def!', Var, Body], Env) when is_atom(Var) ->
    Val = eval_(Body, Env),
    {Val, env:update_binding(Var, Val, Env)};
eval(['def!' | _], _) ->
    throw({error, "syntax error in 'def!'"});
eval(['let*', {vector, Bindings}, Body], Env) ->
    eval(['let*', Bindings, Body], Env);
eval(['let*', Bindings, Body], Env0) ->
    Env1 = let_bindings(Bindings, Env0),
    Expr = eval_(Body, Env1),
    {Expr, Env0};
eval(['let*' | _], _) ->
    throw({error, "syntax error in let*"});
eval([Fun | Args], Env) ->
    F = eval_(Fun, Env),
    A = eval_seq(Args, Env),
    {F(A), Env}.

-spec eval_(Expr :: expr(), Env :: env()) -> expr().
eval_(Expr0, Env) ->
    {Expr1, _} = eval(Expr0, Env),
    Expr1.

-spec eval_seq(Exprs :: [expr()], Env :: env()) -> [expr()].
eval_seq(Exprs, Env) ->
    [eval_(Expr, Env) || Expr <- Exprs].

-spec let_bindings(Bindings :: [expr()], Env :: env()) -> env().
let_bindings([], Env) ->
    Env;
let_bindings([Var, Val | Rest], Env) when is_atom(Var) ->
    Val_ = eval_(Val, Env),
    let_bindings(Rest, env:update_binding(Var, Val_, Env));
let_bindings(_, _) ->
    throw({error, "syntax error in let*"}).
