-module(step4_if_fn_do).
-include("mal.hrl").
-import(core, [readline/1, println/1]).
-export([repl/0]).

-type env() :: env:env().

-spec repl() -> ok.
repl() ->
    Env0 = core:add_builtins(env:new()),
    {_, Env1} = eval(read(<<"(def! not (fn* (a) (if a false true)))">>), Env0),
    repl(Env1).

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
    printer:print_expr(Expr, true).

-spec eval(Expr :: expr(), Env :: env()) -> {expr(), env()}.
eval(Expr, Env) when ?is_atom(Expr) ->
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
eval([list | List], Env) ->
    {eval_list(List, Env), Env};
eval({vector, Vec}, Env) ->
    {{vector, eval_list(Vec, Env)}, Env};
eval(Expr, Env) when is_map(Expr) ->
    Map = maps:fold(fun(K, V, Acc) -> maps:put(eval_(K, Env), eval_(V, Env), Acc) end, #{}, Expr),
    {Map, Env};
eval([do | Body], Env0) ->
    {[Last | _], Env1} = eval_seq_rev(Body, Env0),
    {Last, Env1};
eval(['if', Cond, True], Env) ->
    eval(['if', Cond, True, nil], Env);
eval(['if', Cond, True, False], Env) ->
    Expr = eval_(Cond, Env),
    case Expr of
        false -> eval(False, Env);
        nil   -> eval(False, Env);
        _     -> eval(True, Env)
    end;
eval(['if' | _], _) ->
    throw({error, "syntax error in 'if'"});
eval(['def!', Var, Body], Env0) when is_atom(Var) ->
    Val0 = eval_(Body, Env0),
    Val1 = case Val0 of
        ?fn(Fn) -> fixerize_fn(Var, Fn);
        _       -> Val0
    end,
    Env1 = env:update_binding(Var, Val1, Env0),
    {Val1, Env1};
eval(['def!' | _], _) ->
    throw({error, "syntax error in 'def!'"});
eval(['let*', {vector, Binds}, Body], Env) ->
    eval(['let*', Binds, Body], Env);
eval(['let*', Binds, Body], Env0) ->
    Env1 = let_bindings(Binds, Env0),
    Expr = eval_(Body, Env1),
    {Expr, Env0};
eval(['let*' | _], _) ->
    throw({error, "syntax error in let*"});
eval(['fn*', {vector, Binds}, Body], Env) ->
    eval(['fn*', Binds, Body], Env);
eval(['fn*', Binds, Body], Env) ->
    [throw({error, "syntax error in 'fn*'"}) || Bind <- Binds, not is_atom(Bind)],
    {make_fn(Binds, Body, Env), Env};
eval(['fn*' | _], _) ->
    throw({error, "syntax error in fn*"});
eval(?fn(Expr), Env) ->
    {Expr, Env};
eval([Fun | Args], Env) ->
    F = eval_(Fun, Env),
    A = eval_list(Args, Env),
    {mal_apply(F, A), Env}.

-spec eval_(Expr :: expr(), Env :: env()) -> expr().
eval_(Expr0, Env) ->
    {Expr1, _} = eval(Expr0, Env),
    Expr1.

-spec eval_list(Exprs :: [expr()], Env :: env()) -> [expr()].
eval_list(Exprs, Env) ->
    [eval_(Expr, Env) || Expr <- Exprs].

-spec eval_seq_rev(Exprs :: [expr()], Env :: env()) -> {[expr()], env()}.
eval_seq_rev(Seq, Env) ->
    lists:foldl(fun(Expr0, {List, Env0}) ->
                        {Expr1, Env1} = eval(Expr0, Env0),
                        {[Expr1 | List], Env1}
                end,
                {[], Env}, Seq).

-spec let_bindings(Binds :: [expr()], Env :: env()) -> env().
let_bindings([], Env) ->
    Env;
let_bindings([Var, Val | Rest], Env) when is_atom(Var) ->
    Val_ = eval_(Val, Env),
    let_bindings(Rest, env:update_binding(Var, Val_, Env));
let_bindings(_, _) ->
    throw({error, "syntax error in let*"}).

-spec make_fn(Binds :: [symbol()], Body :: [expr()], Env :: env()) -> fn().
make_fn(Binds0, Body, Env) ->
    {Binds1, Rest} = lists:splitwith(fun(X) -> X =/= '&' end, Binds0),
    case Rest of
        [] ->            {fn,  Binds1,            Body, Env};
        [_, RestBind] -> {fn, {Binds1, RestBind}, Body, Env};
        _ ->             throw({error, "syntax error in fn*"})
    end.

%% Applying Y combinator for adding self recursion
%% (def! fac (fn* [n] (if (= n 0) 1 (* n (fac (- n 1))))))
%% expands to
%% (def! fac ((fn* [f] (f f))
%%            (fn* [f] ((fn* [fac]
%% ->                      (fn* [n] (if (= n 0) 1 (* n (fac (- n 1))))))
%%                      (fn* [n] ((f f) n))))))

%% (def! sum (fn* [x acc] (if (= x 0) acc (sum (- x 1) (+ x acc)))))
%% expands to
%% (def! sum ((fn* [f] (f f))
%%            (fn* [f] ((fn* [sum]
%% ->                      (fn* [x acc] (if (= x 0) acc (sum (- x 1) (+ x acc)))))
%%                      (fn* [x acc] ((f f) x acc))))))
-spec fixerize_fn(Name :: symbol(), Fn :: fn()) -> fn().
fixerize_fn(Name, {fn, {Binds0, RestBind}, Body, Env}) ->
    Binds1 = Binds0 ++ [RestBind],
    FullBinds = Binds0 ++ ['&', RestBind],
    F0 = make_fn([f], [f, f], Env),
    F1 = make_fn([f], [['fn*', [Name], ['fn*', Binds1, Body]],
                       ['fn*', Binds1, [[f, f] | Binds1]]], Env),
    Fn = eval_([F0, F1], Env),
    eval_(make_fn(FullBinds, [Fn | Binds1], Env), Env);
fixerize_fn(Name, {fn, Binds, Body, Env}) ->
    F0 = make_fn([f], [f, f], Env),
    F1 = make_fn([f], [['fn*', [Name], ['fn*', Binds, Body]],
                       ['fn*', Binds, [[f, f] | Binds]]], Env),
    eval_([F0, F1], Env).

-spec mal_apply(Fun :: builtin() | fn(), Args :: [expr()]) -> expr().
mal_apply({fn, {Binds, RestBind}, Body, Env0}, Args0) ->
    Len = length(Binds),
    {Args1, RestArgs} = lists:split(Len, Args0),
    Env1 = env:extend(Binds, Args1, Env0),
    Env2 = env:update_binding(RestBind, RestArgs, Env1),
    eval_(Body, Env2);
mal_apply({fn, Binds, Body, Env0}, Args) ->
    Env1 = env:extend(Binds, Args, Env0),
    eval_(Body, Env1);
mal_apply(F, A) ->
    F(A).
