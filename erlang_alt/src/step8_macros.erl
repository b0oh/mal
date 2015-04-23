-module(step8_macros).
-include("mal.hrl").
-import(core, [readline/1, println/1]).
-export([repl/1]).

-type env() :: env:env().

-spec repl_env(Args :: list()) -> env().
repl_env(Args) ->
    Env0 = core:add_builtins(env:new()),
    {_, Env1} = eval(read(<<"(def! not (fn* [x] (if x false true)))">>), Env0),
    {_, Env2} = eval(read(<<"(def! load-file (fn* [f] (eval (read-string (str \"(do \" (slurp f) \")\")))))">>), Env1),
    {_, Env3} = eval(read(<<"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))">>), Env2),
    {_, Env4} = eval(read(<<"(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME \~(first xs)) (if or_FIXME or_FIXME (or \~@(rest xs))))))))">>), Env3),
    env:set('*ARGV*', [list_to_binary(Arg) || Arg <- Args], Env4).

-spec repl(list() | (Env :: env())) -> ok.
repl([]) ->
    repl(repl_env([]));
repl([FileName | Args]) ->
    Env0 = repl_env(Args),
    Env1 = rep(<< (<<"(load-file \"">>)/binary,
                  (list_to_binary(FileName))/binary,
                  (<<"\")">>)/binary >>,
               Env0),
    repl(Env1);
repl(Env0) ->
    case readline(<<"user> ">>) of
        eof ->
            println(<<"">>);
        Line ->
            Env1 = rep(Line, Env0),
            repl(Env1)
    end.

-spec rep(String :: bin(), Env :: env()) -> env().
rep(String, Env0) ->
    {Print, Env2} =
        try eval(read(String), Env0) of
            {Expr, Env1} ->
                {print(Expr), Env1}
        catch throw:{error, Error} ->
            {["error: ", Error], Env0}
        end,
    println(Print),
    Env2.

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
eval(Expr, Env) ->
    eval_expr(macroexpand(Expr, Env), Env).

-spec eval_(Expr :: expr(), Env :: env()) -> expr().
eval_(Expr0, Env) ->
    {Expr1, _} = eval(Expr0, Env),
    Expr1.

-spec eval_expr(Expr :: expr(), Env :: env()) -> {expr(), env()}.
eval_expr(Expr, Env) when ?is_mal_atom(Expr) ->
    {Expr, Env};
eval_expr(?fn(Expr), Env) ->
    {Expr, Env};
eval_expr(Expr, Env) when is_atom(Expr) ->
    case atom_to_list(Expr) of
        [$: | _] ->
            {Expr, Env};
        _ ->
            case env:lookup(Expr, Env) of
                {ok, Value} -> {Value, Env};
                not_found -> throw({error, ["'", atom_to_list(Expr), "' not found"]})
            end
    end;
eval_expr([list | List], Env) ->
    {eval_list(List, Env), Env};
eval_expr({vector, Vec}, Env) ->
    {{vector, eval_list(Vec, Env)}, Env};
eval_expr(Expr, Env) when is_map(Expr) ->
    Map = maps:fold(fun(K, V, Acc) -> maps:put(eval_(K, Env), eval_(V, Env), Acc) end, #{}, Expr),
    {Map, Env};
eval_expr([do | Body], Env0) ->
    {[Last | _], Env1} = eval_seq_rev(Body, Env0),
    {Last, Env1};
eval_expr(['if', Cond, True], Env) ->
    eval(['if', Cond, True, nil], Env);
eval_expr(['if', Cond, True, False], Env) ->
    Expr = eval_(Cond, Env),
    case Expr of
        false -> eval(False, Env);
        nil   -> eval(False, Env);
        _     -> eval(True, Env)
    end;
eval_expr(['if' | _], _) ->
    throw({error, "syntax error in 'if'"});
eval_expr(['def!', Name, Body], Env0) when is_atom(Name) ->
    Val0 = eval_(Body, Env0),
    Val1 = case Val0 of
        ?fn(Fn) -> fixerize_fn(Name, Fn);
        _       -> Val0
    end,
    Env1 = env:set_top(Name, Val1, Env0),
    {Val1, Env1};
eval_expr(['def!' | _], _) ->
    throw({error, "syntax error in 'def!'"});
eval_expr(['defmacro!', Name, Body], Env0) when is_atom(Name) ->
    Val0 = eval_(Body, Env0),
    Val1 = case Val0 of
        ?fn(Fn) -> fn_to_macro(Fn);
        _       -> Val0
    end,
    Env1 = env:set_top(Name, Val1, Env0),
    {Val1, Env1};
eval_expr(['defmacro!' | _], _) ->
    throw({error, "syntax error in 'defmacro!'"});
eval_expr(['let*', {vector, Binds}, Body], Env) ->
    eval(['let*', Binds, Body], Env);
eval_expr(['let*', Binds, Body], Env0) ->
    Env1 = let_bindings(Binds, Env0),
    Expr = eval_(Body, Env1),
    {Expr, Env0};
eval_expr(['let*' | _], _) ->
    throw({error, "syntax error in let*"});
eval_expr(['fn*', {vector, Binds}, Body], Env) ->
    eval(['fn*', Binds, Body], Env);
eval_expr(['fn*', Binds, Body], Env) ->
    [throw({error, "syntax error in 'fn*'"}) || Bind <- Binds, not is_atom(Bind)],
    {make_fn(Binds, Body, Env), Env};
eval_expr(['fn*' | _], _) ->
    throw({error, "syntax error in fn*"});
eval_expr([eval, Body], Env0) ->
    {Expr, Env1} = eval(Body, Env0),
    eval(Expr, Env1);
eval_expr([eval | _], _) ->
    throw({error, "syntax error in eval"});
eval_expr([quote, Quoted], Env) ->
    {Quoted, Env};
eval_expr([quote | _], _) ->
    throw({error, "syntax error in quote"});
eval_expr([quasiquote, Quoted], Env) ->
    eval(quasiquote(Quoted), Env);
eval_expr([quasiquote | _], _) ->
    throw({error, "syntax error in quasiquote"});
eval_expr([macroexpand, Macro], Env) ->
    {macroexpand(Macro, Env), Env};
eval_expr([macroexpand | _], _) ->
    throw({error, "syntax error in macroexpand"});
eval_expr([Fun | Args], Env0) ->
    F = eval_(Fun, Env0),
    A = eval_list(Args, Env0),
    {Expr1, Env1} = mal_apply(F, A),
    {Vars, Vals} = lists:unzip(maps:to_list(env:top_bindings(Env1))),
    {Expr1, env:extend(Vars, Vals, Env0)}.

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
    let_bindings(Rest, env:set(Var, Val_, Env));
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

-spec fn_to_macro(Fn :: fn()) -> macro().
fn_to_macro({fn, Binds, Body, _}) ->
    {macro, Binds, Body}.

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

-spec quasiquote(Quoted :: expr()) -> expr().
quasiquote({vector, Vector}) ->
    quasiquote(Vector);
quasiquote([unquote, Quoted]) ->
    Quoted;
quasiquote([unquote | _]) ->
    throw({error, "syntax error in unquote"});
quasiquote([['splice-unquote', X] | Rest]) ->
    [concat, X, quasiquote(Rest)];
quasiquote(['splice-unquote' | _]) ->
    throw({error, "syntax error in splice-unquote"});
quasiquote([X | Rest]) ->
    [cons, quasiquote(X), quasiquote(Rest)];
quasiquote(Expr) ->
    [quote, Expr].

-spec macroexpand(Expr :: expr(), Env :: env()) -> expr().
macroexpand([Name | Args] = Expr, Env0) ->
    case env:lookup(Name, Env0) of
        {ok, {macro, Binds, Body}} ->
            Env1 = bind_env(Binds, Args, Env0),
            macroexpand(eval_(Body, Env1), Env0);
        _ ->
            Expr
    end;
macroexpand(Expr, _) ->
    Expr.

-spec mal_apply(Fun :: builtin() | fn(), Args :: [expr()]) -> {expr(), env()}.
mal_apply({fn, Binds, Body, Env0}, Args) ->
    Env1 = bind_env(Binds, Args, Env0),
    eval(Body, Env1);
mal_apply(F, A) ->
    {F(A), env:new()}.

bind_env({Binds, RestBind}, Args0, Env0) ->
    Len = length(Binds),
    {Args1, RestArgs} = lists:split(Len, Args0),
    Env1 = env:extend(Binds, Args1, Env0),
    env:set(RestBind, RestArgs, Env1);
bind_env(Binds, Args, Env) ->
    env:extend(Binds, Args, Env).
