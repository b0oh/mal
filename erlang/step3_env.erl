#!/usr/bin/env escript

-import(core, [readline/1, println/1]).
-import(reader, [read_str/1]).
-import(printer, [print_expr/1]).

repl_env() ->
    #{'+' => fun(X, Y) -> X + Y end,
      '-' => fun(X, Y) -> X - Y end,
      '*' => fun(X, Y) -> X * Y end,
      '/' => fun(X, Y) -> round(X / Y) end}.


list_folder(Expr0, {List, Env0}) ->
    {Expr1, Env1} = eval(Expr0, Env0),
    {[Expr1 | List], Env1}.

map_folder(K, V, {Map, Env0}) ->
    {Key, Env1} = eval(K, Env0),
    {Value, Env2} = eval(V, Env1),
    {maps:put(Key, Value, Map), Env2}.

eval_list(List, Env) -> lists:foldr(fun(X, Acc) -> list_folder(X, Acc) end, {[], Env}, List).

eval_map(Map, Env) -> maps:fold(fun (K, V, Acc) -> map_folder(K, V, Acc) end, {#{}, Env}, Map).

eval_bindings([], Env) -> Env;
eval_bindings([Name, Body | Rest], Env) when is_atom(Name) ->
    {Value, _} = eval(Body, Env),
    eval_bindings(Rest, maps:put(Name, Value, Env));
eval_bindings(_, _) -> throw("syntax error in let*").

eval(Expr, Env) when is_atom(Expr) ->
    case maps:find(Expr, Env) of
        {ok, Value} -> {Value, Env};
        error       -> throw(["'", atom_to_list(Expr), "' not found"])
    end;

eval({vector, Vec0}, Env0) ->
    {Vec1, Env1} = eval_list(Vec0, Env0),
    {{vector, Vec1}, Env1};

eval(Expr, Env) when is_map(Expr) -> eval_map(Expr, Env);

eval(['def!', Name, Body], Env) when is_atom(Name) ->
    {Value, _} = eval(Body, Env),
    {Value, maps:put(Name, Value, Env)};

eval(['let*', {vector, Bindings}, Body], Env) ->
    eval(['let*', Bindings, Body], Env);

eval(['let*', Bindings, Body], Env0) ->
    Env1 = eval_bindings(Bindings, Env0),
    {Expr, _} = eval(Body, Env1),
    {Expr, Env0};

eval([Fun | Args], Env0) ->
    {F, Env1} = eval(Fun, Env0),
    {A, Env2} = eval_list(Args, Env1),
    {apply(F, A), Env2};

eval(Expr, Env) -> {Expr, Env}.

read(Str) -> read_str(Str).

print(Expr) -> print_expr(Expr).

rep(Str, Env0) ->
    {Expr, Env1} = eval(read(Str), Env0),
    {print(Expr), Env1}.

repl(Env0) ->
    Line = readline(<<"user> ">>),
    case Line of
        eof ->
            println(<<"">>);
        _   ->
            {Res, Env1} =
                try rep(Line, Env0)
                catch _:Ex ->
                    ["Uncaught exception: ", Ex]
                end,
            println(Res),
            repl(Env1)
    end.

main(_) ->
    code:add_patha("ebin"),
    Env = repl_env(),
    repl(Env).
