#!/usr/bin/env escript

-import(core, [readline/1, println/1]).
-import(reader, [read_str/1]).
-import(printer, [print_expr/1]).

repl_env() ->
    #{'+' => fun(X, Y) -> X + Y end,
      '-' => fun(X, Y) -> X - Y end,
      '*' => fun(X, Y) -> X * Y end,
      '/' => fun(X, Y) -> round(X / Y) end}.

eval_list(Expr, Env) -> [eval(X, Env) || X <- Expr].


eval(Expr, Env) when is_atom(Expr) ->
    case maps:find(Expr, Env) of
        {ok, Value} -> Value;
        error       -> throw(["'", atom_to_list(Expr), "' not found"])
    end;

eval({vector, Vec}, Env) -> {vector, eval_list(Vec, Env)};

eval(Expr, Env) when is_map(Expr) ->
    maps:fold(fun(K, V, Acc) -> maps:put(eval(K, Env), eval(V, Env), Acc) end, #{}, Expr);

eval([Fun | Args], Env) ->
    F = eval(Fun, Env),
    A = eval_list(Args, Env),
    apply(F, A);

eval(Expr, _Env) -> Expr.


read(Str) -> read_str(Str).

print(Expr) -> print_expr(Expr).

rep(Str, Env) -> print(eval(read(Str), Env)).

repl(Env) ->
    Line = readline(<<"user> ">>),
    case Line of
        eof ->
            println(<<"">>);
        _   ->
            Res = try rep(Line, Env)
                  catch _:Ex ->
                      ["Uncaught exception: ", Ex]
                  end,
            println(Res),
            repl(Env)
    end.

main(_) ->
    code:add_patha("ebin"),
    Env = repl_env(),
    repl(Env).
