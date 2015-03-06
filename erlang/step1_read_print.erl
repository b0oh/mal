#!/usr/bin/env escript

-import(core, [readline/1, println/1]).
-import(reader, [read_str/1]).
-import(printer, [print_expr/1]).

read(Str) ->
    read_str(Str).

eval(Expr, _Env) ->
    Expr.

print(Expr) ->
    print_expr(Expr).

rep(Str) ->
    print(eval(read(Str), #{})).

repl() ->
    Line = readline(<<"user> ">>),
    case Line of
        eof ->
            println(<<"">>);
        _   ->
            Res = try rep(Line)
                  catch _:Ex ->
                      ["Uncaught exception: ", Ex]
                  end,
            println(Res),
            repl()
    end.

main(_) ->
    code:add_patha("ebin"),
    repl().
