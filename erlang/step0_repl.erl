#!/usr/bin/env escript

-import(core, [readline/1, println/1]).

read(Str) -> Str.

eval(Ast) -> Ast.

print(Exp) -> Exp.

rep(Str) ->
    print(eval(read(Str))).

repl() ->
    Line = readline(<<"user> ">>),
    case Line of
        eof ->
            println(<<"">>);
        _   ->
            println(rep(Line)),
            repl()
    end.

main(_) ->
    code:add_patha("ebin"),
    repl().
