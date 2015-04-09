#!/usr/bin/env escript

-import(step0_repl, [repl/0]).

main(_) ->
    code:add_patha("ebin"),
    repl().
