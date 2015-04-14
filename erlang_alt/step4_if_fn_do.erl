#!/usr/bin/env escript

-import(step4_if_fn_do, [repl/0]).

main(_) ->
    code:add_patha("ebin"),
    repl().
