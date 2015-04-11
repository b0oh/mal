#!/usr/bin/env escript

-import(step1_read_print, [repl/0]).

main(_) ->
    code:add_patha("ebin"),
    repl().
