#!/usr/bin/env escript

-import(step8_macros, [repl/1]).

main(Args) ->
    code:add_patha("ebin"),
    repl(Args).
