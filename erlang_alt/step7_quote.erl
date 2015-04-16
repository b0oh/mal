#!/usr/bin/env escript

-import(step7_quote, [repl/1]).

main(Args) ->
    code:add_patha("ebin"),
    repl(Args).
