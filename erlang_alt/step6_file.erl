#!/usr/bin/env escript

-import(step6_file, [repl/1]).

main(Args) ->
    code:add_patha("ebin"),
    repl(Args).
