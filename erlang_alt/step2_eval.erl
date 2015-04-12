#!/usr/bin/env escript

-import(step2_eval, [repl/1]).

main(_) ->
    code:add_patha("ebin"),
    Env = core:add_builtins(env:new()),
    repl(Env).
