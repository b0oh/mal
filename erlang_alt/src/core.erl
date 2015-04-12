-module(core).
-include("mal.hrl").
-export([readline/1, println/1, add_builtins/1]).

-spec readline(Prompt :: bin()) -> eof | bin().
readline(Prompt) ->
    io:setopts([{binary, true}, {encoding, utf8}]),
    io:get_line(Prompt).

-spec println(String :: bin()) -> ok.
println(String) ->
    io:format("~ts~n", [String]).

-spec add_builtins(Env :: env:env()) -> env:env().
add_builtins(Env0) ->
    Builtins = #{'+' => fun plus/1,
                 '-' => fun minus/1,
                 '*' => fun multiply/1,
                 '/' => fun divide/1},
    maps:fold(fun(K, F, Env) -> env:update_binding(K, F, Env) end,
              Env0, Builtins).

plus([X, Y]) -> X + Y.
minus([X, Y]) -> X - Y.
multiply([X, Y]) -> X * Y.
divide([X, Y]) -> round(X / Y).
