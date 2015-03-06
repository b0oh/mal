-module(core).
-export([readline/1, println/1]).

readline(Prompt) ->
    io:setopts([{binary, true}, {encoding, utf8}]),
    io:get_line(Prompt).

println(Str) ->
    io:format("~ts~n", [Str]).
