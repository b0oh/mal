-module(core).
-export([readline/1, println/1]).

-type bin() :: unicode:unicode_binary().

-spec readline(Prompt :: bin()) -> eof | bin().
readline(Prompt) ->
    io:setopts([{binary, true}, {encoding, utf8}]),
    io:get_line(Prompt).

-spec println(String :: bin()) -> ok.
println(String) ->
    io:format("~ts~n", [String]).
