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
    Builtins = #{count    => fun count/1,
                 'list?'  => fun is_list/1,
                 'empty?' => fun is_empty/1,
                 'pr-str' => fun pr_str/1,
                 str      => fun str/1,
                 prn      => fun prn/1,
                 println  => fun prnln/1,
                 '='      => fun eq/1,
                 '<'      => fun lt/1,
                 '<='     => fun lte/1,
                 '>'      => fun gt/1,
                 '>='     => fun gte/1,
                 '+'      => fun plus/1,
                 '-'      => fun minus/1,
                 '*'      => fun multiply/1,
                 '/'      => fun divide/1},
    maps:fold(fun(K, F, Env) -> env:update_binding(K, F, Env) end,
              Env0, Builtins).

count([{vector, Vec}]) -> count([Vec]);
count([nil]) -> 0;
count([List]) -> length(List).

is_list([List]) -> erlang:is_list(List).

is_empty([{vector, Vec}]) -> is_empty([Vec]);
is_empty([List]) -> List =:= [].

pr_str(Args) -> printer:print_seq(Args, true).

str(Args) -> << <<(printer:print_expr(A, false))/binary>> || A <- Args >>.

prn(Args) ->
    println(pr_str(Args)),
    nil.

prnln(Args) ->
    println(printer:print_seq(Args, false)),
    nil.

eq([{vector, X}, {vector, Y}]) -> X =:= Y;
eq([{vector, Vec}, Y]) -> Vec =:= Y;
eq([X, {vector, Vec}]) -> X =:= Vec;
eq([X, Y]) -> X =:= Y.

lt([X, Y]) -> X < Y.
lte([X, Y]) -> X =< Y.
gt([X, Y]) -> X > Y.
gte([X, Y]) -> X >= Y.

plus([X, Y]) -> X + Y.
minus([X, Y]) -> X - Y.
multiply([X, Y]) -> X * Y.
divide([X, Y]) -> round(X / Y).
