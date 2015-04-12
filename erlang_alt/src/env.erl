-module(env).
-include("mal.hrl").
-export([new/0, lookup/2, add_binding/3]).
-export_type([env/0]).

-type env() :: map().

-spec new() -> env().
new() ->
    #{}.

-spec lookup(Var :: expr(), Env :: env()) -> {ok, expr()} | not_found.
lookup(Var, Env) ->
    case maps:find(Var, Env) of
        {ok, Val} -> {ok, Val};
        error       -> not_found
    end.

-spec add_binding(Var :: expr(), Val :: expr(), Env :: env()) -> env().
add_binding(Var, Val, Env) ->
    maps:put(Var, Val, Env).
