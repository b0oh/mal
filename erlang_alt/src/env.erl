-module(env).
-include("mal.hrl").
-export([new/0, lookup/2, update_binding/3, extend/3]).
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

-spec update_binding(Var :: expr(), Val :: expr(), Env :: env()) -> env().
update_binding(Var, Val, Env) ->
    maps:put(Var, Val, Env).

-spec extend(Vars :: [symbol()], Vals :: [expr()], Env :: env()) -> env().
extend([], [], Env) ->
    Env;
extend([Var | Vars], [Val | Vals], Env0) ->
    extend(Vars, Vals, maps:put(Var, Val, Env0)).
