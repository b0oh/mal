-module(env).
-include("mal.hrl").
-export([new/0, top_bindings/1, lookup/2, set_top/3, set/3, extend/3]).
-export_type([env/0]).

-type env() :: {map(), map()}.

-spec new() -> env().
new() ->
    {#{}, #{}}.

-spec top_bindings(Env :: env()) -> map().
top_bindings({Top, _}) ->
    Top.

-spec lookup(Var :: expr(), Env :: env()) -> {ok, expr()} | not_found.
lookup(Var, {Top, Local}) ->
    case maps:find(Var, Local) of
        {ok, Val} ->
            {ok, Val};
        error ->
            case maps:find(Var, Top) of
                {ok, Val} ->
                    {ok, Val};
                error ->
                    not_found
            end
    end.

-spec set_top(Var :: expr(), Val :: expr(), Env :: env()) -> env().
set_top(Var, Val, {Top, Local}) ->
    {maps:put(Var, Val, Top), Local}.

-spec set(Var :: expr(), Val :: expr(), Env :: env()) -> env().
set(Var, Val, {Top, Local}) ->
    {Top, maps:put(Var, Val, Local)}.

-spec extend(Vars :: [symbol()], Vals :: [expr()], Env :: env()) -> env().
extend([], [], Env) ->
    Env;
extend([Var | Vars], [Val | Vals], {Top, Local}) ->
    extend(Vars, Vals, {Top, maps:put(Var, Val, Local)}).
