-type bin() :: unicode:unicode_binary().
-type symbol() :: atom().
-type mal_string() :: bin().
-type mal_list() :: [expr()].
-type vector() :: {vector, mal_list()}.
-type builtin() :: fun(([expr()]) -> expr()).
-type fn_binds() :: [symbol()] | {[symbol()], symbol()}.
-type fn() :: {fn, fn_binds(), expr(), env:env()}.
-type macro() :: {macro, fn_binds(), expr()}.
-type expr() :: integer() | float() | mal_string() | symbol() | mal_list() | vector()
              | map() | fn() | macro() | builtin().

-define(fn(Fn), {fn, _, _, _} = Fn).
-define(macro(Macro), {macro, _, _} = Macro).
-define(is_builtin(Expr), is_function(Expr, 1)).
-define(is_mal_atom(Expr), Expr =:= nil; Expr =:= true; Expr =:= false; is_integer(Expr);
                           is_float(Expr); is_binary(Expr); ?is_builtin(Expr)).
