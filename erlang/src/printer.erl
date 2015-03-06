-module(printer).
-export([print_expr/1]).

print_expr(Expr)    when is_atom(Expr)       -> atom_to_binary(Expr, utf8);
print_expr(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
print_expr(Float)   when is_float(Float)     -> float_to_binary(Float, [{decimals, 10}, compact]);
print_expr(String)  when is_binary(String)   -> <<$", String/binary, $">>;
print_expr(List)    when is_list(List)       -> <<$(, (print_list(List))/binary, $)>>;
print_expr(Map)     when is_map(Map)         -> <<${, (print_map(maps:to_list(Map)))/binary, $}>>;
print_expr({vector, Vec})                    -> <<$[, (print_list(Vec))/binary, $]>>;
print_expr({keyword, Keyword})               -> <<$\:, (atom_to_binary(Keyword, utf8))/binary>>.

print_list([])            -> <<"">>;
print_list([Expr])        -> print_expr(Expr);
print_list([Expr | Rest]) -> <<(print_expr(Expr))/binary, $\s, (print_list(Rest))/binary>>.

print_map([])              -> <<"">>;
print_map([{K, V}])        -> <<(print_expr(K))/binary, $\s, (print_expr(V))/binary>>;
print_map([{K, V} | Rest]) -> <<(print_expr(K))/binary, $\s, (print_expr(V))/binary, $\s, (print_map(Rest))/binary>>.
