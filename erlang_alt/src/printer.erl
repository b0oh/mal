-module(printer).
-include("mal.hrl").
-export([print_expr/1, print_expr/2, print_seq/2]).

-spec print_expr(Expr :: expr()) -> bin().
print_expr(Expr) ->
    print_expr(Expr, true).

-spec print_expr(Expr :: expr(), Readable :: boolean()) -> bin().
print_expr(Expr, _) when is_atom(Expr) ->
    atom_to_binary(Expr, utf8);
print_expr(Expr, _) when is_integer(Expr) ->
    integer_to_binary(Expr);
print_expr(Expr, _) when is_float(Expr) ->
    float_to_binary(Expr, [{decimals, 10}, compact]);
print_expr(Expr, true) when is_binary(Expr) ->
    escape(Expr);
print_expr(Expr, false) when is_binary(Expr) ->
    Expr;
print_expr(Expr, R) when is_list(Expr) ->
    <<$(, (print_seq(Expr, R))/binary, $)>>;
print_expr(Expr, R) when is_map(Expr) ->
    <<${, (print_map(Expr, R))/binary, $}>>;
print_expr({vector, Vec}, R) ->
    <<$[, (print_seq(Vec, R))/binary, $]>>;
print_expr(?fn(_), _) ->
    <<"#<fn>">>;
print_expr(Expr, _) when ?is_builtin(Expr) ->
    <<"#<builtin>">>.

-spec print_seq(Exprs :: [expr()], Readable :: boolean()) -> bin().
print_seq([], _) ->
    <<>>;
print_seq([Expr], R) ->
    print_expr(Expr, R);
print_seq([Expr | Rest], R) ->
    <<(print_expr(Expr, R))/binary, $\s, (print_seq(Rest, R))/binary>>.

-spec print_map(Map :: map(), Readable :: boolean()) -> bin().
print_map(Map, R) ->
    print_map_(maps:to_list(Map), R).

print_map_([], _) ->
    <<>>;
print_map_([{K, V}], R) ->
    <<(print_expr(K, R))/binary, $\s, (print_expr(V, R))/binary>>;
print_map_([{K, V} | Rest], R) ->
    <<(print_expr(K, R))/binary, $\s, (print_expr(V, R))/binary, $\s, (print_map_(Rest, R))/binary>>.

-spec escape(bin()) -> bin().
escape(String) ->
    Escaped = << <<(escape_char(C))/binary>> || <<C/utf8>> <= String >>,
    <<$", Escaped/binary, $">>.

-spec escape_char(Char :: char()) -> bin().
escape_char($")  -> <<$\\, $">>;
escape_char($\\) -> <<$\\, $\\>>;
escape_char($\n) -> <<$\\, $\n>>;
escape_char(C)   -> <<C/utf8>>.
