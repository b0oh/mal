-module(printer).
-export([print_expr/1]).

-type expr() :: reader:expr().
-type bin() :: unicode:unicode_binary().

-spec print_expr(Expr :: expr()) -> bin().
print_expr(Expr) when is_atom(Expr) ->
    atom_to_binary(Expr, utf8);
print_expr(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
print_expr(Float) when is_float(Float) ->
    float_to_binary(Float, [{decimals, 10}, compact]);
print_expr(String) when is_binary(String) ->
    escape(String);
print_expr(List) when is_list(List) ->
    <<$(, (print_seq(List))/binary, $)>>;
print_expr(Map) when is_map(Map) ->
    <<${, (print_map((Map)))/binary, $}>>;
print_expr({vector, Vec}) ->
    <<$[, (print_seq(Vec))/binary, $]>>.

-spec print_seq(Exprs :: [expr()]) -> bin().
print_seq([]) ->
    <<>>;
print_seq([Expr]) ->
    print_expr(Expr);
print_seq([Expr | Rest]) ->
    <<(print_expr(Expr))/binary, $\s, (print_seq(Rest))/binary>>.

-spec print_map(Map :: map()) -> bin().
print_map(Map) ->
    print_map_(maps:to_list(Map)).

print_map_([]) ->
    <<>>;
print_map_([{K, V}]) ->
    <<(print_expr(K))/binary, $\s, (print_expr(V))/binary>>;
print_map_([{K, V} | Rest]) ->
    <<(print_expr(K))/binary, $\s, (print_expr(V))/binary, $\s, (print_map_(Rest))/binary>>.

-spec escape(bin()) -> bin().
escape(String) ->
    Escaped = << <<(escape_char(C))/binary>> || <<C/utf8>> <= String >>,
    <<$", Escaped/binary, $">>.

-spec escape_char(Char :: char()) -> bin().
escape_char($")  -> <<$\\, $">>;
escape_char($\\) -> <<$\\, $\\>>;
escape_char($\n) -> <<$\\, $\n>>;
escape_char(C) -> <<C/utf8>>.
