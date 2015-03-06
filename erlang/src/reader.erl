-module(reader).
-export([read_str/1]).

-define(is_digit(C), C >= $0, C =< $9).
-define(is_open_paren(C),  C =:= <<"(">>; C =:= <<"[">>; C =:= <<"{">>).
-define(is_close_paren(C), C =:= <<")">>; C =:= <<"]">>; C =:= <<"}">>).

-define(token_regex, <<"[\\s ,]*(~@|[\\[\\]{}()'`~@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\\s \\[\\]{}()'\"`~@,;]*)">>).


tokenize(Str) ->
    Tokens = re:split(Str, ?token_regex),
    [T || T <- Tokens, T =/= <<"">>].


read_str(Str) ->
    Tokens = tokenize(Str),
    case Tokens of
        [] -> nil;
        _  -> {Expr, _} = read_expr(Tokens), Expr
    end.


read_symbol(Sym) -> binary_to_atom(Sym, utf8).


read_atom([<<"nil">>                    | Tokens]) -> {nil, Tokens};
read_atom([<<"true">>                   | Tokens]) -> {true, Tokens};
read_atom([<<"false">>                  | Tokens]) -> {false, Tokens};
read_atom([<<$\:, Keyword/binary>>      | Tokens]) -> {{keyword, binary_to_atom(Keyword, utf8)}, Tokens};
read_atom([<<$", String/binary>>        | Tokens]) -> {read_string(String), Tokens};
read_atom([<<$-, C, Rest/binary>> = Bin | Tokens]) when ?is_digit(C) -> {read_integer(Rest, Bin), Tokens};
read_atom([<<C, Rest/binary>>     = Bin | Tokens]) when ?is_digit(C) -> {read_integer(Rest, Bin), Tokens};
read_atom([Atom                         | Tokens]) -> {read_symbol(Atom), Tokens}.


read_string(<<$\\, $", Rest/binary>>) -> <<$\\, $", (read_string(Rest))/binary>>;
read_string(<<C, Rest/binary>>) when C =/= $" -> <<C, (read_string(Rest))/binary>>;
read_string(<<$", _/binary>>) -> <<"">>;
read_string(<<>>)  -> throw("expected '\"', but got end of line").


read_integer(<<C,  Rest/binary>>, Orig) when ?is_digit(C) -> read_integer(Rest, Orig);
read_integer(<<$., Rest/binary>>, Orig) -> read_float(Rest, Orig);
read_integer(<<"">>,              Orig) -> binary_to_integer(Orig);
read_integer(_,                   Orig) -> read_symbol(Orig).


read_float(<<C,  Rest/binary>>, Orig) when ?is_digit(C) -> read_float(Rest, Orig);
read_float(<<"">>,              Orig) -> binary_to_float(Orig);
read_float(_,                   Orig) -> read_symbol(Orig).


read_list([], P) ->
    throw(["expected '", P, "', but got end of line"]);

read_list([P | Tokens], P) ->
    {[], Tokens};

read_list(Tokens0, P) ->
    {Expr, Tokens1} = read_expr(Tokens0),
    case Tokens1 of
        [P | Tokens2] ->
            {[Expr], Tokens2};
        Tokens3 ->
            {List, Tokens4} = read_list(Tokens3, P),
            {[Expr | List], Tokens4}
    end.


close_paren(<<"(">>) -> <<")">>;
close_paren(<<"[">>) -> <<"]">>;
close_paren(<<"{">>) -> <<"}">>.


list_handler(<<"(">>) -> fun (X) -> X end;
list_handler(<<"[">>) -> fun vector_handler/1;
list_handler(<<"{">>) -> fun map_handler/1.

vector_handler(Vec) -> {vector, Vec}.

map_handler(Map) -> maps:from_list(propertize(Map)).

propertize([]) -> [];
propertize([K, V | Rest]) -> [{K, V} | propertize(Rest)].


wrap(Wrap, Tokens0) ->
    {Expr, Tokens1} = read_expr(Tokens0),
    {[Wrap, Expr], Tokens1}.


read_expr([])                  -> throw("unexpected end of line");
read_expr([<<";">>  | Tokens]) -> {nil, Tokens};
read_expr([<<"'">>  | Tokens]) -> wrap(quote, Tokens);
read_expr([<<"`">>  | Tokens]) -> wrap(quasiquote, Tokens);
read_expr([<<"~">>  | Tokens]) -> wrap(unquote, Tokens);
read_expr([<<"~@">> | Tokens]) -> wrap('splice-unquote', Tokens);
read_expr([<<"@">>  | Tokens]) -> wrap(deref, Tokens);
read_expr([<<"^">>  | Tokens0]) ->
    {Meta, Tokens1} = read_expr(Tokens0),
    {Expr, Tokens2} = read_expr(Tokens1),
    {['with-meta', Expr, Meta], Tokens2};
read_expr([P | Tokens0]) when ?is_open_paren(P)  ->
    {List, Tokens1} = read_list(Tokens0, close_paren(P)),
    Handler = list_handler(P),
    {Handler(List), Tokens1};
read_expr([P | _]) when ?is_close_paren(P) ->
    throw(["unexpected '", P, "'"]);
read_expr(Tokens) ->
    read_atom(Tokens).
