-module(reader).
-export([read_str/1]).
-export_type([expr/0]).

-define(token_regex, <<"[\\s ,]*(~@|[\\[\\]{}()'`~@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\\s \\[\\]{}()'\"`~@,;]*)">>). % '
-define(is_digit(C), C >= $0, C =< $9).
-define(is_open_paren(C),  C =:= <<"(">>; C =:= <<"[">>; C =:= <<"{">>).
-define(is_close_paren(C), C =:= <<")">>; C =:= <<"]">>; C =:= <<"}">>).
-define(ok(Expr, Tokens), {ok, Expr, Tokens}).
-define(err(Error), {error, Error}).

-type bin() :: unicode:unicode_binary().
-type token() :: bin().
-type tokens() :: [token()].
-type vec() :: {vector, [expr()]}.
-type mal_string() :: bin().
-type expr() :: integer() | float() | mal_string() | atom() | [expr()] | vec() | map().
-type reader_success() :: {ok, expr(), tokens()}.
-type reader_error() :: {error, iolist()}.
-type reader_result() :: reader_success() | reader_error().

-spec read_str(String :: bin()) -> {ok, expr()} | {error, iolist()}.
read_str(String) ->
    Tokens0 = re:split(String, ?token_regex),
    Tokens1 = [T || T <- Tokens0, T =/= <<>>],
    case Tokens1 of
        [] ->
            nil;
        _  ->
            case read_expr(Tokens1) of
                {ok, Expr, _}  -> {ok, Expr};
                {error, Error} -> {error, Error}
            end
    end.

-spec read_expr(Tokens :: tokens()) -> reader_result().
read_expr([]) ->
    ?err("unexpected end of line");
read_expr([<<";">> | Tokens]) ->
    ?ok(nil, Tokens);
read_expr([<<"'">> | Tokens]) ->
    wrap(quote, Tokens);
read_expr([<<"`">> | Tokens]) ->
    wrap(quasiquote, Tokens);
read_expr([<<"~">> | Tokens]) ->
    wrap(unquote, Tokens);
read_expr([<<"~@">> | Tokens]) ->
    wrap('splice-unquote', Tokens);
read_expr([<<"@">> | Tokens]) ->
    wrap(deref, Tokens);
read_expr([<<"^">> | Tokens0]) -> %"
    case read_expr(Tokens0) of
        {ok, Meta, Tokens1} ->
            case read_expr(Tokens1) of
                {ok, Expr, Tokens2} ->
                    ?ok(['with-meta', Expr, Meta], Tokens2);
                {error, Error} ->
                    ?err(Error)
            end;
        {error, Error} ->
            ?err(Error)
    end;
read_expr([P | Tokens0]) when ?is_open_paren(P)  ->
    case read_list(Tokens0, close_paren(P)) of
        {ok, List, Tokens1} ->
            Handler = list_handler(P),
            ?ok(Handler(List), Tokens1);
        {error, Error} ->
            ?err(Error)
    end;
read_expr([P | _]) when ?is_close_paren(P) ->
    ?err(["unexpected '", P, "'"]);
read_expr(Tokens) ->
    read_atom(Tokens).

-spec wrap(Wrap :: expr(), Tokens :: tokens()) -> reader_result().
wrap(Wrap, Tokens0) ->
    case read_expr(Tokens0) of
        {ok, Expr, Tokens1} ->
            ?ok([Wrap, Expr], Tokens1);
        {error, Error} ->
            ?err(Error)
    end.

-spec read_list(Tokens :: tokens(), ClosingParen :: token()) -> reader_result().
read_list([], P) ->
    ?err(["expected '", P, "', but got end of line"]);
read_list([P | Tokens], P) ->
    ?ok([], Tokens);
read_list(Tokens0, P) ->
    case read_expr(Tokens0) of
        {ok, Expr, Tokens1} ->
            case Tokens1 of
                [P | Tokens2] ->
                    ?ok([Expr], Tokens2);
                _ ->
                    case read_list(Tokens1, P) of
                        {ok, Rest, Tokens2} ->
                            ?ok([Expr | Rest], Tokens2);
                        {error, Error} ->
                            ?err(Error)
                    end
            end;
        {error, Error} ->
            ?err(Error)
    end.

-spec read_atom(Tokens :: tokens()) -> reader_result().
read_atom([<<$-, C/utf8, Rest/binary>> = Bin | Tokens]) when ?is_digit(C) ->
    read_integer(Rest, Bin, Tokens);
read_atom([<<C/utf8, Rest/binary>> = Bin | Tokens]) when ?is_digit(C) ->
    read_integer(Rest, Bin, Tokens);
read_atom([<<$", String/binary>> | Tokens]) ->
    read_string(String, Tokens);
read_atom([Symbol | Tokens]) ->
    read_symbol(Symbol, Tokens).

-spec read_integer(Bin :: bin(), Orig :: token(), Tokens :: tokens()) -> reader_result().
read_integer(<<C/utf8, Rest/binary>>, Orig, Tokens) when ?is_digit(C) ->
    read_integer(Rest, Orig, Tokens);
read_integer(<<$., Rest/binary>>, Orig, Tokens) ->
    read_float(Rest, Orig, Tokens);
read_integer(<<>>, Orig, Tokens) ->
    ?ok(binary_to_integer(Orig), Tokens);
read_integer(_, Orig, Tokens) ->
    read_symbol(Orig, Tokens).

-spec read_float(Bin :: bin(), Orig :: token(), Tokens :: tokens()) -> reader_result().
read_float(<<C/utf8, Rest/binary>>, Orig, Tokens) when ?is_digit(C) ->
    read_float(Rest, Orig, Tokens);
read_float(<<>>, Orig, Tokens) ->
    ?ok(binary_to_float(Orig), Tokens);
read_float(_, Orig, Tokens) ->
    read_symbol(Orig, Tokens).

-spec read_string(Bin :: bin(), Tokens :: tokens()) -> reader_result().
read_string(<<$\\, $", Rest/binary>>, Tokens0) ->
    case read_string(Rest, Tokens0) of
        {ok, String, Tokens1} ->
            ?ok(<<$", String/binary>>, Tokens1);
        {error, Error} ->
            ?err(Error)
    end;
read_string(<<C/utf8, Rest/binary>>, Tokens0) when C =/= $" ->
    case read_string(Rest, Tokens0) of
        {ok, String, Tokens1} ->
            ?ok(<<C/utf8, String/binary>>, Tokens1);
        {error, Error} ->
            ?err(Error)
    end;
read_string(<<$", _/binary>>, Tokens) ->
    ?ok(<<>>, Tokens);
read_string(<<>>, _)  ->
    ?err("expected '\"', but got end of line").

-spec read_symbol(Symbol :: token(), Tokens :: tokens()) -> reader_result().
read_symbol(Symbol, Tokens) -> ?ok(binary_to_atom(Symbol, utf8), Tokens).


close_paren(<<"(">>) -> <<")">>;
close_paren(<<"[">>) -> <<"]">>;
close_paren(<<"{">>) -> <<"}">>.

list_handler(<<"(">>) -> fun(X) -> X end;
list_handler(<<"[">>) -> fun vector_handler/1;
list_handler(<<"{">>) -> fun map_handler/1.

vector_handler(Vec) -> {vector, Vec}.

map_handler(Map) -> maps:from_list(propertize(Map)).

propertize([]) -> [];
propertize([K, V | Rest]) -> [{K, V} | propertize(Rest)].
