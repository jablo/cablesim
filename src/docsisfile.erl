-module(docsisfile).

-export([parse/1, parse/2]).

parse(Data) ->
    parse(Data, []).

parse(<< >>, Acc) ->
    lists:reverse(Acc);
parse(<< Type, Length, Value:Length/binary, R/binary >>, Acc) ->
    parse(R, [{Type, Value}|Acc]).

    
