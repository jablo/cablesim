-module(doubledict).
-include_lib("eunit/include/eunit.hrl").
-export([new/0, store/3, fetch_bykey/2, fetch_byval/2, erase_bykey/2, erase_byval/2]).

new() ->
    {dict:new(), dict:new()}.

store(K,V, {D1, D2}) ->
    {dict:store(K, V, D1), dict:store(V, K, D2)}.

fetch_bykey(K, {D1, _}) ->
    dict:fetch(K, D1).

fetch_byval(V, {_, D2}) ->
    dict:fetch(V, D2).

erase_bykey(K, {D1, D2}) ->
    {dict:erase(K,D1), dict:erase(dict:fetch(K,D1), D2)}.

erase_byval(V,{D1, D2}) ->
    {dict:erase(dict:fetch(V,D2),D1), dict:erase(V,D2)}.

%
% Unit Tests
%


insert_test() ->
    D=store(a, 1, new()),
    ?assert(fetch_bykey(a, D) =:= 1 andalso fetch_byval(1, D) =:= a).
