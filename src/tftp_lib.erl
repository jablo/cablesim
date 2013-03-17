%%%-------------------------------------------------------------------
%%% File    : tftp_lib.erl
%%% Author  : Jacob Lorensen <jacoblorensen@gmail.com>
%%% Description : Library routines for tftp protocol
%%%
%%% Created : 17 Mar 2013 by Jacob Lorensen <jacoblorensen@gmail.com>
%%%-------------------------------------------------------------------
-module(tftp_lib).

%% API
-export([decode/1, encode/1, t1/0]).

-import(lists, [keymember/3, keysearch/3, keyreplace/4]).

-include_lib("eunit/include/eunit.hrl").
-include("tftp.hrl").

%% @doc
%% encode a tftp packet as binary
%% @end
encode(#tftp_request{opcode=Opcode, filename=Filename, mode=Mode}) ->
    << Opcode:16/big, 
       (list_to_binary(Filename))/binary, ?C_EOS:8, 
       (list_to_binary(Mode))/binary, ?C_EOS:8 >>;
encode(#tftp_data{block=Block, data=Data}) ->
    << ?OC_DATA:16/big, Block:16/big, (length(Data)):16/big, Data/binary >>;
encode(#tftp_ack{block=Block}) ->
    << ?OC_ACK:16/big, Block:16/big >>;
encode(#tftp_err{errcode=Err, errstring=Msg}) ->
    << ?OC_ERR:16/big, Err:16/big, Msg/binary, ?C_EOS:8 >>.


%% @doc
%% decode a tftp packet as binary
%% @end
decode(<<Opcode:16/big, FilenameAndMode/binary>> )
  when Opcode =:= ?OC_RRQ orelse Opcode =:= ?OC_WRQ ->
    {Filename, Rest } = decode_asciiz(FilenameAndMode),
    {Mode, _} = decode_asciiz(Rest),
    #tftp_request{opcode=Opcode,filename=Filename, mode=Mode};
decode(<< ?OC_DATA:16/big, Block:16/big, Data/binary>>) ->
    #tftp_data{block=Block, data=Data};
decode(<< ?OC_ACK:16/big, Block:16/big >>) ->
    #tftp_ack{block=Block};
decode(<< ?OC_ERR:16/big, Err:16/big, Msg/binary >>) ->
    {MsgStr, _} = decode_asciiz(Msg),
    #tftp_err{errcode=Err, errstring=MsgStr}.

decode_asciiz(B) ->
    decode_asciiz(B, []).
decode_asciiz(<<?C_EOS:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_asciiz(<<B:8, Rest/binary>>, Acc) ->
    decode_asciiz(Rest, [B|Acc]).

encode_test_() ->
    P = #tftp_request{opcode=?OC_RRQ, filename="config.cfg", mode="binary"},
    R = << ?OC_RRQ,  (list_to_binary("config.cfg"))/binary, ?C_EOS, 
                     (list_to_binary("binary"))/binary, ?C_EOS >>,
    io:format("HEJ FROM TEST P:~p ~n R:~p ~n", [P, R]),
    [?_assert(encode(P) =:= R)].


t1() ->
    P = #tftp_request{opcode=?OC_RRQ, filename="config.cfg", mode="binary"},
    R = << ?OC_RRQ:16,  (list_to_binary("config.cfg"))/binary, ?C_EOS:8, 
                     (list_to_binary("binary"))/binary, ?C_EOS:8 >>,
    io:format("HEJ FROM TEST P:~p ~n R:~p ~n", [P, R]),
    E = encode(P),
    io:format("Encoded: ~p~n", [encode(P)]),
    E.
