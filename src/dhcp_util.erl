%%%-------------------------------------------------------------------
%%% File    : dhcp_lib.erl
%%% Author  : Ruslan Babayev <ruslan@babayev.com>
%%% Description : 
%%%
%%% Created : 17 Apr 2006 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(dhcp_util).

%% API
-export([optsearch/2, get_client_id/1, fmt_clientid/1, fmt_gateway/1, fmt_ip/1,
        fmt_hostname/1]).
-include("dhcp.hrl").

optsearch(Option, D) when is_record(D, dhcp) ->
    case lists:keysearch(Option, 1, D#dhcp.options) of
	{value, {Option, Value}} ->
	    {value, Value};
	false ->
	    false
    end.
    
get_client_id(D) when is_record(D, dhcp) ->
    case optsearch(?DHO_DHCP_CLIENT_IDENTIFIER, D) of
        {value, ClientId} ->
	    ClientId;
	false ->
	    D#dhcp.chaddr
    end.

fmt_clientid(D) when is_record(D, dhcp) ->
    fmt_clientid(get_client_id(D));
fmt_clientid([_T, E1, E2, E3, E4, E5, E6]) ->
    fmt_clientid({E1, E2, E3, E4, E5, E6});
fmt_clientid({E1, E2, E3, E4, E5, E6}) ->
    lists:flatten(
      io_lib:format("~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b",
	     [E1, E2, E3, E4, E5, E6])).

fmt_gateway(D) when is_record(D, dhcp) ->
    case D#dhcp.giaddr of
	{0, 0, 0, 0} -> [];
	IP           -> lists:flatten(io_lib:format("via ~s", [fmt_ip(IP)]))
    end.

fmt_hostname(D) when is_record(D, dhcp) ->
    case optsearch(?DHO_HOST_NAME, D) of
        {value, Hostname} ->
            lists:flatten(io_lib:format("(~s)", [Hostname]));
	false ->
	    []
    end.

fmt_ip({A1, A2, A3, A4}) ->
    io_lib:format("~w.~w.~w.~w", [A1, A2, A3, A4]).
