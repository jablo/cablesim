-module(dhcpserver).
-export([serve/0]).

serve() ->
    receive
        finished ->
            io:format("DHCP server finished\n");
        {discover, ClientMac, ClientPid} ->
            io:format("DHCP offer to ~p\n", [ClientMac]),
            ClientPid ! {down, {offer, ClientMac}},
            serve();
        {request, ClientMac, ClientPid} ->
            io:format("DHCP ack to ~p\n", [ClientMac]),
            ClientPid ! {down, {ack, ClientMac}},
            serve();
        {renew, ClientMac, ClientPid} ->
            io:format("DHCP re-ack to ~p\n", [ClientMac]),           
            ClientPid ! {down, {ack, ClientMac}},
            serve();
        X ->
            io:format("DHCPserver illegal msg ~p\n", [X]),
            serve()
    end.

