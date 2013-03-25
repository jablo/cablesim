%%%-------------------------------------------------------------------
%%% @author Jacob Lorensen
%%% @copyright 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%% @doc DHCP relay agent simulator
%%%
%%% For non-root uses:
%%% http://askubuntu.com/questions/8250/weird-issue-with-iptables-redirection
%%% @end
%%%-------------------------------------------------------------------
-module(cmts).
-include("debug.hrl").
%-compile([debug_info, export_all]).

-behaviour(gen_server).

%% Public API
-export([start_link/5, send_packet/3, stop/1, reboot/1, disconnect/2, connect/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% 
-import(dhcp_util, [optsearch/2, get_client_id/1, fmt_clientid/1, fmt_gateway/1, fmt_ip/1,
        fmt_hostname/1]).

-include("dhcp.hrl").

-define(DHCP_SERVER_PORT, 67).
-define(DHCP_CLIENT_PORT, 68).
-define(DHCP_RELAY_PORT, 67).

-record(state, {socket, cmts, dhcp_helpers, giaddress, cms}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(ServerId, GiAddress, LogFile) -> 
%%                                 {ok,Pid} | ignore | {error,Error}
%% Description: Starts an instance of the Cmts server
%%--------------------------------------------------------------------
start_link(Cmts, Intf, GiAddress, Netmask, DhcpHelpers) ->
    io:format("CMTS start-link ~p ~p ~p ~n", [Cmts, GiAddress, DhcpHelpers]),
    X = gen_server:start_link({local, Cmts}, ?MODULE,
                              [Cmts, Intf, GiAddress, Netmask, DhcpHelpers], 
                              []), %{debug,[trace]}{debug,[log]}
    ?debug("cmts:start_link(~p, ~p, ~p, ~p, ~p) ~n   returns ~p~n", 
           [Cmts, Intf, GiAddress, Netmask, DhcpHelpers, X]),
    X. %{debug,[trace]}{debug,[log]}


%% @doc
%% Cable modems call this to have a dhcp packet relayed to the dhcp server
%% @end
send_packet(CMTS, Packet, CmId) ->
    gen_server:cast(CMTS, {Packet, CmId}).

%% @doc
%% Cable modem call this to disconnect from the cmts
%% @end
disconnect(Cmts, Mac) ->
    gen_server:cast(Cmts, {disconnect, Mac}).

%% @doc
%% Cable modems can call this to establish connection explicitly
%% @end
connect(_Cmts, _CmId) ->
    ok. % noop, since sending packets does implicit connect

%% @doc
%% reset cmts (ie, reset all connected cable modems)
%% @end
reboot(Cmts) ->
    gen_server:cast(Cmts, {reboot}).

%% @doc
%% Stop the cmts server
%% @end
stop(CMTS) ->
    gen_server:cast(CMTS, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initialize an instance of the cmts server process
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%%--------------------------------------------------------------------
init([ServerId, Intf, GiAddress, Netmask, DhcpHelpers]) ->
    netconfig:network_config(Intf, GiAddress, Netmask),
    Options = get_sockopt(GiAddress),
    case gen_udp:open(?DHCP_RELAY_PORT, Options) of
	{ok, Socket} ->
	    error_logger:info_msg("Starting CMTS ~p~n", [ServerId]),
	    {ok, #state{socket = Socket,
			cmts = ServerId,
                        dhcp_helpers = DhcpHelpers,
                        giaddress = GiAddress,
                        cms=dict:new()}};
	{error, Reason} ->
	    error_logger:error_msg("Cannot open udp port ~w",
				   [?DHCP_RELAY_PORT]),
	    {stop, Reason}
    end.



%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% This will handle packets sent from cable modems to forward to the 
%% dhcp server
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    dict:map (fun (_,V) -> cm:stop(V) end, State#state.cms),
    {stop, normal, State};
% External event: reboot a cmts - reboots all attached cable modems
handle_cast({reboot}, State) ->
    dict:map (fun (_,V) -> cm:reset(V) end, State#state.cms),
    {noreply, State};
% External event: disconnect a cable modem
handle_cast({disconnect, Mac}, State) ->
    error_logger:info_msg("Disconnecting ~p~n", [Mac]),
    {noreply, State#state{cms=dict:erase(Mac, State#state.cms)}};
% External event: forward a dhcp packet from an attached cable modem
handle_cast({DhcpPacket = #dhcp{}, CmId}, State = #state{dhcp_helpers = IPs, socket = Socket}) ->
    D = DhcpPacket#dhcp{giaddr=State#state.giaddress},
    Mac = DhcpPacket#dhcp.chaddr,
    CableModems2 = dict:store(Mac, CmId, State#state.cms),
    State2 = State#state{cms=CableModems2},
    lists:foreach(
      fun (IP) -> gen_udp:send(Socket, IP, ?DHCP_SERVER_PORT, dhcp_lib:encode(D)) end,
      IPs),
    {noreply, State2}.    



%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% Received UDP packets from the DHCP server to forward to cable modems
%%--------------------------------------------------------------------
handle_info({udp, _Socket, _IP, _Port, Packet}, State) ->
    DHCP = dhcp_lib:decode(Packet),
    %error_logger:info_msg("DHCP server replied: ~p~n", [DHCP]),
    case optsearch(?DHO_DHCP_MESSAGE_TYPE, DHCP) of
	{value, MsgType} ->
	    handle_dhcp(MsgType, DHCP, State);
	false ->
            handle_dhcp(0, DHCP, State)
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    error_logger:logfile(close),
    gen_udp:close(State#state.socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% The DHCP message handler - forward to Cable modem client
%%%-------------------------------------------------------------------
handle_dhcp(_DHCPPACKET_TYPE, D = #dhcp{chaddr=Mac}, State) ->
    CMID = dict:fetch(Mac, State#state.cms),            
    cm:receive_packet(CMID, D),
    {noreply, State}.

get_sockopt(IP) ->
    [binary, {broadcast, true}, {ip, IP}].
