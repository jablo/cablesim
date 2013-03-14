%%%-------------------------------------------------------------------
%%% File    : cmts 
%%% Author  : Jacob Lorensen
%%% Description : DHCP relay agent simulator
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%
%%%
%%% For non-root uses:
%%% http://askubuntu.com/questions/8250/weird-issue-with-iptables-redirection
%%%-------------------------------------------------------------------
-module(cmts).
%-compile([debug_info, export_all]).

-behaviour(gen_server).

%% Public API
-export([start_link/3, send_packet/3, stop/1, reboot/1, disconnect/2, connect/2]).

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
-define(DHCP_SERVER_IP, {192,168,56,105}).

-record(state, {socket, cmts, dhcp_serverip, giaddress, cms}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(ServerId, GiAddress, LogFile) -> 
%%                                 {ok,Pid} | ignore | {error,Error}
%% Description: Starts an instance of the Cmts server
%%--------------------------------------------------------------------
start_link(Cmts, GiAddress, DhcpServerIP) ->
    gen_server:start_link({local, Cmts}, ?MODULE,
			  [Cmts, GiAddress, DhcpServerIP], 
                          [{debug,[log]}]). %{debug,[trace]}

%% Cable modems call this to have a dhcp packet relayed to the dhcp server
send_packet(CMTS, Packet, CmId) ->
    gen_server:cast(CMTS, {Packet, CmId}).

%% Cable modem call this to disconnect from the cmts
disconnect(Cmts, CmId) ->
    gen_server:cast(Cmts, {disconnect, CmId}).

%% Cable modems can call this to establish connection explicitly
connect(_Cmts, _CmId) ->
    ok. % noop, since sending packets does implicit connect

%% reset cmts (ie, reset all connected cable modems)
reboot(Cmts) ->
    gen_server:cast(Cmts, {reboot}).

%% 
stop(CMTS) ->
    gen_server:cast(CMTS, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ServerId, GiAddress, DhcpServerIP]) ->
    Options = get_sockopt(),
    case gen_udp:open(?DHCP_RELAY_PORT, Options) of
	{ok, Socket} ->
	    error_logger:info_msg("Starting CMTS ~p~n", [ServerId]),
	    {ok, #state{socket = Socket,
			cmts = ServerId,
                        dhcp_serverip = DhcpServerIP,
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
handle_cast({disconnect, CmId}, State) ->
    error_logger:info_msg("Disconnecting ~p~n", [CmId]),
    {noreply, State#state{cms=dict:erase(CmId, State#state.cms)}};
% External event: forward a dhcp packet from an attached cable modem
handle_cast({DhcpPacket = #dhcp{}, CmId}, State) ->
    Socket = State#state.socket,
    {IP, Port} = get_dest(DhcpPacket),
    D = DhcpPacket#dhcp{giaddr=State#state.giaddress},
    Mac = DhcpPacket#dhcp.chaddr,
    %error_logger:info_msg("Relaying DHCP from ~p ~s~n", [CmId, fmt_clientid(D)]),
    CableModems2 = dict:store(Mac, CmId, State#state.cms),
    State2 = State#state{cms=CableModems2},
    gen_udp:send(Socket, IP, Port, dhcp_lib:encode(D)),
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

get_dest(D) when is_record(D, dhcp) ->
    {?DHCP_SERVER_IP, ?DHCP_SERVER_PORT}.

get_sockopt() ->
    [binary, {broadcast, true}].
