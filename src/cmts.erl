%%%-------------------------------------------------------------------
%%% File    : cmts 
%%% Author  : Jacob Lorensen
%%% Description : DHCP relay agent simulator
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%% Based on code for DHCP server:
%%% Created : 20 Sep 2006 by Ruslan Babayev <ruslan@babayev.com>
%%%
%%% Links
%%% http://tools.ietf.org/html/rfc3046
%%% RFC 1542. 
%%% http://www.ietf.org/internet-drafts/draft-ietf-dhc-implementation-02.txt
%%%
%%% For non-root uses:
%%% http://askubuntu.com/questions/8250/weird-issue-with-iptables-redirection
%%%-------------------------------------------------------------------
-module(cmts).

-behaviour(gen_server).

%% API
-export([start_link/2, send_packet/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% 
-import(dhcp_util, [optsearch/2, get_client_id/1, fmt_clientid/1, fmt_gateway/1, fmt_ip/1,
        fmt_hostname/1]).

-include("dhcp.hrl").
-include("simul.hrl").

-define(SERVER, ?MODULE).
-define(DHCP_SERVER_PORT, 67).
-define(DHCP_CLIENT_PORT, 68).
-define(DHCP_RELAY_PORT, 67).
-define(DHCP_SERVER_IP, {192,168,56,105}).
-define(GI_ADDRESS, {192,168,56,102}).

-record(state, {socket, server_id, cms}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ServerId, LogFile) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [ServerId, LogFile], [{debug,[trace]}]).

%% Cable modems call this to have a dhcp packet relayed to the dhcp server
send_packet(CMTS, Packet) ->
    gen_server:cast(CMTS, Packet).

%% send a stop this will end up in "handle_event"
stop(N)  -> 
    gen_fsm:send_all_state_event(N, stopit).
    

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
init([ServerId, LogFile]) ->
    error_logger:logfile({open, LogFile}),
    Options = get_sockopt(),
    case gen_udp:open(?DHCP_RELAY_PORT, Options) of
	{ok, Socket} ->
	    error_logger:info_msg("Starting DHCP releay..."),
	    {ok, #state{socket = Socket,
			server_id = ServerId,
                        cms=doubledict:new()}};
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
handle_cast(DhcpPacket, State) ->
    Socket = State#state.socket,
    {IP, Port} = get_dest(DhcpPacket),
    D = DhcpPacket#dhcp{giaddr=?GI_ADDRESS},
    Mac = DhcpPacket#dhcp.chaddr,
    error_logger:info_msg("Relaying DHCP from ~p on ~s to ~s ~s ~s",
			  [Mac,
                           fmt_ip(IP), fmt_clientid(D),
			   fmt_hostname(D), fmt_gateway(D)]),
    CableModems2 = doubledict:store(Mac, State#state.server_id, State#state.cms),
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
    error_logger:info_msg("DHVP server replied: ~p~n", [DHCP]),
    case optsearch(?DHO_DHCP_MESSAGE_TYPE, DHCP) of
	{value, MsgType} ->
	    handle_dhcp(MsgType, DHCP, State);
	false ->
	    ok
    end,
    {noreply, State};
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
handle_dhcp(_DHCPPACKET_TYPE, D, State) ->
    Mac = D#dhcp.chaddr,
    CMID = doubledict:fetch_bykey(Mac, State#state.cms),            
    error_logger:info_msg("DHCPDOFFER from ~s ~s ~s relay to ~p",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D),
                          CMID]),
    cm:receive_packet(CMID, D),
    State.

%%% Behaviour is described in RFC2131 sec. 4.1
%%% NO: look in RFC about DHCP relays
get_dest(D) when is_record(D, dhcp) ->
    {?DHCP_SERVER_IP, ?DHCP_SERVER_PORT}.

get_sockopt() ->
    [binary, {broadcast, true}].











