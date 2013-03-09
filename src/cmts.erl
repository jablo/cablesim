%%%-------------------------------------------------------------------
%%% File    : cmts 
%%% Author  : Jacob Lorensen
%%% Author  : Ruslan Babayev <ruslan@babayev.com>
%%% Description : DHCP relay agent simulator
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%% based on Google code
%%% Created : 20 Sep 2006 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(cmts).

-behaviour(gen_server).

%% API
-export([start_link/2, send_packet/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-include("dhcp.hrl").
-include("simul.hrl").

-define(SERVER, ?MODULE).
-define(DHCP_SERVER_PORT, 67).
-define(DHCP_CLIENT_PORT, 10068).
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
    case gen_udp:open(?DHCP_CLIENT_PORT, Options) of
	{ok, Socket} ->
	    error_logger:info_msg("Starting DHCP releay..."),
	    {ok, #state{socket = Socket,
			server_id = ServerId,
                        cms=doubledict:new()}};
	{error, Reason} ->
	    error_logger:error_msg("Cannot open udp port ~w",
				   [?DHCP_CLIENT_PORT]),
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
    error_logger:info_msg("Relaying DHCP on ~s to ~s ~s ~s",
			  [fmt_ip(IP), fmt_clientid(D),
			   fmt_hostname(D), fmt_gateway(D)]),
    gen_udp:send(Socket, IP, Port, dhcp_lib:encode(D)),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% Received UDP packets from the DHCP server to forward to cable modems
%%--------------------------------------------------------------------
handle_info({udp, _Socket, _IP, _Port, Packet}, State) ->
    io:format("handle_indfo udp  ~p~n", [Packet]),
    DHCP = dhcp_lib:decode(Packet),
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
handle_dhcp(?DHCPOFFER, D, State) ->
    error_logger:info_msg("DHCPDOFFER from ~s ~s ~s",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D)]),
    State;

handle_dhcp(?DHCPACK, D, State) ->
    error_logger:info_msg("DHCPDACK from ~s ~s ~s",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D)]),
    State;

handle_dhcp(?DHCPNAK, D, State) ->
    error_logger:info_msg("DHCPNAK from ~s ~s ~s",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D)]),
    State;

handle_dhcp(MsgType, _D, _State) ->
    error_logger:error_msg("Invalid DHCP message type ~p", [MsgType]).

%%% Behaviour is described in RFC2131 sec. 4.1
%%% NO: look in RFC about DHCP relays
get_dest(D) when is_record(D, dhcp) ->
    {?DHCP_SERVER_IP, ?DHCP_SERVER_PORT}.

%is_broadcast(D) when is_record(D, dhcp) ->
%    (D#dhcp.flags bsr 15) == 1.

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

get_sockopt() ->
    [binary, {broadcast, true}].











