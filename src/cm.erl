%%%-------------------------------------------------------------------
%%% File    : cm
%%% Author  : Jacob Lorensen
%%% Description : Cable modem simulator
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%
%%% Simulating a cable modem essentially consists of forwarding packets from
%%% cable modem sub-components: DHCP client, TFTP client, ToD client.
%%% and other optional sub-components: eMTA, CPE
%%% The latter (eMTA and CPE) need special treatment in that DHCP option 82 
%%% must be appended to any DHCP packet coming from the device, but not from
%%% TFTP packets for example.
%%%
%%% Links
%%% http://tools.ietf.org/html/rfc3046
%%% RFC 1542. 
%%% http://www.ietf.org/internet-drafts/draft-ietf-dhc-implementation-02.txt
%%%
%%% For non-root uses:
%%% http://askubuntu.com/questions/8250/weird-issue-with-iptables-redirection
%%%-------------------------------------------------------------------
-module(cm).
%-compile([debug_info, export_all]).
-behaviour(gen_server).

%% public api
-export([stop/1, start_link/1, start_link/2, 
         send_packet/2, relay_packet/2, receive_packet/2, linkstate/2,
         poweron/1, poweroff/1, reset/1, 
         connect_cmts/2, disconnect_cmts/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("dhcp.hrl").
-include("device.hrl").

-record(state, {
          linkstate=offline,     % link state obtained from the built in dhcp client
          device,                % cable modem's mac address, dhcp template, and built in sub servers
          devices_behind         % list of devices behind this cable modem
         }).

%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
%% @spec start_link(Device, BehindDevs) -> {ok,Pid} | ignore | {error,Error}
%% @end
start_link(Device) ->
    start_link(Device, []).
start_link(Device = #device{}, BehindDevs) ->
    gen_server:start_link({local, Device#device.server_id}, ?MODULE,
			  [Device, BehindDevs], [{debug,[]}]). %

stop(CmId) ->
    gen_server:cast(CmId, {stop}).

%% @doc
%% Callback for the cable modem's dhcp_client to signal when IP link state
%% to the cmts is up/down.
%% @see relay_packet
%% @end
linkstate(CmId, online) ->
    gen_server:cast(CmId, {linkstate, online});
linkstate(CmId, offline) ->
    gen_server:cast(CmId, {linkstate, offline}).

%% @doc
%% Cable modem native components modems call this to have a network packet 
%% sent to the cmts (and further onwards) to the dhcp server. Send_packet
%% does not do any packet rewrites, whereas relay_packet does option-82 for
%% dhcp packets
%% @see relay_packet
%% @end
send_packet(CmId, Packet) ->
    gen_server:cast(CmId, {send_packet, Packet}).

%% @doc
%% Devices behind the cable modem (mta, cpe) call this to have a network packet 
%% relayed to the cmts (and further onwards) to the dhcp server. relay_packet
%% appends dhcp option 82 for outgoing dhcp packets
%% @see send_packet
%% @end
relay_packet(CmId, Packet) ->
    gen_server:cast(CmId, {relay_packet, Packet}).


%% @doc
%% Cmts process calls this to deliver a network packet to the cable modem
%% Packet types: DHCP, TFTP, ToD
%% @end
receive_packet(CmId, Packet) ->
    gen_server:cast(CmId, {receive_packet, Packet}).

%%% External events (human)

%% @doc
%% External cable modem control - power on a modem
%% @end
poweron(N) ->
    gen_server:cast(N, {poweron}).

%% @doc
%% External cable modem control - power off a modem
%% @end
poweroff(N) ->
    gen_server:cast(N, {poweroff}).

%% @doc
%% External cable modem control - reset a modem
%% @end
reset(N) ->
    gen_server:cast(N, {reset}).

%% @doc
%% External cable modem control - Connect to a (new) cmts
%% @end
connect_cmts(CmId, CmtsId) ->
    gen_server:cast(CmId, {connect, CmtsId}).

%% @doc
%% External cable modem control - disconnect from a (new) cmts
%% @end
disconnect_cmts(CmId) ->
    gen_server:cast(CmId, {disconnect}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the cable modem server
%%--------------------------------------------------------------------
init([Device, BehindDevs | OptionalState]) when is_record(Device, device) ->
    error_logger:info_msg("Starting Cable Modem ~p ~p~n", 
                          [Device#device.server_id, Device#device.mac]),
    InitialState = {ok, #state{device = Device,
                               devices_behind = BehindDevs,
                               linkstate=offline}},
    case OptionalState of
        [] -> InitialState;
        [sdfjhdsf] -> handle_poweron_reset(InitialState)
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
%%--------------------------------------------------------------------
handle_cast({stop}, StateData) ->
    {stop, normal, StateData};   %% tell it to stop
% External event: tell the cable modem to power off
handle_cast({poweroff}, StateData = #state{device = Device, devices_behind = DevBehind}) ->
    Device = StateData#state.device,
    dhcp_client:poweroff(Device#device.dhcp_client),
    lists:foreach(fun (D) -> dhcp_client:poweroff(D#device.dhcp_client) end,
                  DevBehind),
    cmts:disconnect(Device#device.upstream_id, Device#device.mac),
    {noreply, StateData};
% External event: tell the cable modem to power off
handle_cast({reset}, StateData = #state{device = Device, devices_behind = DevBehind}) ->
    dhcp_client:reset(Device#device.dhcp_client),
    lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                  DevBehind),
    {noreply, StateData};
% External event: tell the cable modem to power on
handle_cast({poweron}, StateData = #state{device = Device}) ->
    dhcp_client:poweron(Device#device.dhcp_client),
    {noreply, StateData};
% External event: tell the cable modem to (re-)connect to a (different) cmts
handle_cast({connect, CmtsId}, StateData = #state{device = Device, devices_behind = DevBehind}) ->
    if Device#device.upstream_id =:= undefined ->
            Device2 = Device#device{upstream_id = CmtsId},
            dhcp_client:reset(Device2#device.dhcp_client),
            lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                          DevBehind),
            {noreply, StateData#state{device = Device2}};
       CmtsId =:= Device#device.upstream_id ->
            {noreply, StateData};
       true ->
            cmts:disconnect(Device#device.upstream_id, Device#device.mac),
            dhcp_client:reset(Device#device.dhcp_client),
            lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                          DevBehind),
            Device2 = Device#device{upstream_id=CmtsId},
            {noreply, StateData#state{device=Device2}}
    end;
% External event: tell the cable modem we're not connected to a cmts
handle_cast({disconnect}, StateData = #state{device = Device, devices_behind = DevBehind}) ->
    cmts:disconnect(Device#device.upstream_id, Device#device.mac),
    dhcp_client:reset(Device#device.dhcp_client),
    lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                  DevBehind),
    Device2 = Device#device{upstream_id=undefined},
    {noreply, StateData#state{device = Device2}};
% handles DHCP client link status feedback
handle_cast({linkstate, L}, StateData) ->
    handle_linkstate(L, StateData);
% handles cable modem client network communication
handle_cast({send_packet, P}, StateData = #state{}) ->
    handle_send_packet(P, StateData);
% handles embedded device (mta, cpe) network communication
handle_cast({relay_packet, P}, StateData = #state{}) ->
    handle_relay_packet(P, StateData);
% handles network packets received from cmts
handle_cast({receive_packet, P}, StateData) ->
    handle_receive_packet(P, StateData).

%%
%% Handle linkstate changes by forwarding the state change to child processes
%%
handle_linkstate(offline, StateData = #state{linkstate=Lold}) ->
    case Lold of
        offline ->
            {noreply, StateData};
        online ->
            lists:foreach(fun (D) -> dhcp_client:poweroff(D#device.dhcp_client) end,
                          StateData#state.devices_behind),
            {noreply, StateData#state{linkstate=offline}}
    end;            
handle_linkstate(online, StateData = #state{linkstate=Lold}) ->
    case Lold of
        online ->
            {noreply, StateData};
        offline ->
            lists:foreach(fun (D) -> dhcp_client:poweron(D#device.dhcp_client) end,
                          StateData#state.devices_behind),
            {noreply, StateData#state{linkstate=online}}
    end.

%%
%% route packet received to correct module
%% perform any necessary packet translation 
%%
handle_receive_packet(P = #dhcp{chaddr=ChAddr}, 
                      StateData = #state{device = Device, devices_behind = BehindDevs}) ->
    lists:foreach(fun (_Dev = #device{dhcp_client=Process, mac=Mac}) ->
                          if Mac =:= ChAddr ->
                                  dhcp_client:receive_packet(Process, P);
                             true -> ok
                          end
                  end,
                  [Device | BehindDevs]),
    {noreply, StateData}.

%%
%% send a packet from the cable modem to upstream cmts
%%
handle_send_packet(_P, StateData = #state{device = Device}) 
  when Device#device.upstream_id =:= undefined ->
    dhcp_client:poweroff(Device#device.dhcp_client),
    {noreply, StateData};
handle_send_packet(P = #dhcp{options = Options}, StateData = #state{device = Device}) ->
    Cmts = Device#device.upstream_id,
    Me = Device#device.server_id,
    Op2 = Options ++ 
        [{?DHO_DHCP_AGENT_OPTIONS, [16#02, 16#06 | tuple_to_list(Device#device.mac)]}],
    cmts:send_packet(Cmts, P#dhcp{options = Op2}, Me),
    {noreply, StateData}.

%%
%% relay a network packet from a device behind the cable modem to the upstream cmts
%%
handle_relay_packet(_, StateData = #state{device = Device})
  when Device#device.upstream_id =:= undefined ->
    {noreply, StateData};
handle_relay_packet(_, StateData = #state{linkstate=offline}) ->
    {noreply, StateData};
handle_relay_packet(P = #dhcp{options = Options}, StateData = #state{device = Device}) ->
    Cmts = Device#device.upstream_id,
    Me = Device#device.server_id,
    % replace, add option-82 to dhcp packets; leave others alone
    Op2 = Options ++ 
        [{?DHO_DHCP_AGENT_OPTIONS, [16#01,16#04,16#00,16#04,16#07,16#7a, 
                                    16#02, 16#06 | tuple_to_list(Device#device.mac)]}],
    cmts:send_packet(Cmts, P#dhcp{options = Op2}, Me),
    {noreply, StateData}.

%% @doc Handle poweron-reset. This works equally well for initially starting
%% the cable modem and for re-starting the cable modem in case of process
%% failure. We can call reset first, then poweron because poweron is ignored if
%% the dhcp clients are already powered on, while reset if ignored if
%% the dhcp clients are powered off. Further, initially the processes don't exist
%% and thus these messages are actually ignored.
%% @end
handle_poweron_reset(StateData = #state{device = Device, devices_behind = DevBehind}) ->
    cm:poweron(Device#device.server_id),
    dhcp_client:reset(Device#device.dhcp_client),
    dhcp_client:poweron(Device#device.dhcp_client),
    lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                  DevBehind),
    lists:foreach(fun (D) -> dhcp_client:poweron(D#device.dhcp_client) end,
                  DevBehind),
    StateData.
    

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State = #state{device=Device}) ->
    dhcp_client:stop(Device#device.dhcp_client),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% Received UDP packets from the DHCP server to forward to cable modems
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
