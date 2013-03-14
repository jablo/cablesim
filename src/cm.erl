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

%% public api (from cm)
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
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Device) ->
    start_link(Device, []).
start_link(Device = #device{}, BehindDevs) ->
    gen_server:start_link({local, Device#device.server_id}, ?MODULE,
			  [Device, BehindDevs], [{debug,[log]}]). %

stop(CmId) ->
    gen_server:cast(CmId, {stop}).

linkstate(CmId, online) ->
    gen_server:cast(CmId, {linkstate, online});
linkstate(CmId, offline) ->
    gen_server:cast(CmId, {linkstate, offline}).

%% Cable modem native components modems call this to have a network packet 
%% relayed to the cmts (and further onwards) to the dhcp server
send_packet(CmId, Packet) ->
    gen_server:cast(CmId, {send_packet, Packet}).

%% Devices behind the cable modem (mta, cpe) call this to have a network packet 
%% relayed to the cmts (and further onwards) to the dhcp server
relay_packet(CmId, Packet) ->
    gen_server:cast(CmId, {relay_packet, Packet}).

%% Cmts calls this to deliver a network packet to the cable modem
%% Packet types: DHCP, TFTP, ToD
receive_packet(CmId, Packet) ->
    gen_server:cast(CmId, {receive_packet, Packet}).

%%% External events (human)

%% power on a modem
poweron(N) ->
    gen_server:cast(N, {poweron}).

%% power off a modem
poweroff(N) ->
    gen_server:cast(N, {poweroff}).

%% reset a modem
reset(N) ->
    gen_server:cast(N, {reset}).

%% Connect to a (new) cmts
connect_cmts(CmId, CmtsId) ->
    gen_server:cast(CmId, {connect, CmtsId}).

%% Connect to a (new) cmts
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
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Device, BehindDevs]) when is_record(Device, device) ->
    error_logger:info_msg("Starting Cable Modem ~p~n", [Device#device.server_id]),
    {ok, #state{device = Device,
                devices_behind = BehindDevs,
                linkstate=offline}}.

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
    cmts:disconnect(Device#device.upstream_id),
    {noreply, StateData};
% External event: tell the cable modem to power off
handle_cast({reset}, StateData = #state{device = Device, devices_behind = DevBehind}) ->
    dhcp_client:reset(Device#device.dhcp_client),
    lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                  DevBehind),
    {noreply, StateData};
% External event: tell the cable modem to reset
handle_cast({poweron}, StateData = #state{device = Device, devices_behind = DevBehind}) ->
    dhcp_client:poweron(Device#device.dhcp_client),
    lists:foreach(fun (D) -> dhcp_client:poweron(D#device.dhcp_client) end,
                  DevBehind),
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
            cmts:disconnect(Device#device.upstream_id, Device#device.server_id),
            dhcp_client:reset(Device#device.dhcp_client),
            lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                          DevBehind),
            Device2 = Device#device{upstream_id=CmtsId},
            {noreply, StateData#state{device=Device2}}
    end;
% External event: tell the cable modem we're not connected to a cmts
handle_cast({disconnect}, StateData = #state{device = Device, devices_behind = DevBehind}) ->
    cmts:disconnect(Device#device.upstream_id, Device#device.server_id),
    dhcp_client:reset(Device#device.dhcp_client),
    lists:foreach(fun (D) -> dhcp_client:reset(D#device.dhcp_client) end,
                  DevBehind),
    Device2 = Device#device{upstream_id=undefined},
    {noreply, StateData#state{device = Device2}};
% handles DHCP client link status feedback
handle_cast({linkstate, L}, StateData) ->
    {noreply, StateData#state{linkstate=L}};
% handles cable modem client network communication
handle_cast({send_packet, _P}, StateData = #state{device = Device}) 
  when Device#device.upstream_id =:= undefined ->
    dhcp_client:poweroff(Device#device.dhcp_client),
    {noreply, StateData};
handle_cast({send_packet, P}, StateData = #state{device = Device}) ->
    Cmts = Device#device.upstream_id,
    Me = Device#device.server_id,
    cmts:send_packet(Cmts, P, Me),
    {noreply, StateData};
% handles embedded device (mta, cpe) network communication
handle_cast({relay_packet, _}, StateData = #state{device = Device})
  when Device#device.upstream_id =:= undefined ->
    {noreply, StateData};
handle_cast({relay_packet, _}, StateData = #state{linkstate=offline}) ->
    {noreply, StateData};
handle_cast({relay_packet, P}, StateData = #state{device = Device}) ->
    Cmts = Device#device.upstream_id,
    Me = Device#device.server_id,
    error_logger:info_msg("Relaying not implemented correctly yet, opt82 missing from dhcp packets;"),
    % replace, add option-82 to dhcp packets; leave others alone
    cmts:send_packet(Cmts, P, Me),
    {noreply, StateData};
% handles network packets received from cmts
handle_cast({receive_packet, P = #dhcp{chaddr=ChAddr}}, 
            StateData = #state{device = Device, devices_behind = BehindDevs}) ->
    % must route to correct module
    lists:foreach(fun (_Dev = #device{dhcp_client=Process, mac=Mac}) ->
                          if Mac =:= ChAddr ->
                                  dhcp_client:receive_packet(Process, P);
                             true -> ok
                          end
                  end,
                  [Device | BehindDevs]),
    {noreply, StateData}.

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
