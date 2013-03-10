%%%-------------------------------------------------------------------
%%% File    : cm
%%% Author  : Jacob Lorensen
%%% Description : Cable modem simulator
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%
%%% Simulating a cable modem essentially consists of fowarding packets from
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

-behaviour(gen_server).

%% public api (from cm)
-export([stop/1, start_link/3, start_link/2, 
         send_packet/2, relay_packet/2, receive_packet/2, linkstate/2,
         poweron/1, poweroff/1, reset/1, 
         connect_cmts/2, disconnect_cmts/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("dhcp.hrl").

-record(state, {server_id, cmts=undefined, cm_dhcp, cpe_dhcp, linkstate=offline}).
% add state: cm_tftp, cm_tod, cm_mta, cm_cpe for other embedded devices

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ServerId, Mac) ->
    gen_server:start_link({local, ServerId}, ?MODULE,
			  [ServerId, Mac], [{debug,[trace]}]). %
start_link(Cmts, ServerId, Mac) ->
    gen_server:start_link({local, ServerId}, ?MODULE,
			  [ServerId, Mac, Cmts], [{debug,[trace]}]). %

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
init([ServerId, Mac, Cmts]) ->
    DHCP_Ref = mk_unique_atom(ServerId, dhcp),
    dhcp_client:start_link(DHCP_Ref, Mac, 
                           fun (P) -> cm:send_packet(ServerId, P) end,
                           fun (B) -> cm:linkstate(ServerId, B) end),
    {ok, #state{server_id=ServerId, cmts=Cmts, cm_dhcp=DHCP_Ref, linkstate=offline}};
init([ServerId, Mac]) ->
    DHCP_Ref = mk_unique_atom(ServerId, dhcp),
    dhcp_client:start_link(DHCP_Ref, Mac, 
                           fun (P) -> cm:send_packet(ServerId, P) end,
                           fun (B) -> cm:linkstate(ServerId, B) end),
    {ok, #state{server_id=ServerId, cmts=undefined, cm_dhcp=DHCP_Ref, linkstate=offline}}.

mk_unique_atom(Prefix, Postfix) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ atom_to_list(Postfix)).

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
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm2:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%% {next_state, NextStateName, NextState} |
%% {next_state, NextStateName, NextState, Timeout} |
%% {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, StateData) ->
    {stop, normal, StateData};   %% tell it to stop
handle_cast({poweroff}, StateData = #state{cm_dhcp=DHCP}) ->
    dhcp_client:poweroff(DHCP),
    {noreply, StateData};
handle_cast({reset}, StateData = #state{cm_dhcp=DHCP}) ->
    dhcp_client:reset(DHCP),
    {noreply, StateData};
handle_cast({poweron}, StateData = #state{cm_dhcp=DHCP}) ->
    dhcp_client:poweron(DHCP),
    {noreply, StateData};
% re-connecting is tricky, and not entirely correct here
% see Rebinding... later...
handle_cast({connect, CmtsId}, StateData = #state{cm_dhcp=DHCP}) ->
    if StateData#state.cmts =:= undefined ->
            StateData2 = StateData#state{cmts=CmtsId, linkstate=offline},
            cmts:connect(StateData2#state.cmts, StateData2#state.server_id),
            dhcp_client:reset(DHCP),
            {noreply, StateData2};
       CmtsId =:= StateData#state.cmts ->
            {noreply, StateData};
       true ->
            cmts:disconnect(StateData#state.cmts, StateData#state.server_id),
            StateData2 = StateData#state{cmts=CmtsId, linkstate=offline},
            cmts:connect(StateData2#state.cmts, StateData2#state.server_id),
            dhcp_client:reset(DHCP),
            {noreply, StateData2}
    end;
handle_cast({disconnect}, StateData) ->
    cmts:disconnect(StateData#state.cmts, StateData#state.server_id),
    StateData2 = StateData#state{cmts=undefined, linkstate=offline},
    {noreply, StateData2};
% handles DHCP client status feedback
handle_cast({linkstate, L}, StateData) ->
    {noreply, StateData#state{linkstate=L}};
handle_cast({send_packet, _P}, StateData = #state{cmts=undefined, cm_dhcp=DHCP}) ->
    dhcp_client:poweroff(DHCP),
    {noreply, StateData};
handle_cast({send_packet, P}, StateData = #state{cmts=Cmts, server_id=Me}) ->
    cmts:send_packet(Cmts, P, Me),
    {noreply, StateData};
handle_cast({relay_packet, _}, StateData = #state{cmts=undefined}) ->
    {noreply, StateData};
handle_cast({relay_packet, _}, StateData = #state{linkstate=offline}) ->
    {noreply, StateData};
handle_cast({relay_packet, P}, StateData = #state{cmts=Cmts, server_id=Me}) ->
    error_log:info_msg("Relaying not implemented correctly yet"),
    % replace, add option-82 to dhcp packets; leave others alone
    cmts:send_packet(Cmts, P, Me),
    {noreply, StateData};
handle_cast({receive_packet, P = #dhcp{}}, StateData = #state{cm_dhcp=Dhcp}) ->
    dhcp_client:receive_packet(Dhcp, P),
    {noreply, StateData}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State = #state{cm_dhcp=Dhcp}) ->
    dhcp_client:stop(Dhcp),
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
