%%%-------------------------------------------------------------------
%%% File    : cm
%%% Author  : Jacob Lorensen
%%% Description : Cable Modem dhcp client simulator
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%-------------------------------------------------------------------
-module(cm).
-behaviour(gen_fsm).

%% public api
-export([stop/1, start_link/2, start_link/3, 
         poweron/1, poweroff/1, reset/1, 
         connect_cmts/2, disconnect_cmts/1,
         receive_packet/2]).

%% gen_fsm callbacks
-export([init/1, code_change/4, handle_state/2, handle_event/3, handle_info/3, 
         handle_sync_event/4, terminate/3]).

%% gen_fsm states
-export([poweroff/2, dhcp_selecting/2, dhcp_requesting/2, dhcp_rebooting/2, dhcp_bound/2,
        dhcp_renewing/2]).
% implicit states dhcp_init/1, dhcp_initreboot/1

-include("simul.hrl").
-include("dhcp.hrl").

%%
%% state data
%%
-record(state, {ip="", leasetime=0, bindtime=0, cmts, name, mac=""}).
-define(STATE_TIMEOUT, 5000).
-define(RENEW_TIMEOUT, 30000).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(N, MAC) ->
    gen_fsm:start_link({local, N}, cm, [undefined, N, MAC], []). %{debug,[trace]}
start_link(CMTS, N, MAC) ->
    gen_fsm:start_link({local, N}, cm, [CMTS, N, MAC], []). %{debug,[trace]}


%% send a stop this will end up in "handle_event"
stop(N)  -> gen_fsm:send_all_state_event(N, {stop}).

%%% External events (human)

%% power on a modem
poweron(N) ->
    gen_fsm:send_event(N, {reset}).

%% power off a modem
poweroff(N) ->
    gen_fsm:send_all_state_event(N, {poweroff}).

%% reset a modem
reset(N) ->
    gen_fsm:send_all_state_event(N, {reset}).

%% Connect to a (new) cmts
connect_cmts(CmId, CmtsId) ->
    gen_fsm:send_all_state_event(CmId, {connect,CmtsId}).

%% Connect to a (new) cmts
disconnect_cmts(CmId) ->
    gen_fsm:send_all_state_event(CmId, {disconnect}).

%%% External events ( network )
receive_packet(N, PACKET) ->
    gen_fsm:send_event(N, {receive_packet, PACKET}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm2:start/[3,4] or
%% gen_fsm2:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%% {ok, StateName, State, Timeout} |
%% ignore |
%% {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([CMTS, N, MAC]) ->
    {ok, poweroff, #state{cmts=CMTS, name=N, mac=MAC}}.


%%
%% Shoudld be called on every state change...
%%
handle_state(StateName, State) ->
    io:format("I'm in a new state ~p~n",[StateName]),
    {ok, State}.

%%%
%%%% STATES
%%%
poweroff({reset}, StateData) ->
    dhcp_init(StateData).

dhcp_selecting({receive_packet, #dhcp{yiaddr=ClientIP}}, StateData) ->
    NewStateData=StateData#state{ip=ClientIP},
    %error_logger:info_msg("Receive offer myip ~p~n", [NewStateData]),
    % first offer is fine, ask for it
    send_request(StateData#state.cmts, NewStateData),
    {next_state, dhcp_requesting, NewStateData, ?STATE_TIMEOUT};
dhcp_selecting(timeout, StateData) ->
    dhcp_init(StateData).

dhcp_requesting({receive_packet, D}, StateData) ->
    handle_dhcp_ack(D, StateData);
dhcp_requesting(timeout, StateData) ->
    dhcp_init(StateData).

% implicit dhcp_initreboot state
dhcp_initreboot(StateData) ->
    send_request(StateData#state.cmts, StateData),
    {next_state, dhcp_rebooting, StateData, ?STATE_TIMEOUT}.

dhcp_rebooting({receive_packet, D}, StateData) ->
    handle_dhcp_ack(D, StateData);
dhcp_rebooting(timeout, StateData) ->
    dhcp_init(StateData#state{ip=undefined,leasetime=undefined}).

dhcp_bound(timeout, StateData) ->
    send_renew(StateData#state.cmts, StateData),
    {next_state, dhcp_renewing, StateData, 15000}.

dhcp_renewing({receive_packet, D}, StateData)  ->
    handle_dhcp_ack(D, StateData);
dhcp_renewing(timeout, StateData) ->
    T2 = t2(StateData),
    if T2 > 0 ->
            send_renew(StateData#state.cmts, StateData),
            {next_state, dhcp_renewing, StateData, ?RENEW_TIMEOUT};
       true ->
            dhcp_init(StateData)
    end.

% implicit dhcp_init state
dhcp_init(StateData) ->
    case StateData#state.cmts of
        undefined -> {next_state, poweroff, StateData};
        _ -> send_discover(StateData#state.cmts, StateData),
             {next_state, dhcp_selecting, StateData, ?STATE_TIMEOUT}
    end.

handle_dhcp_ack(D = #dhcp{yiaddr=ClientIP}, StateData) ->
    case dhcp_util:optsearch(?DHO_DHCP_MESSAGE_TYPE, D) of
	{value, ?DHCPNAK} ->
            {next_state, dhcp_init, StateData#state{ip=undefined,leasetime=undefined}};
        {value, ?DHCPACK} ->
            case dhcp_util:optsearch(?DHO_DHCP_LEASE_TIME, D) of
                {value, Value} ->
                    %error_logger:info_msg("Lease reply with lease time ~p~n", [Value]),
                    {_,Bindtime,_} = erlang:now(),
                    NewStateData = StateData#state{ip=ClientIP,leasetime=Value,bindtime=Bindtime},
                    T1 = t1(StateData),
                    {next_state, dhcp_bound, NewStateData, T1*1000};
                false ->
                    error_logger:info_msg("State dhcp_bound timeout indefinite (bootp)~n"),
                    {next_state, dhcp_bound, StateData#state{ip=ClientIP,leasetime=undefined}}
            end
    end.

t1(StateData) ->    
    StateData#state.leasetime div 2.

t2(StateData) ->
    {_,Now,_} = erlang:now(),
    Now - StateData#state.bindtime - StateData#state.leasetime.

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
handle_event({poweroff}, _StateName, StateData) ->
    {next_state, poweroff, StateData};
handle_event({reset}, poweroff, StateData) ->
    {next_state, poweroff, StateData};
handle_event({reset}, dhcp_bound, StateData) ->
    dhcp_initreboot(StateData);
handle_event({reset}, _StateName, StateData) ->
    dhcp_init(StateData);
handle_event({poweron}, _StateName, StateData) ->
    dhcp_init(StateData);
% re-connecting is tricky, and not entirely correct here
% see Rebinding... later...
handle_event({connect, CmtsId}, _StateName, StateData) ->
    if StateData#state.cmts =:= undefined ->
            StateData2 = StateData#state{cmts=CmtsId},
            cmts:connect(StateData2#state.cmts, StateData2#state.name),
            dhcp_init(StateData2);
        CmtsId =:= StateData#state.cmts ->
            cmts:disconnect(StateData#state.cmts, StateData#state.name),
            StateData2 = StateData#state{cmts=CmtsId},
            cmts:connect(StateData2#state.cmts, StateData2#state.name),
            dhcp_init(StateData2);
       true ->
            dhcp_init(StateData)
    end;
handle_event({disconnect}, _StateName, StateData) ->
    cmts:disconnect(StateData#state.cmts, StateData#state.name),
    {next_state, poweroff, StateData#state{cmts=undefined}};
handle_event({stop}, _StateName, StateData) ->
    {stop, normal, StateData}.   %% tell it to stop

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm2:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%% {next_state, NextStateName, NextState} |
%% {next_state, NextStateName, NextState, Timeout} |
%% {reply, Reply, NextStateName, NextState} |
%% {reply, Reply, NextStateName, NextState, Timeout} |
%% {stop, Reason, NewState} |
%% {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%% {next_state, NextStateName, NextState} |
%% {next_state, NextStateName, NextState, Timeout} |
%% {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    error_logger:info_msg("Terminating ~p  ~p~n", [_Reason, _StateName]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%% {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



%% Utility functions

send_discover(CMTS, StateData) ->    
    Discover = #dhcp{
      op = ?BOOTREQUEST,
      chaddr = StateData#state.mac,
      options=[{?DHO_DHCP_MESSAGE_TYPE, ?DHCPDISCOVER}]
     },
    %error_logger:info_msg("Send dhcp_discover ~p ~p~n", [CMTS, Discover]),
    cmts:send_packet(CMTS, Discover, StateData#state.name).

send_request(CMTS, StateData) ->    
    Request = #dhcp{
      op = ?BOOTREQUEST,
      chaddr = StateData#state.mac,
      options=[{?DHO_DHCP_MESSAGE_TYPE, ?DHCPREQUEST},
              {?DHO_DHCP_REQUESTED_ADDRESS, StateData#state.ip}]
     },
    %error_logger:info_msg("Send dhcp_request ~p ~p~n", [CMTS, Request]),
    cmts:send_packet(CMTS, Request, StateData#state.name).

send_renew(CMTS, StateData) ->    
    Request = #dhcp{
      op = ?BOOTREQUEST,
      chaddr = StateData#state.mac,
      ciaddr = StateData#state.ip,
      options=[{?DHO_DHCP_MESSAGE_TYPE, ?DHCPREQUEST}]
     },
    %error_logger:info_msg("Send dhcp_request ~p ~p~n", [CMTS, Request]),
    cmts:send_packet(CMTS, Request, StateData#state.name).

% http://www.tcpipguide.com/free/t_DHCPGeneralOperationandClientFiniteStateMachine.htm
% http://www.erlang.org/doc/reference_manual/records.html
% http://www.erlang.org/doc/design_principles/fsm.html
% http://www.erlang.org/doc/reference_manual/modules.html
% https://github.com/yrashk/gen_fsm2/blob/master/example/src/example_fsm.erl
% http://www.erlang.org/documentation/doc-4.8.2/doc/design_principles/fsm.html
