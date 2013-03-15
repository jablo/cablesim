%%%-------------------------------------------------------------------
%%% File    : dhcp_client
%%% Author  : Jacob Lorensen
%%% Description : Dhcp client state machine
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%-------------------------------------------------------------------
-module(dhcp_client).
%-compile([debug_info, export_all]).
-behaviour(gen_fsm).

%% public api
-export([stop/1, start_link/2,
         poweron/1, poweroff/1, reset/1, 
         receive_packet/2]).

%% gen_fsm callbacks
-export([init/1, code_change/4, handle_state/2, handle_event/3, handle_info/3, 
         handle_sync_event/4, terminate/3]).

%% gen_fsm states
-export([poweroff/2, dhcp_selecting/2, dhcp_requesting/2, dhcp_rebooting/2, dhcp_bound/2,
        dhcp_renewing/2]).
% Non-public states: dhcp_init/1, dhcp_initreboot/1

-include("dhcp.hrl").
-include("device.hrl").

%%
%% state data
%%
-record(state, {
          ip="",                 % IP address obtained via the dhcp protocol
          leasetime=0,           % lease time obtained via the dhcp protocol
          bindtime=0,            % the time we received the lease
          device,                % device structure describing this dhcp client
          retransmit_count=0     % for retransmitting states
         }).
-define(RETRANSMIT_TIMEOUT, 5000).
-define(RETRANSMIT_COUNT_MAX, 12).
-define(RENEW_TIMEOUT, 30000).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link(N, Device) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(N, Device) ->
    gen_fsm:start_link({local, N}, dhcp_client, [N, Device], [{debug,[]}]). %{debug,[trace]}

%% send a stop this will end up in "handle_event"
stop(N)  -> gen_fsm:send_all_state_event(N, {stop}).

%%% External events (human interaction)

poweron(N) ->
    gen_fsm:send_all_state_event(N, {poweron}).

poweroff(N) ->
    gen_fsm:send_all_state_event(N, {poweroff}).

reset(N) ->
    gen_fsm:send_all_state_event(N, {reset}).

%%% External events (network)

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
init([N, Device]) ->
    error_logger:info_msg("Starting DHCP Client ~p~n", [N]),
    {ok, poweroff, #state{device=Device}}.

%%
%% Should be called on every state change...
%%
handle_state(StateName, State) ->
    io:format("I'm in a new state ~p~n",[StateName]),
    {ok, State}.

%%%
%%%% STATES
%%%

poweroff({poweron}, StateData) ->
    dhcp_init(StateData);
poweroff(_, StateData) ->
    {next_state, poweroff, StateData}.

dhcp_selecting({receive_packet, #dhcp{yiaddr=ClientIP}}, StateData) ->
    NewStateData=StateData#state{ip=ClientIP},
    % a bit naïve - first offer is fine, ask for it
    send_request(NewStateData),
    {next_state, dhcp_requesting, NewStateData, ?RETRANSMIT_TIMEOUT};
dhcp_selecting(timeout, StateData) ->
    dhcp_init(StateData);
dhcp_selecting(_, StateData) ->
    {next_state, dhcp_selecting, StateData}.

dhcp_requesting({receive_packet, D}, StateData) ->
    handle_dhcp_ack(D, dhcp_requesting, StateData);
dhcp_requesting(timeout, StateData = #state{retransmit_count = Rex}) ->
    if Rex > ?RETRANSMIT_COUNT_MAX ->
            dhcp_init(StateData);
       true ->
            send_request(StateData),
            {next_state, dhcp_requesting, StateData#state{retransmit_count = Rex+1}}
    end;
dhcp_requesting(_, StateData) ->
    {next_state, dhcp_requesting, StateData}.

dhcp_rebooting({receive_packet, D}, StateData) ->
    handle_dhcp_ack(D, dhcp_rebooting, StateData);
dhcp_rebooting(timeout, StateData = #state{retransmit_count = Rex}) ->
    if Rex > ?RETRANSMIT_COUNT_MAX ->
            dhcp_init(StateData);
       true ->
            send_request(StateData),
            {next_state, dhcp_rebooting, StateData#state{retransmit_count = Rex+1}}
    end;
dhcp_rebooting(_, StateData) ->
    {next_state, dhcp_rebooting, StateData}.

dhcp_bound(timeout, StateData) ->
    send_renew(StateData),
    {next_state, dhcp_renewing, StateData, 15000};
dhcp_bound(_, StateData) ->
    {next_state, dhcp_bound, StateData, t1(StateData)*1000}.
    
dhcp_renewing({receive_packet, D}, StateData)  ->
    handle_dhcp_ack(D, dhcp_renewing, StateData);
dhcp_renewing(_, StateData) ->
    T2 = t2(StateData),
    if T2 > 0 ->
            send_renew(StateData),
            {next_state, dhcp_renewing, StateData, ?RENEW_TIMEOUT};
       true ->
            dhcp_init(StateData)
    end.

%%
%% Internal transition states
%%

dhcp_init(StateData = #state{device=D}) ->
    T = D#device.template,
    (T#device_template.linkstate_fun)(D, offline),
    send_discover(StateData),
    StateData2 = StateData#state{ip=undefined,leasetime=undefined,retransmit_count=0},
    {next_state, dhcp_selecting, StateData2, ?RETRANSMIT_TIMEOUT}.

dhcp_initreboot(StateData = #state{device = Device}) ->
    T = Device#device.template,
    (T#device_template.linkstate_fun)(Device, offline),
    send_request(StateData),
    {next_state, dhcp_rebooting, StateData, ?RETRANSMIT_TIMEOUT}.

%%
%% State transition functions
%%

handle_dhcp_ack(D = #dhcp{yiaddr=ClientIP}, CurrentState, StateData = #state{device = Device}) ->
    case dhcp_util:optsearch(?DHO_DHCP_MESSAGE_TYPE, D) of
	{value, ?DHCPNAK} ->
            dhcp_init(StateData);
        {value, ?DHCPACK} ->
            Template = Device#device.template,
            case dhcp_util:optsearch(?DHO_DHCP_LEASE_TIME, D) of
                {value, Value} ->
                    {_,Bindtime,_} = erlang:now(),
                    NewStateData = StateData#state{ip=ClientIP,leasetime=Value,
                                                   bindtime=Bindtime,retransmit_count=0},
                    (Template#device_template.linkstate_fun)(Device, online),
                    T1 = t1(NewStateData),
                    {next_state, dhcp_bound, NewStateData, T1*1000};
                false ->
                    error_logger:info_msg("State dhcp_bound timeout indefinite (bootp)~n"),
                    (Template#device_template.linkstate_fun)(Device, online),
                    {next_state, dhcp_bound, StateData#state{ip=ClientIP,leasetime=undefined}}
            end;
        % when expecting nak/ack, ignore anything else
        _ -> {next_state, CurrentState, StateData}
    end.

t1(StateData) ->    
    {_,Now,_} = erlang:now(),
    TimePassed = Now - StateData#state.bindtime,
    TimeLeft = (StateData#state.leasetime div 2) - TimePassed,
    if TimeLeft > 0 -> TimeLeft;
       true -> 0
    end.

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
handle_event({poweroff}, _StateName, StateData = #state{device=D}) ->
    T = D#device.template,
    (T#device_template.linkstate_fun)(D, offline),
    {next_state, poweroff, StateData};
handle_event({poweron}, poweroff, StateData) ->
    dhcp_init(StateData);
handle_event({poweron}, StateName, StateData) ->
    {next_state, StateName, StateData};
handle_event({reset}, poweroff, StateData) ->
    {next_state, poweroff, StateData};
handle_event({reset}, dhcp_bound, StateData) ->
    dhcp_initreboot(StateData);
handle_event({reset}, _StateName, StateData) ->
    dhcp_init(StateData);
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
%%% Utility functions
%%%===================================================================

send_discover(#state{device=D}) when is_record(D, device) ->
    T = D#device.template,
    Discover = #dhcp{
      op = ?BOOTREQUEST,
      chaddr = D#device.mac,
      options=[{?DHO_DHCP_MESSAGE_TYPE, ?DHCPDISCOVER},
               {?DHO_VENDOR_CLASS_IDENTIFIER, T#device_template.vendor_class_id},
               {?DHO_DHCP_CLIENT_IDENTIFIER, (T#device_template.client_id_fun)(D)},
               {?DHO_DHCP_PARAMETER_REQUEST_LIST, T#device_template.parameter_request_list},
               {?DHO_VENDOR_ENCAPSULATED_OPTIONS, (T#device_template.vendor_options_fun)(D)}]
     },
    (T#device_template.send_packet_fun)(D, Discover).

send_request(#state{ip=IP, device=D}) ->
    T = D#device.template,
    Request = #dhcp{
      op = ?BOOTREQUEST,
      chaddr = D#device.mac,
      options=[{?DHO_DHCP_MESSAGE_TYPE, ?DHCPREQUEST},
               {?DHO_DHCP_REQUESTED_ADDRESS, IP},
               {?DHO_VENDOR_CLASS_IDENTIFIER, T#device_template.vendor_class_id},
               {?DHO_DHCP_CLIENT_IDENTIFIER, (T#device_template.client_id_fun)(D)},
               {?DHO_DHCP_PARAMETER_REQUEST_LIST, T#device_template.parameter_request_list},
               {?DHO_VENDOR_ENCAPSULATED_OPTIONS, (T#device_template.vendor_options_fun)(D)}]
     },
    (T#device_template.send_packet_fun)(D, Request).

send_renew(#state{ip=IP, device=D}) ->
    T = D#device.template,
    Renew = #dhcp{
      op = ?BOOTREQUEST,
      chaddr = D#device.mac,
      ciaddr = IP,
      options=[{?DHO_DHCP_MESSAGE_TYPE, ?DHCPREQUEST},
               {?DHO_VENDOR_CLASS_IDENTIFIER, T#device_template.vendor_class_id},
               {?DHO_DHCP_CLIENT_IDENTIFIER, (T#device_template.client_id_fun)(D)},
               {?DHO_DHCP_PARAMETER_REQUEST_LIST, T#device_template.parameter_request_list},
               {?DHO_VENDOR_ENCAPSULATED_OPTIONS, (T#device_template.vendor_options_fun)(D)}]
     },
    (T#device_template.send_packet_fun)(D, Renew).
