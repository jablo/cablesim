% http://www.tcpipguide.com/free/t_DHCPGeneralOperationandClientFiniteStateMachine.htm
% http://www.erlang.org/doc/reference_manual/records.html
% http://www.erlang.org/doc/design_principles/fsm.html
% http://www.erlang.org/doc/reference_manual/modules.html
% https://github.com/yrashk/gen_fsm2/blob/master/example/src/example_fsm.erl
% http://www.erlang.org/documentation/doc-4.8.2/doc/design_principles/fsm.html
-module(cm).
-behaviour(gen_fsm).

%% public api
-export([stop/1, start_link/3, poweron/1, poweroff/1, reset/1, receive_packet/2]).

%% gen_fsm callbacks
-export([init/1, code_change/4, handle_state/2, handle_event/3, handle_info/3, 
         handle_sync_event/4, terminate/3]).

%% gen_fsm states
-export([poweroff/2, dhcp_selecting/2, dhcp_requesting/2, dhcp_initreboot/2, dhcp_rebooting/2, dhcp_bound/2,
         dhcp_renewing/2, dhcp_rebinding/2]).

-include("simul.hrl").
-include("dhcp.hrl").

%%
%% state data
%%
-record(state, {ip="", leasetime=0, cmts, name, mac=""}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(CMTS, N, MAC) ->
    gen_fsm:start_link({local, N}, cm, [CMTS, N, MAC], [{debug,[trace]}]).

%% send a stop this will end up in "handle_event"
stop(N)  -> gen_fsm:send_all_state_event(N, stopit).

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
    io:format("Receive offer myip ~p~n", [NewStateData]),
    {next_state, dhcp_requesting, StateData};
dhcp_selecting(timeout, StateData) ->
    dhcp_init(StateData).

dhcp_requesting({receive_packet, #dhcp{yiaddr=_ClientIP}}, StateData) ->
    {next_state, dhcp_requesting, StateData}.

dhcp_initreboot({receive_packet, #dhcp{yiaddr=_ClientIP}}, StateData) ->
    {next_state, dhcp_requesting, StateData}.

dhcp_rebooting({receive_packet, #dhcp{yiaddr=_ClientIP}}, StateData) ->
    {next_state, dhcp_requesting, StateData}.

dhcp_bound({receive_packet, #dhcp{yiaddr=_ClientIP}}, StateData) ->
    {next_state, dhcp_requesting, StateData}.

dhcp_renewing({receive_packet, #dhcp{yiaddr=_ClientIP}}, StateData) ->
    {next_state, dhcp_requesting, StateData}.

dhcp_rebinding({receive_packet, #dhcp{yiaddr=_ClientIP}}, StateData) ->
    {next_state, dhcp_requesting, StateData}.

% implicit dhcp_init state
dhcp_init(StateData) ->
    send_discover(StateData#state.cmts, StateData),
    {next_state, dhcp_selecting, StateData, 2000}.


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
handle_event({reset}, _StateName, StateData) ->
    dhcp_init(StateData);
handle_event({poweron}, _StateName, StateData) ->
    dhcp_init(StateData).

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
      op = ?DHCPDISCOVER,
      chaddr = StateData#state.mac
     },
    io:format("Send discover ~p ~p~n", [CMTS, Discover]),
    cmts:send_packet(CMTS, Discover).



