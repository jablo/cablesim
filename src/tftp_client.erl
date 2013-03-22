%%%-------------------------------------------------------------------
%%% File    : tftp_client
%%% Author  : Jacob Lorensen
%%% Description : Tftp client state machine
%%% As opposedt to the dhcp client, tftp clients will communicate
%%% with the network directly, not going through the cable modem 
%%% process nor the cmts process. Partly for efficiency, partly to
%%% more closely model real network traffic: Simulating thousands of
%%% cable modems - and thus thousands of tftp clients -all routing
%%% traffic through the cmts, would serialize a lot of that traffic.
%%% Unless, of course, the CMTS process would implement a number of
%%% worker tftp processes... which in effect, would bring us back into
%%% a design where the tftp processes communicate directly with the
%%% network. Hence.
%%% Created : 17 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%-------------------------------------------------------------------
-module(tftp_client).
-behaviour(gen_fsm).

-include("device.hrl").
-include("tftp.hrl").

-define(ACK_TIMEOUT, 5000). % timeout in tftp protocol before we retransmit an ack

-record(state, {
          device,
          server,
          socket,
          port = ?TFTP_PORT,
          filename,
          lastack,
          block,
          data}).

%% public api
-export([stop/1, start_link/2, get_file/3, receive_packet/2, set_standby/1]).

%% gen_fsm callbacks
-export([init/1, code_change/4, handle_state/2, handle_event/3, handle_info/3, 
         handle_sync_event/4, terminate/3]).

%% gen_fsm states
-export([standby/2, rrq_sent/2, ack_sent/2]).

%% @doc
%% Create a dhcp_client instance
%%
%% @spec start_link(N, Device) -> {ok, Pid} | ignore | {error, Error}
%% @end
start_link(N, Device) ->
    gen_fsm:start_link({local, N}, tftp_client, [N, Device], [{debug,[trace]}]). %{debug,[trace]}

%% send a stop this will end up in "handle_event"
stop(N)  -> gen_fsm:send_all_state_event(N, {stop}).

%%% External events (human interaction)

%% @doc
%% Start the tftp client state machine requesting it to get a file
%% @end
get_file(N, Server, File) ->
    gen_fsm:send_event(N, {start_read, Server, File}).

%% @doc
%% force the tftp client state machine into standby state regadless of
%% the current state - ie., interrupt any onging file tansfer
%% @end
set_standby(N) ->
    gen_fsm:send_all_state_event(N, {standby}).

%%% External events (network)

%% @doc
%% receive a data packet from the network (cmts/cable modem acts as proxy).
%% @end
receive_packet(N, PACKET) ->
    gen_fsm:send_event(N, {packet, PACKET}).

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
    error_logger:info_msg("Starting TFTP Client ~p~n", [N]),
    {ok, standby, #state{device=Device}}.

%%
%% Should be called on every state change...
%%
handle_state(StateName, State) ->
    io:format("I'm in a new state ~p~n",[StateName]),
    {ok, State}.

%%%
%%%% STATES
%%%

standby({start_read, Server, Filename}, State) ->
    { ok, Sock } = gen_udp:open(0, [binary]),
    State2 = State#state{filename=Filename, server=Server, socket=Sock, lastack=0, data = <<>>},
    send_rrq(State2);
standby(_, State) ->
    {next_state, standby, State}.

rrq_sent({packet, #tftp_data{port=Port, block=Block, data=Data}}, State) ->
    OData = State#state.data,
    NData = << OData/binary, Data/binary >>,
    State2 = State#state{port = Port, lastack=Block, data = NData},
    send_ack(State2);
rrq_sent(timeout, State) ->
    send_rrq(State);
rrq_sent(_, State) ->
    {next_state, rrq_sent, State}.

ack_sent({packet, #tftp_data{block=Block}}, State = #state{lastack=LastAck}) 
  when Block < LastAck ->
    State;
ack_sent({packet, #tftp_data{block=Block}}, State = #state{lastack=LastAck}) 
  when Block =:= LastAck ->
    send_ack(State);
% the data block concatenation isn't really expensive; mostly cable modem config files
% are 1 block long (300 bytes or so).
ack_sent({packet, #tftp_data{block=Block, data=Data}}, State = #state{}) ->
    OData = State#state.data,
    State2 = State#state{lastack=Block, data = << OData/binary, Data/binary >>},
    send_ack(State2);
ack_sent(timeout, State) ->    
    send_ack(State);
ack_sent(_, State) ->
    {next_state, ack_sent, State}.
   

%%
%% Internal transition states
%%
send_ack(State = #state{lastack=Ack, socket=S, server=IP, port=P}) ->
    Pck = #tftp_ack{block=Ack},
    gen_udp:send(S, IP, P, tftp_lib:encode(Pck)),
    {next_state, ack_sent, State}.

send_rrq(State = #state{socket=S, server=IP, port=P, filename=F}) ->
    Pck = #tftp_request{opcode=?OC_RRQ, filename=F, mode=?C_OCTET},
    gen_udp:send(S, IP, P, tftp_lib:encode(Pck)),
    {next_state, rrq_sent, State, ?ACK_TIMEOUT}.

force_close(StateData = #state{socket = S}) ->
    case S of
        undefined -> ok;
        _ -> gen_udp:close(S)
    end,
    {next_state, standby, StateData#state{socket=undefined}}.

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
handle_event({standby}, _StateName, StateData) ->
    force_close(StateData).

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
handle_info({udp, _Socket, _IP, Port, Packet}, StateName, State) ->
    Tftp = tftp_lib:decode(Packet),
    if is_record(Tftp, tftp_data) ->
            receive_packet(self(), Tftp#tftp_data{port = Port});
       true ->  
            receive_packet(self(), Tftp)
    end,
    {next_state, StateName, State};
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

