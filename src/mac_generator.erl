%%%-------------------------------------------------------------------
%%% @author Jacob Lorensen
%%% @copyright 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%% @doc Unique MAC address generator
%%% @end
%%%-------------------------------------------------------------------
-module(mac_generator).
-behaviour(gen_server).

-include("debug.hrl").

%% Public API
-export([start_link/0, nextmac/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {prefixdict}).

%%====================================================================
%% API
%%====================================================================

%% @doc
%% Starts the mac address generator server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ?debug("mac_generator:start_link()~n"),
    X = gen_server:start_link({local, ?MODULE}, ?MODULE,
			  [], 
                          []),
    ?debug("mac_generator:start_link() returns ~p~n", [X]),
    X. %{debug,[trace]}{debug,[log]}

%% @doc
%% Calcalate a next mac address based on a mac vendor prefix. Repeated calls
%% will generate a new unique mac address (based on an increasing counter).
%% A fully specified mac address will just be returned unmodified.
%% @end
nextmac(M = {_,_,_,_,_,_}) ->
    M;
nextmac(Prefix = {_,_,_}) ->
    ?debug("mac_generator:nextmac(~p)~n", [Prefix]),
    gen_server:call(?MODULE, {nextmac, Prefix}).

%%---------------------------------------------------------------------
%% Gen-server callbacks
%%---------------------------------------------------------------------

%% @doc Initialize an instance of the cmts server process
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
init(_Arg) ->
    ?debug("mac_generator:init(~p)~n", [_Arg]),
    X = {ok, #state{prefixdict=dict:new()}},
    ?debug("mac_generator:init(~p) returns ~p~n", [_Arg, X]),
    X.


handle_call(_X = {nextmac, Prefix}, From, State) ->
    ?debug("mac_generator:handle_call(~p, ~p, ~p) ~n", [_X, From, State]),
    handle_nextmac(Prefix, From, State);
handle_call(_X = {test}, _F, S) ->
    ?debug("TEST: mac_generator:(~p, ~p, ~p) ~n", [_X, _F, S]),
    {reply, ok, S};
handle_call(_X, _F, _S) ->
    ?debug("no match: mac_generator:(~p, ~p, ~p) ~n", [_X, _F, _S]),
    error.

handle_nextmac(Prefix = {A, B, C}, _From, State = #state{prefixdict=Prefixes}) ->
    {Prefixes2, Count} =  case dict:find(Prefix, Prefixes) of
                              {ok, Countx} ->
                                  {dict:store(Prefix, Countx+1, Prefixes), Countx};
                              _ ->
                                  Countx = 0,
                                  {dict:store(Prefix, 1, Prefixes), Countx}
                          end,
    State2 = State#state{prefixdict=Prefixes2},
    Mac = {A, B, C, Count div 256 div 256, Count div 256, Count rem 256},
    {reply, Mac, State2}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
