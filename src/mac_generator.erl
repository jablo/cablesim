%%%-------------------------------------------------------------------
%%% @author Jacob Lorensen
%%% @copyright 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%% @doc Unique MAC address generator
%%% @end
%%%-------------------------------------------------------------------
-module(mac_generator).

-behaviour(gen_server).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE,
			  [], 
                          []). %{debug,[trace]}{debug,[log]}

nextmac(Prefix) ->
    gen_server:call(?MODULE, {nextmac, Prefix}).

%%---------------------------------------------------------------------
%% Gen-server callbacks
%%---------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Initialize an instance of the cmts server process
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%%--------------------------------------------------------------------
init(_Arg) ->
    {ok, #state{prefixdict=dict:new()}}.



handle_call({nextmac, Prefix}, From, State) ->
    handle_nextmac(Prefix, From, State).

%% @doc
%% Calcalate a next mac address based on a mac vendor prefix. Repeated calls
%% will generate a new unique mac address (based on an increasing counter)
%% @end
handle_nextmac(Prefix = {A, B, C}, _From, State = #state{prefixdict=Prefixes}) ->
    Prefixes2 =  case dict:find(Prefix, Prefixes) of
                     {ok, Count} ->
                         dict:store(Prefix, Count+1, Prefixes);
                     _ ->
                         Count = 0,
                         dict:store(Prefix, 1, Prefixes)
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
