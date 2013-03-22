%% @author Jacob Lorensen
%% @copyright Jacob Lorensen 2013
%% @doc Cable modem simulation main supervisor
%% @end
-module(cablesim_sup).
%-compile([debug_info, export_all]).

-behaviour(supervisor).

-include("device.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Cmtses) ->
    io:format("start_link: Cmtses ~p~n", [Cmtses]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Cmtses]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Cmtses]) ->
    RestartStrategy = {one_for_one, 10, 60},    
    io:format("init CMTSes: ~p~n", [Cmtses]),
    MR = lists:map(
           fun ({Id, Ip, DhcpHelpers}) ->
                   {Id, 
                    {Id, start_link, [Id, Ip, DhcpHelpers]},
                    permanent, 5000, worker, [ch1]}
           end,
           Cmtses),
    io:format("MR: ~p~n", [MR]),
    Children = MR,
    io:format("SUP Children: ~p~n", [Children]),
    {ok, {RestartStrategy, Children}}.
