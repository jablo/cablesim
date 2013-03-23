%% @author Jacob Lorensen
%% @copyright Jacob Lorensen 2013
%% @doc Cable modem simulation main supervisor
%% @end
-module(cablesim_sup).
%-compile([debug_info, export_all]).

-behaviour(supervisor).

-include("device.hrl").

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Cmtses, Cms) ->
    io:format("start_link:~nCmtses ~p~nCms: ~p", [Cmtses, Cms]),
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, [Cmtses, Cms]),
    io:format("Supervisor start_link returned ~p~n", [R]),
    R.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Cmtses|_]) ->
    RestartStrategy = {one_for_one, 10, 60},    
    io:format("init~nCMTSes: ~p~n", [Cmtses]),
    CmtsChildren = lists:map(
                     fun ({Id, Ip, DhcpHelpers}) ->
                             {Id, 
                              {cmts, start_link, [Id, Ip, DhcpHelpers]},
                              permanent, 5000, worker, [cmts,cm]}
                     end,
                     Cmtses),
    io:format("Permament CMTS processes: ~p~n", [CmtsChildren]),
    supervisor:check_childspecs(CmtsChildren),
    MacChild = {mac_generator,{mac_generator,start_link,[]},
                permanent,5000,worker,[mac_generator]},
    Children = [MacChild | CmtsChildren],
    io:format("SUP Children: ~p~n", [Children]),
    {ok, {RestartStrategy, Children}}.
