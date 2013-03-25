%% @author Jacob Lorensen
%% @copyright Jacob Lorensen 2013
%% @doc Cable modem simulation main supervisor
%% @end
-module(cablesim_sup).
%-compile([debug_info, export_all]).

-behaviour(supervisor).

-include("debug.hrl").
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
    ?debug("start_link:~nCmtses ~p~nCms: ~p", [Cmtses, Cms]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Cmtses, Cms]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Cmtses|_]) ->
    RestartStrategy = {one_for_one, 10, 60},    
    CmtsChildren = lists:map(
                     fun ({Id, Intf, Ip, Netmask, DhcpHelpers}) ->
                             {Id, 
                              {cmts, start_link, [Id, Intf, Ip, Netmask, DhcpHelpers]},
                              permanent, 5000, worker, [cmts,cm]}
                     end,
                     Cmtses),
    supervisor:check_childspecs(CmtsChildren),
    MacChild = {mac_generator,{mac_generator,start_link,[]},
                permanent,5000,worker,[mac_generator]},
    Children = [MacChild | CmtsChildren],
    supervisor:check_childspecs(Children),
    {ok, {RestartStrategy, Children}}.
