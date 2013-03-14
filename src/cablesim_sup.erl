-module(cablesim_sup).
%-compile([debug_info, export_all]).

-behaviour(supervisor).

-include("device.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    RestartStrategy = {one_for_one, 10, 60},
    ChildSpec = {cmts, 
                 {cmts, start_link, [cmts, {192,168,56,102}, {192,168,56,105}]},
                 permanent, 5000, worker, [ch1]},
    Children = [ChildSpec],
    {ok, {RestartStrategy, Children}}.
