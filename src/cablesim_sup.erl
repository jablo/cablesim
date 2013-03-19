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
    Children = 
        [
         {cmts, 
          {cmts, start_link, [cmts, {192,168,56,102}, {192,168,56,105}]},
          permanent, 5000, worker, [ch1]}
%         {cmts2, 
%          {cmts, start_link, [cmts2, {192,168,56,103}, {192,168,56,105}]},
%          permanent, 5000, worker, [ch1]}
        ],
    {ok, {RestartStrategy, Children}}.
