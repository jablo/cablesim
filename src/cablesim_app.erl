-module(cablesim_app).
-compile([debug_info, export_all]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cablesim_sup:start_link().

stop(_State) ->
    ok.
