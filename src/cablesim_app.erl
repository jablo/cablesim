-module(cablesim_app).
%-compile([debug_info, export_all]).
-behaviour(application).
-include("device.hrl").

%% Application callbacks
-export([start/2, stop/1, start_equipment/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    X = cablesim_sup:start_link(),
    io:format("X=~p~n", X),
    X.

start_equipment() ->
    LogFile = "/tmp/log.log",
    error_logger:logfile({open, LogFile}),
    ok.

stop(_State) ->
    ok.

