%% @author Jacob Lorensen
%% @copyright Jacob Lorensen 2013
%% @doc Cable modem simulation application
%% @end
-module(cablesim_app).
%-compile([debug_info, export_all]).
-behaviour(application).
-include("device.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc
%% Start the cablesim application. Starts a cablesim supervisor, and
%% a some cable modem simulation processes.
%% @spec start(StartType, StartArgs) -> {ok, Pid} 
%% @end
start(_StartType, _StartArgs) ->
    error_logger:logfile({open, "/tmp/cablesim.log"}),
    X = cablesim_sup:start_link(),
    simulate(1),
    X.

stop(_State) ->
    ok.

%% @doc
%% Create and start N cable modems with MTA and CPE behind it 
%% based on specified CM, MTA and CPE device templates
%% @end
mk_cms(0, _, _, _, _, _) -> ok;
mk_cms(N, NMin, Cmts, CMTempl, MTATempl, CPETempl) ->
    CM = device:mk_device(
           device:mk_pname(cm, N+NMin), Cmts, 
           device:mk_mac({0,0,0}, 3*N+NMin), CMTempl),
    MTA = device:mk_device(
            device:mk_pname(cm, N+NMin, mta), CM#device.server_id,
            device:mk_mac({0,0,0}, 3*N+NMin+1), MTATempl),
    CPE = device:mk_device(
            device:mk_pname(cm, N+NMin, cpe), CM#device.server_id,
            device:mk_mac({0,0,0}, 3*N+NMin+2), CPETempl),
    device:start_cablemodem(CM, [MTA, CPE]),
    cm:poweron(device:mk_pname(cm, N)),
    mk_cms(N-1, NMin, Cmts, CMTempl, MTATempl, CPETempl).

%% @doc
%% Start a simulation of a number of CG300 cable modems with
%% built in MTA and CPE
%% @spec simulate(N) -> ok
%% @end
simulate(N) ->
    [CMT|_] = device:cm_db(),
    [MTAT|_] = device:mta_db(),
    [CPET|_] = device:cpe_db(),
    mk_cms(N, 0, cmts, CMT, MTAT, CPET),
    mk_cms(N, 1000, cmts2, CMT, MTAT, CPET).
