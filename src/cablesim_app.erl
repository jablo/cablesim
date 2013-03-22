%%%-------------------------------------------------------------------
%% @author Jacob Lorensen
%% @copyright Jacob Lorensen 2013
%% @doc Cable modem simulation application
%% @end
%%%-------------------------------------------------------------------
-module(cablesim_app).
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
mk_cms(NMin, NMin, _, _, _, _) -> ok;
mk_cms(N, NMin, Cmts, CMTempl, MTATempl, CPETempl) ->
    io:format("N=~p NMin=~p Cmts=~p~n", [N, NMin, Cmts]),
    CM = device:mk_device(
           device:mk_pname(cm, N), Cmts, 
           device:mk_mac({0,0,0}, 3*N), CMTempl),
    MTA = device:mk_device(
            device:mk_pname(cm, N, mta), CM#device.server_id,
            device:mk_mac({0,0,0}, 3*N+1), MTATempl),
    CPE = device:mk_device(
            device:mk_pname(cm, N, cpe), CM#device.server_id,
            device:mk_mac({0,0,0}, 3*N+2), CPETempl),
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
    io:format("Creating ~p cable modems~n", [N]),
    mk_cms(N, 0, cmts, CMT, MTAT, CPET).
%    mk_cms(N+1000, 1000, cmts2, CMT, MTAT, CPET).
