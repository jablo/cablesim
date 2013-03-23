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
    {ok, Cmtses} = application:get_env(cablesim, cmtslist),
    io:format("Starting Cmtses: ~p~n", [Cmtses]),
    {ok, Cms} = application:get_env(cablesim, cmlist),
    io:format("Starting Cms: ~p~n", [Cms]),
    cablesim_sup:start_link(Cmtses, Cms),
    io:format("Supervisor started~nTesting mac: ~p~n ", [mac_generator:nextmac({1,2,3})]),
    simulate(Cms),
    {ok, self()}.

stop(_State) ->
    ok.

%% @doc
%% create and start a cable modem based on templates for the components
%% @end
mk_cm(Cmts, CmId, MacVendor, CmTempl, MtaTempl, CpeTemplList) ->
    CM = device:mk_device(CmId, Cmts, mac_generator:nextmac(MacVendor), CmTempl),
    MTA = device:mk_device(
            device:mk_pname(cm, mta), CM#device.server_id,
            mac_generator:nextmac(MacVendor), MtaTempl),
    CPEList = lists:map(fun ({T,N}) ->
                                device:mk_device(
                                  device:mk_pname(cm, cpe, N), CM#device.server_id,
                                  mac_generator:nextmac(MacVendor), T)
                        end,
                        lists:zip(CpeTemplList, lists:seq(1,length(CpeTemplList)))),
    device:start_cablemodem(CM, [MTA| CPEList]),
    cm:poweron(CmId).

%% @doc
%% Start a simulation of a number of CG300 cable modems with
%% built in MTA and CPE
%% @spec simulate(N) -> ok
%% @end
simulate([]) -> ok;

simulate([{CmId, MacVendor, CmTmplName, MtaTmplName, CpeTmplNameList, Cmts} |T]) ->
    CmTmpl = device:cm_db(CmTmplName),
    MtaTmpl = device:mta_db(MtaTmplName),
    CpeTmplList = lists:map(fun (X) -> device:cpe_db(X) end, CpeTmplNameList),
    mk_cm(Cmts, CmId, MacVendor, CmTmpl, MtaTmpl, CpeTmplList),
    simulate(T).
