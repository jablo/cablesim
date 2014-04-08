%%%-------------------------------------------------------------------
%% @author Jacob Lorensen
%% @copyright Jacob Lorensen 2013
%% @doc Cable modem simulation application
%% @end
%%%-------------------------------------------------------------------
-module(cablesim_app).
-behaviour(application).
-include("debug.hrl").
-include("device.hrl").

%% Convenience api
-export([start_all/0, stop_all/0]).

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
    io:format("~nSupervisor started~n"),
    simulate(Cms),
    {ok, self()}.

stop(_State) ->
    ok.

%%%===============================================================================
%%%
%%% Web service server startup code
%%%
%%%===============================================================================

start_all() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(cablesim),
    % cowboy routes
    Host = '_',
    Port = 8799,
    Dispatch = cowboy_router:compile(
                 [{Host, [{"/cmts", cablesim_rest, []}]}]
                ),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]).

stop_all() ->
    application:stop(crypto),
    application:stop(ranch),
    application:stop(cowboy),
    application:stop(cablesim).
    

%%%===========================================================================
%%%
%%% Simulation scenario setup code
%%%
%%%===========================================================================

%% @doc
%% create and start a cable modem based on templates for the components
%% @end
mk_cm(Cmts, CmId, MacVendor, CmTempl, MtaTempl, CpeTemplList) ->
    io:format(">>>> CMTempl: ~p MtaTempl: ~p CpeTemplList: ~p~n", [CmTempl, MtaTempl, CpeTemplList]),
    CM = device:mk_device(CmId, Cmts, mac_generator:nextmac(MacVendor), CmTempl),
    MTA = case MtaTempl of
              undefined -> undefined;
              { TheMtaTempl, MtaMacVendor } ->
                  device:mk_device(
                    device:mk_pname(CmId, mta), CM#device.server_id,
                    mac_generator:nextmac(MtaMacVendor), TheMtaTempl);
              _ ->
                  device:mk_device(
                    device:mk_pname(CmId, mta), CM#device.server_id,
                    mac_generator:nextmac(MacVendor), MtaTempl)
          end,
    CPEList = lists:map(fun ({T,N}) ->
                                case T of
                                    undefined -> undefined;
                                    { TheCpeTempl, CpeMacVendor } ->
                                        device:mk_device(
                                          device:mk_pname(CmId, cpe, N), CM#device.server_id,
                                          mac_generator:nextmac(CpeMacVendor), TheCpeTempl);
                                    _ ->
                                        device:mk_device(
                                          device:mk_pname(CmId, cpe, N), CM#device.server_id,
                                          mac_generator:nextmac(MacVendor), T)
                                end
                        end,
                        lists:zip(CpeTemplList, lists:seq(1,length(CpeTemplList)))),
%    io:format(">>>> CM: ~p MTA: ~p CPEs: ~p~n", [CM, MTA, CPEList]),
    BehindList = lists:filter(fun (E) -> 
                                      case E of
                                          undefined -> false;
                                          _ -> true
                                      end
                              end, [MTA | CPEList]),
    device:start_cablemodem(CM, BehindList),
    cm:poweron(CmId).

%% @doc
%% Start a simulation of a number of CG3000 cable modems with
%% built in MTA and CPE
%% @spec simulate(N) -> ok
%% @end
simulate([]) -> ok;

simulate([{CmId, MacVendor, CmTmplName, MtaTmplName, CpeTmplNameList, Cmts} |T]) ->
    CmTmpl = device:cm_db(CmTmplName),
    MtaTmpl = device:mta_db(MtaTmplName),
    io:format("simulate: CpeTmplNameList ~p~n", [CpeTmplNameList]),
    CpeTmplList = lists:map(fun (X) -> device:cpe_db(X) end, CpeTmplNameList),
    mk_cm(Cmts, CmId, MacVendor, CmTmpl, MtaTmpl, CpeTmplList),
    simulate(T).

