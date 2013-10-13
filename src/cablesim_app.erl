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

%    {ok, Cms} = application:get_env(cablesim, cmlist),
% auto-generate a number of identical cable modems
    {ok, CmCount} = application:get_env(cablesim, cmcount),
    {ok, CmVendor} = application:get_env(cablesim, cmvendor),
    Cms = mk_cm_tmpl(CmCount, CmVendor),
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

%% Auto-generate a number of cable modems
mk_cm_tmpl(Count, VMac) ->
	mk_cm_tmpl(Count, VMac, []).
mk_cm_tmpl(0, _, Acc) -> Acc;
mk_cm_tmpl(Count, VMac, Acc) ->
	CmID = mk_id(cm, Count),
	Cm = {CmID, VMac, cg300_cm, no_mta, [cg3000_cpe], cmts},
	mk_cm_tmpl(Count-1, VMac, [Cm|Acc]).

% auto-generate cable modem id
mk_id(Prefix, N) when is_integer(N) ->
   list_to_atom(atom_to_list(Prefix) ++ integer_to_list(N)).

	

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
