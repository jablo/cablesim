-module(cablesim_sup).
%-compile([debug_info, export_all]).

-behaviour(supervisor).

-include("device.hrl").

%% API
-export([start_link/0, start_cablemodem/2, do_startcm/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_cablemodem(Device, BehindDevs) ->
    % first start the cable modem process
    supervisor:start_child(
      ?MODULE, {Device#device.server_id,
                {cm, start_link, [Device, BehindDevs]}, 
                transient, brutal_kill, worker, dynamic}),
    % then create cable modem's dhcp module
    DeviceTempl = Device#device.template,
    supervisor:start_child(
      ?MODULE, {Device#device.dhcp_client,
                {DeviceTempl#device_template.dhcp_module, start_link,
                 [Device#device.dhcp_client, Device]},
                transient, brutal_kill, worker, dynamic}),
    % Create dhcp server component on all behind-devices
    lists:foreach(fun (F_D) ->
                          F_DT = F_D#device.template,
                          supervisor:start_child(
                            ?MODULE, {F_D#device.dhcp_client,
                                      {F_DT#device_template.dhcp_module, 
                                       start_link, [F_D#device.dhcp_client, F_D]},
                                      transient, brutal_kill, worker, dynamic})
                  end,
                  BehindDevs).

do_startcm() ->
    [CMTempl, MTATempl, CPETempl] = modemmodels:cpedb(),
    CM_Dev = #device{server_id = cm1, upstream_id = cmts, mac = {0,0,0,0,0,1}, 
                     dhcp_client = cm1_dhcp,
                     template = CMTempl},
    MTA_Dev = #device{server_id = cm1_mta, upstream_id = cm1, mac = {0,0,0,0,0,2},
                      dhcp_client = cm1_mtadhcp,
                      template = MTATempl},
    CPE_Dev = #device{server_id = cm1_cpe, upstream_id = cm1, mac = {0,0,0,0,0,3},
                      dhcp_client = cm1_cpedhcp,
                      template = CPETempl},
    cablesim_sup:start_cablemodem(CM_Dev, [CPE_Dev,MTA_Dev]).


%start_cmts(CmtsId, GiAddress, DhcpAddress) ->
%    supervisor:start_child(
%      ?MODULE, {CmtsId, 
%                {cmts, start_link, [CmtsId, GiAddress, DhcpAddress]},
%                transient, brutal_kill, worker, dynamic}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    RestartStrategy = {one_for_one, 10, 60},
    ChildSpec = {cmts, 
                 {cmts, start_link, [cmts, {192,168,56,102}, {192,168,56,105}]},
                 permanent, brutal_kill, worker, [ch1]},
    Children = [ChildSpec],
    {ok, {RestartStrategy, Children}}.
