-module(cablesim_app).
%-compile([debug_info, export_all]).
-behaviour(application).
-include("device.hrl").

%% Application callbacks
-export([start/2, stop/1, start_cablemodem/2]).

-export([mk_cms/4]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc
%% Start the cablesim application. Starts a cablesim supervisor, and
%% a some cable modem simulation processes.
%% @spec start(_, _) -> {ok, Pid} | 
%% @end
start(_StartType, _StartArgs) ->
    error_logger:logfile({open, "/tmp/cablesim.log"}),
    X = cablesim_sup:start_link(),
    simulate(2),
    X.

stop(_State) ->
    ok.

%% @doc
%% Create a cable modem transient server process
%% @spec start_cm(Device, BehindDevs) -> {ok, Pid} | ignore | {error, Error}
%% @end
start_cm(Device, BehindDevs) ->
    supervisor:start_child(
      cablesim_sup, {Device#device.server_id,
                     {cm, start_link, [Device, BehindDevs]}, 
                     transient, 5000, worker, dynamic}).

%% @doc
%% Create a dhcp client module server process for a Device. Returns ignore
%% if this device does not have a dhcp module and thus no processes should
%% be created. (This would be a very strange network device indeed).
%% @spec start_dhcp(Device) -> { ok, Pid} | ignore | {error, Error}
%% @end
start_dhcp(Device) ->
    DeviceTempl = Device#device.template,
    DhcpModule = DeviceTempl#device_template.dhcp_module,
    case DhcpModule of
        undefined -> ignore;
        _ ->
            Name = Device#device.dhcp_client,
            supervisor:start_child(
              cablesim_sup, {Name,
                             {DhcpModule, start_link, [Name, Device]},
                             transient, 5000, worker, dynamic})
    end.

%% @doc
%% Create a tod client module server process for a Device. Returns ignore
%% if this device does not have a tod module and thus no processes should
%% be created.
%% @spec start_tod(Device) -> { ok, Pid} | ignore | {error, Error}
%% @end
start_tod(Device) ->
    DeviceTempl = Device#device.template,
    ToDModule = DeviceTempl#device_template.tod_module,
    case ToDModule of
        undefined -> ignore;
        _ ->
            Name = Device#device.tod_client,
            supervisor:start_child(
              cablesim_sup, {Name,
                             {ToDModule, start_link, [Name, Device]},
                             transient, 5000, worker, dynamic}
             )
    end.

%% @doc
%% Create a tftp client module server process for a Device. Returns ignore
%% if this device does not have a tftp module and thus no processes should
%% be created.
%% @spec start_tftp(Device) -> { ok, Pid} | ignore | {error, Error}
%% @end
start_tftp(Device) ->
    DeviceTempl = Device#device.template,
    TftpModule = DeviceTempl#device_template.tftp_module,
    case TftpModule of
        undefined -> ignore;
        _ ->
            Name = Device#device.tftp_client,
            supervisor:start_child(
              cablesim_sup, {Name,
                             {TftpModule, start_link, [Name, Device]},
                             transient, 5000, worker, dynamic}
             )
    end.

%% @doc
%% Create a full cable modem simulation, ie. create processes for
%% the cable modem, it's dhcp client, tftp client, tod client
%% and - recursively - create the same for any device running 
%% behind the cable modem.
%% @spec start_cablemodem(Device, BehindDevs) -> ok | ?
%% @end
start_cablemodem(Device, BehindDevs) ->
    start_cm(Device, BehindDevs),
    lists:foreach(fun (F_D) ->
                          start_dhcp(F_D),
                          start_tod(F_D),
                          start_tftp(F_D)
                  end,
                  [Device | BehindDevs]).


%% @doc
%% Create a device record from a Process name, upstream
%% process name, Mac address nad device template record.
%% @spec mk_device(Name, Upstream, Mac, Templ) -> Device
%% @end
mk_device(Name, Upstream, Mac, Templ = #device_template{}) ->
    #device{server_id = Name, upstream_id = Upstream, mac = Mac, 
            dhcp_client = mk_atom(Name, dhcp),
            tod_client = mk_atom(Name, tod),
            tftp_client = mk_atom(Name, tftp),
            template = Templ}.

mk_atom(Prefix, N) when is_integer(N) ->
   list_to_atom(atom_to_list(Prefix) ++ integer_to_list(N));
mk_atom(Prefix, Postfix) when is_atom(Postfix) ->
   list_to_atom(atom_to_list(Prefix) ++ atom_to_list(Postfix)).
mk_atom(Prefix, N, Postfix) ->
   list_to_atom(atom_to_list(Prefix) ++ integer_to_list(N)  ++ "_" ++ atom_to_list(Postfix)).

%% @doc
%% Make a MAC address 
%% @spec mk_mac({A,B,C,D}, N} -> {A,B,C,D,E,F}.
%% @end
mk_mac({A,B,C,D}, N) -> {A,B,C,D,N div 200, N rem 200}.

%% @doc
%% Create and start N cable modems with MTA and CPE behind it 
%% based on specified CM, MTA and CPE device templates
%% @spec _ -> ok.
%% @end
mk_cms(0, _, _, _) -> ok;
mk_cms(N, CMTempl, MTATempl, CPETempl) ->
    CM = mk_device(mk_atom(cm, N), cmts, mk_mac({0,0,0,0}, 3*N), CMTempl),
    MTA = mk_device(mk_atom(cm, N, mta), CM#device.server_id, mk_mac({0,0,0,0}, 3*N+1), MTATempl),
    CPE = mk_device(mk_atom(cm, N, cpe), CM#device.server_id, mk_mac({0,0,0,0}, 3*N+2), CPETempl),
    start_cablemodem(CM, [MTA, CPE]),
    cm:poweron(mk_atom(cm, N)),
    mk_cms(N-1, CMTempl, MTATempl, CPETempl).

%% @doc
%% Start a simulation of a number of CG300 cable modems with
%% built in MTA and CPE
%% @spec N -> ok.
%% @end
simulate(N) ->
    [CMT, MTAT, CPET] = device:cpedb(),
    mk_cms(N, CMT, MTAT, CPET).
