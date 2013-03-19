%%%-------------------------------------------------------------------
%%% File    : device
%%% Author  : Jacob Lorensen
%%% Description : a database of modem model signatures
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%-------------------------------------------------------------------
-module(device).

-export([cm_db/0, mta_db/0, cpe_db/0, 
         mk_device/4, mk_mac/2, mk_pname/2, mk_pname/3,
         start_cablemodem/2]).

-include("dhcp.hrl").
-include("device.hrl").

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
%% @spec start_cablemodem(Device, BehindDevs) -> ok 
%% @end
start_cablemodem(Device, BehindDevs) ->
    % first build the children, they start in powered-off state
    % and won't do anything before their parent cable modem process
    % is brought online
    lists:foreach(fun (F_D) ->
                          start_dhcp(F_D),
                          start_tod(F_D),
                          start_tftp(F_D)
                  end,
                  [Device | BehindDevs]),
    start_cm(Device, BehindDevs).

%% @doc
%% Make a MAC address 
%% @end
mk_mac({A,B,C}, N) -> {A,B,C, N div 256 div 256,N div 256, N rem 256}.

%% @doc
%% Create a device record from a Process name, upstream
%% process name, Mac address nad device template record.
%% @spec mk_device(Name, Upstream, Mac, Templ) -> Device
%% @end
mk_device(Name, Upstream, Mac, Templ = #device_template{}) ->
    #device{server_id = Name, upstream_id = Upstream, mac = Mac, 
            dhcp_client = mk_pname(Name, dhcp),
            tod_client = mk_pname(Name, tod),
            tftp_client = mk_pname(Name, tftp),
            template = Templ}.

mk_pname(Prefix, N) when is_integer(N) ->
   list_to_atom(atom_to_list(Prefix) ++ integer_to_list(N));
mk_pname(Prefix, Postfix) when is_atom(Postfix) ->
   list_to_atom(atom_to_list(Prefix) ++ atom_to_list(Postfix)).
mk_pname(Prefix, N, Postfix) ->
   list_to_atom(atom_to_list(Prefix) ++ integer_to_list(N)  ++ "_" ++ atom_to_list(Postfix)).


%% @doc
%% Cable modem template database.
%% @end
cm_db() ->
    [#device_template
     {id = cg3000_cm,
      tftp_module = tftp_client,
      send_packet_fun = fun (D, P) -> cm:send_packet(D#device.server_id, P) end,
      linkstate_fun = 
          fun (D, B = offline) -> 
                  io:format("CM ~p is ~p~n", [D#device.server_id, B]),
                  cm:linkstate(D#device.server_id, B),
                  DT = D#device.template,
                  (DT#device_template.tftp_module):set_standby(D#device.tftp_client);
              (D, {B = online, Dhcp}) -> 
                  io:format("CM ~p is ~p~n", [D#device.server_id, B]),
                  cm:linkstate(D#device.server_id, B),
                  DT = D#device.template,
                  (DT#device_template.tftp_module):get_file(
                    D#device.tftp_client, 
                    dhcp_util:get_tftpserver(Dhcp),
                    dhcp_util:get_tftpfile(Dhcp))
          end,
      vendor_class_id = "docsis3.0",
      vendor_options_fun =
          fun (_) -> [{2,"ECM"},
                      {3,"ECM:EMTA:EPS"},
                      {4,"2BG104UJ01191"},
                      {5,"1.04"},
                      {6,"3.9.21.7.mp2.V0016RC1"},
                      {7,"2.3.0"},
                      {8,"00095B"},
                      {9,"CG3000"},
                      {10,"Netgear"},
                      {254,"\b"}]
          end,
      client_id_fun = 
          fun (#device{mac={A,B,C,D,E,F}}) -> 
                  [16#ff,C,D,E,F,0,03,00,01,A,B,C,D,E,F] 
          end,
      parameter_request_list = [1,2,3,4,7,6,122]
     }].

%% @doc
%% Media terminal adapter (telephony device) template database.
%% @end
mta_db() ->
    [#device_template
     {id = cg3000_mta,
      send_packet_fun = fun (D, P) -> 
                                cm:relay_packet(D#device.upstream_id, P) 
                        end,
      linkstate_fun = 
          fun (D, B) -> io:format("MTA ~p is ~p~n", [D#device.server_id, B]),ok
          end,
      vendor_class_id = 
          "pktc1.0:051f0101000201020901010b04060903040c01010d01010f010110010912020007",
      vendor_options_fun = 
          fun (#device{mac={A,B,C,D,E,F}}) 
              ->  [{2,"EMTA"},
                   {4,"2BG104UJ01175"},
                   {5,"1.04"},
                   {6,"3.9.21.7.mp2"},
                   {7,"2.3.0"},
                   {8,[A, B, C]},
                   {9,"CG3000"},
                   {10,"Netgear"},
                   {31,[A, B, C, D, E, F]},
                   {32,[61,97,197,22]}]
          end,
      client_id_fun = fun (#device{mac={A,B,C,D,E,F}}) -> [16#ff,C,D,E,F,0,03,00,01,A,B,C,D,E,F] end,
      parameter_request_list = [1,2,3,6,7,12,15,54,122]
     }].

%% @doc
%% PC / router template database.
%% @end
cpe_db() ->
    [#device_template
     {id = cg3000_cpe,
      send_packet_fun = fun (D, P) -> cm:relay_packet(D#device.upstream_id, P) end,
      linkstate_fun = 
          fun (D, B) -> io:format("CPE ~p is ~p~n", [D#device.server_id, B]),ok
          end,
      vendor_class_id = "",
      client_id_fun = fun (#device{mac={A,B,C,D,E,F}}) -> [16#ff,C,D,E,F,0,03,00,01,A,B,C,D,E,F] end,
      parameter_request_list = [72,1,2,3,4,6,7,12,15,23,26,54,51,122]
     }
    ].

