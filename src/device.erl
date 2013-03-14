%%%-------------------------------------------------------------------
%%% File    : device
%%% Author  : Jacob Lorensen
%%% Description : a database of modem model signatures
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%-------------------------------------------------------------------
-module(device).
%-compile([debug_info, export_all]).

-export([cpedb/0]).

-include("device.hrl").

cpedb() ->
    [#device_template
     {id = cg3000_cm,
      send_packet_fun = fun (D, P) -> cm:send_packet(D#device.server_id, P) end,
      linkstate_fun = fun (D, B) -> cm:linkstate(D#device.server_id, B) end,
      vendor_class_id = "docsis3.0",
      vendor_options = [{2,"ECM"},
                        {3,"ECM:EMTA:EPS"},
                        {4,"2BG104UJ01191"},
                        {5,"1.04"},
                        {6,"3.9.21.7.mp2.V0016RC1"},
                        {7,"2.3.0"},
                        {8,"00095B"},
                        {9,"CG3000"},
                        {10,"Netgear"},
                        {254,"\b"}],
      client_id_fun = fun (#device{mac={A,B,C,D,E,F}}) -> [16#ff,C,D,E,F,0,03,00,01,A,B,C,D,E,F] end,
      parameter_request_list = [1,2,3,4,7,6,122]
     },
     #device_template
     {id = cg3000_mta,
      send_packet_fun = fun (D, P) -> 
                                cm:relay_packet(D#device.upstream_id, P) 
                        end,
      linkstate_fun = fun (_, _) -> ok end,
      vendor_class_id = 
          "pktc1.0:051f0101000201020901010b04060903040c01010d01010f010110010912020007",
      vendor_options = 
          [{2,"EMTA"},
           {4,"2BG104UJ01175"},
           {5,"1.04"},
           {6,"3.9.21.7.mp2 "},
           {7,"2.3.0"},
           {8,[48,70,154]},
           {9,"CG3000"},
           {10,"Netgear"},
           {31,[48,70,154,122,54,151]},
           {32,[61,97,197,22]}],
      client_id_fun = fun (#device{mac={A,B,C,D,E,F}}) -> [16#ff,C,D,E,F,0,03,00,01,A,B,C,D,E,F] end,
      parameter_request_list = [1,2,3,6,7,12,15,54,122]
     },
     #device_template
     {id = cg3000_cpe,
      send_packet_fun = fun (D, P) -> cm:relay_packet(D#device.upstream_id, P) end,
      linkstate_fun = fun (_, _) -> ok end,
      vendor_class_id = "",
      vendor_options = [],
      client_id_fun = fun (#device{mac={A,B,C,D,E,F}}) -> [16#ff,C,D,E,F,0,03,00,01,A,B,C,D,E,F] end,
      parameter_request_list = [72,1,2,3,4,6,7,12,15,23,26,54,51,122]
     }
    ].

