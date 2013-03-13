%%%-------------------------------------------------------------------
%%% File    : modemmodels
%%% Author  : Jacob Lorensen
%%% Description : a database of modem model signatures
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%-------------------------------------------------------------------
-module(modemmodels).

-export([cpedb/0]).

-include("device.hrl").

cpedb() ->
    [#device_template
     {id = cg3000_cm,
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
      vendor_class_id = "",
      vendor_options = [],
      client_id_fun = fun (#device{mac={A,B,C,D,E,F}}) -> [16#ff,C,D,E,F,0,03,00,01,A,B,C,D,E,F] end,
      parameter_request_list = [72,1,2,3,4,6,7,12,15,23,26,54,51,122]
     }
    ].

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of dhcp client options that is specific for a given
%% combination of Vendor, Model, HWVersion and Firmware
%%
%% @spec (String, String, String, String) -> [{int, ???}]
%% @end
%%--------------------------------------------------------------------



%giaddr=10.23.0.1,
%dhcp-message-type=01,
%relay-agent-circuit-id=01:04:80:03:07:7a,
%dhcp-parameter-request-list=1,2,3,4,7,6,122, 
%client-id-created-from-mac-address=0,
%relay-agent-remote-id=02:06:30:46:9a:7a:36:0c,
%client-id=ff:9a:7a:36:0c:00:03:00:01:30:46:9a:7a:36:0c,
%dhcp-class-identifier=docsis3.0:,
%chaddr=30:46:9a:7a:36:0c,
%vendor-encapsulated-options=
%02:03:45:43:4d:03:0c:45:43:4d:3a:45:4d:54:41:3a:45:50:53:04:0d:32:42:47:
%31:30:34:55:4a:30:31:31:39:31:05:04:31:2e:30:34:06:15:33:2e:39:2e:32:31:
%2e:37:2e:6d:70:32:2e:56:30:30:31:36:52:43:31:07:05:32:2e:33:2e:30:08:06:
%30:30:30:39:35:42:09:06:43:47:33:30:30:30:0a:07:4e:65:74:67:65:61:72:fe:01:08




%-define(DHO_VENDOR_ENCAPSULATED_OPTIONS, 43).
%-define(DHO_VENDOR_CLASS_IDENTIFIER,     60).
%-define(DHO_DHCP_AGENT_OPTIONS,          82). %% rfc3046

%client-id=ff:9a:7a:36:0c:00:03:00:01:30:46:9a:7a:36:0c,

