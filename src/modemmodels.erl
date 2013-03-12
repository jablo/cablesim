%%%-------------------------------------------------------------------
%%% File    : modemmodels
%%% Author  : Jacob Lorensen
%%% Description : a database of modem model signatures
%%%
%%% Created : 08 March 2013 by Jacob Lorensen <jalor@yousee.dk> 
%%%-------------------------------------------------------------------
-module(modemmodels).

-export([cpedb/0]).

%o43() ->
%    <<43:8, 98:8, 
%      16#02:8,16#03:8,16#45:8,16#43:8,16#4d:8,16#03:8,16#0c:8,16#45:8,16#43:8,16#4d:8,16#3a:8,16#45:8,16#4d:8,16#54:8,
%      16#41:8,16#3a:8,16#45:8,16#50:8,16#53:8,16#04:8,16#0d:8,16#32:8,16#42:8,16#47:8,16#31:8,16#30:8,16#34:8,16#55:8,
%      16#4a:8,16#30:8,16#31:8,16#31:8,16#39:8,16#31:8,16#05:8,16#04:8,16#31:8,16#2e:8,16#30:8,16#34:8,16#06:8,16#15:8,
%      16#33:8,16#2e:8,16#39:8,16#2e:8,16#32:8,16#31:8,16#2e:8,16#37:8,16#2e:8,16#6d:8,16#70:8,16#32:8,16#2e:8,16#56:8,
%      16#30:8,16#30:8,16#31:8,16#36:8,16#52:8,16#43:8,16#31:8,16#07:8,16#05:8,16#32:8,16#2e:8,16#33:8,16#2e:8,16#30:8,
%      16#08:8,16#06:8,16#30:8,16#30:8,16#30:8,16#39:8,16#35:8,16#42:8,16#09:8,16#06:8,16#43:8,16#47:8,16#33:8,16#30:8,
%      16#30:8,16#30:8,16#0a:8,16#07:8,16#4e:8,16#65:8,16#74:8,16#67:8,16#65:8,16#61:8,16#72:8,16#fe:8,16#01:8,16#08:8
%    >>.

%o43() ->
%    << 43:8, 98:8,
%      16#02:8,16#03:8,16#45:8,16#43:8,16#4d:8,16#03:8,16#0c:8,16#45:8,16#43:8,16#4d:8,16#3a:8,16#45:8,16#4d:8,16#54:8,
%      16#41:8,16#3a:8,16#45:8,16#50:8,16#53:8,16#04:8,16#0d:8,16#32:8,16#42:8,16#47:8,16#31:8,16#30:8,16#34:8,16#55:8,
%      16#4a:8,16#30:8,16#31:8,16#31:8,16#39:8,16#31:8,16#05:8,16#04:8,16#31:8,16#2e:8,16#30:8,16#34:8,16#06:8,16#15:8,
%      16#33:8,16#2e:8,16#39:8,16#2e:8,16#32:8,16#31:8,16#2e:8,16#37:8,16#2e:8,16#6d:8,16#70:8,16#32:8,16#2e:8,16#56:8,
%      16#30:8,16#30:8,16#31:8,16#36:8,16#52:8,16#43:8,16#31:8,16#07:8,16#05:8,16#32:8,16#2e:8,16#33:8,16#2e:8,16#30:8,
%      16#08:8,16#06:8,16#30:8,16#30:8,16#30:8,16#39:8,16#35:8,16#42:8,16#09:8,16#06:8,16#43:8,16#47:8,16#33:8,16#30:8,
%      16#30:8,16#30:8,16#0a:8,16#07:8,16#4e:8,16#65:8,16#74:8,16#67:8,16#65:8,16#61:8,16#72:8,16#fe:8,16#01:8,16#08:8
%    >>.

% cpe - customer placed equipment
% Describes both cable modems, mta and routers/computers
-record(cpe, {
          id,                         % short atom id
          type = cm,                  % cm, mta or cpe
          vendor,                     % vendor string
          model,                      % model string
          firmware,                   % firmware version
          hwversion,                  % hardware version
          dhcp_module = dhcp_client,  % dhcp state machine implementation
          tftp_module = undefined,    % tftp implementing state machine
          tod_module = undefined,     % tod protocol implementation
          vendor_options = [],        % dhcp option43 options
          vendor_class_id,            % dhcp option60 vendor class string
          client_identifier = [],     % dhcp option61 client identifier
          parameter_request_list = [] % dhcp option55
         }). 

cpedb() ->
    [#cpe{id = 1,
          vendor = "Netgear",
          model = "CG3000",
          firmware = "1.0",
          hwversion = "1",
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
          vendor_class_id = "docsis3.0"}].

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of dhcp client options that is specific for a given
%% combination of Vendor, Model, HWVersion and Firmware
%%
%% @spec (String, String, String, String) -> [{int, ???}]
%% @end
%%--------------------------------------------------------------------




%-define(DHO_VENDOR_ENCAPSULATED_OPTIONS, 43).
%-define(DHO_VENDOR_CLASS_IDENTIFIER,     60).
%-define(DHO_DHCP_AGENT_OPTIONS,          82). %% rfc3046

%client-id=ff:9a:7a:36:0c:00:03:00:01:30:46:9a:7a:36:0c,
