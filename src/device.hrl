%%%
%%%
%%%
-record(device, {mac, template}).

% cpe - customer placed equipment
% Describes both cable modems, mta and routers/computers
-record(device_template, {
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
          client_id_fun = fun (#device{}) -> [] end,
          client_identifier = [],     % dhcp option61 client identifier
          parameter_request_list = [] % dhcp option55
         }). 



