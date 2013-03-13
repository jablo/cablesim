%%%
%%%
%%%
-record(device, {
          mac,                       % device's mac address
          template,                  % device's dhcp template
          dhcp_client,               % device's built in dhcp client
          tftp_client,               % optional built in tftp client in the device (cm and mtas)
          tod_client                 % optional built in tod client in the device (cm)
         }).

% cpe - customer placed equipment
% Describes both cable modems, mta and routers/computers
-record(device_template, {
          id,                         % short atom id
          type,                       % device type
          dhcp_module = dhcp_client,  % dhcp state machine implementation
          tftp_module = undefined,    % tftp implementing state machine
          tod_module = undefined,     % tod protocol implementation
          vendor_options = [],        % dhcp option43 options
          vendor_class_id,            % dhcp option60 vendor class string
          client_id_fun = fun (#device{}) -> [] end,
          client_identifier = [],     % dhcp option61 client identifier
          parameter_request_list = [] % dhcp option55
         }). 



