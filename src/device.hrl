%%%
%%%
%%%
-record(device, {
          server_id,                 % device's server id
          upstream_id,               % upstream device (cmts or cable modem)
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
          % Modules that can implement behaviour. Having variables here makes it possible
          % to implement more than one engine for each behaviour, ie. to model faulty cable modem
          % (we have cable modems that does release/renew every 2-5 seconds!)
          dhcp_module = dhcp_client,  % dhcp state machine implementation (module name)
          tftp_module = undefined,    % tftp implementing state machine (module name)
          tod_module = undefined,     % tod protocol implementation (module name)
          % Callout funs to send packet / communicate link state
          send_packet_fun,
          linkstate_fun,
          % DHCP signature
          vendor_options = [],        % dhcp option43 options
          vendor_class_id,            % dhcp option60 vendor class string
          client_id_fun = fun (#device{}) -> [] end,
          client_identifier = [],     % dhcp option61 client identifier
          parameter_request_list = [] % dhcp option55
         }). 

