%%
%% 
%% 

-ifndef(_TFTP).
-define(_TFTP, true).

-record(tftp_request, {
          opcode :: int,             % Operation code
          filename :: string,
          mode :: string
         }).

-record(tftp_data, {
          port :: int,
          block :: int,
          data :: binary
         }).

-record(tftp_ack, { 
          block  :: int
         }).

-record(tftp_err, {
          errcode :: int,
          errstring :: string
         }).

% TFTP 
-define(TFTP_PORT, 69).
          
% TFTP opcode
-define(OC_RRQ, 1).                 % READ Request
-define(OC_WRQ, 2).                 % WRITE Request
-define(OC_DATA, 3).                % DATA packet
-define(OC_ACK, 4).                 % ACK packet
-define(OC_ERR, 5).                 % ERROR message

% TFTP Constants
-define(C_EOS, 0).                  % end of string
-define(C_NETASCII, "netascii").    % netascii transfer mode
-define(C_OCTET, "octet").          % octet or binary transfer mode
-define(C_BLKSIZ, 512).             % TFTP data packet block size

% TFTP error codes
-define(ERR_UNDEF, 0).              % Not defined, see error message (if any).
-define(ERR_FILENOTFOUND, 1).       % File not found.
-define(ERR_ACCESS, 2).             % Access violation.
-define(ERR_DISK, 3).               % Disk full or allocation exceeded.
-define(ERR_ILLEGALOP, 4).          % Illegal TFTP operation.
-define(ERR_UNKNOWNID, 5).          % Unknown transfer ID.
-define(ERR_EXISTS, 6).             % File already exists.
-define(ERR_USER, 7).               % No such user.

-endif.
