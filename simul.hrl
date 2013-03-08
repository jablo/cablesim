-ifndef(_SIMUL).
-define(_SIMUL, true).

-record(packet, {
          direction,             %% upstream ro downstream
          cmid,                  %% cable modem id (fsm name)
          data                   %% data packet
         }).
              


-endif.
