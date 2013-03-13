-module(main).
-export([r/0, stop/0]).
-include("device.hrl").

r() ->
    CmtsId = cmts,
    LogFile = "/tmp/log.log",
    error_logger:logfile({open, LogFile}),
    cmts:start_link(CmtsId, {192,168,56,102}, {192,168,56,105}),
    [CMTempl, MTATempl, _CPETempl] = modemmodels:cpedb(),
    CM_Dev = #device{server_id = cm1, upstream_id = CmtsId, mac = {0,0,0,0,0,1}, template = CMTempl},
    MTA_Dev = #device{server_id = mta1, upstream_id = cm1, mac = {0,0,0,0,0,2}, template = MTATempl},
    cm:start_link(CM_Dev, [MTA_Dev]).

stop() ->
    cmts:stop(cmts).
