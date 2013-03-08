-module(main).
-export([simulate/0]).

simulate() ->
    CmtsId = cmts,
    LogFile = "/tmp/log.log",
    cmts:start_link(CmtsId, LogFile),
    CmId = cm1,
    cm:start_link(CmtsId, CmId, {11,11,11,11,11,11}).
