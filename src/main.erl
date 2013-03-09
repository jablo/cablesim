-module(main).
-export([run/0, stop/1]).

run() ->
    CmtsId = cmts,
    LogFile = "/tmp/log.log",
    cmts:start_link(CmtsId, LogFile),
    CmId = cm1,
    cm:start_link(CmtsId, CmId, {11,11,11,11,11,11}),
    {CmtsId, CmId}.

stop({CmtsId, CmId}) ->
    cm:stop(CmId),
    cmts:stop(CmtsId).
