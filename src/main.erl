-module(main).
-export([r/0, run/0, stop/1]).
-include("device.hrl").

r() ->
    CmtsId = cmts,
    LogFile = "/tmp/log.log",
    error_logger:logfile({open, LogFile}),
    cmts:start_link(CmtsId, {192,168,56,102}, {192,168,56,105}),
    [CMTemplate|_] = modemmodels:cpedb(),
    CmId1 = cm1,
    cm:start_link(CmtsId, CmId1, #device{mac={00,00,00,00,00,01}, template=CMTemplate}).

run() ->
    CmtsId = cmts,
    LogFile = "/tmp/log.log",
    cmts:start_link(CmtsId, LogFile),
    CmId1 = cm1,
    cm:start_link(CmtsId, CmId1, {00,00,00,00,00,01}),
    CmId2 = cm2,
    cm:start_link(CmtsId, CmId2, {00,00,00,00,00,02}),
    CmId3 = cm3,
    cm:start_link(CmtsId, CmId3, {00,00,00,00,00,03}),
    CmId4 = cm4,
    cm:start_link(CmtsId, CmId4, {00,00,00,00,00,04}),
    CmId5 = cm5,
    cm:start_link(CmtsId, CmId5, {00,00,00,00,00,05}),
    CmId6 = cm6,
    cm:start_link(CmtsId, CmId6, {00,00,00,00,00,06}),
    CmId7 = cm7,
    cm:start_link(CmtsId, CmId7, {00,00,00,00,00,07}),
    CmId8 = cm8,
    cm:start_link(CmtsId, CmId8, {00,00,00,00,00,08}),
    CmId9 = cm9,
    cm:start_link(CmtsId, CmId9, {00,00,00,00,00,09}),
    CmId10 = cm10,
    cm:start_link(CmtsId, CmId10, {00,00,00,00,00,10}),
    RE = {CmtsId, Cms = [CmId1,CmId2,CmId3,CmId4,CmId5,CmId6,CmId7,CmId8,CmId9,CmId10]},
    lists:foreach (fun (X) -> cm:poweron(X) end,  Cms),
    RE.

stop({CmtsId, Cms}) ->
    cmts:stop(CmtsId),
    lists:foreach (fun (X) -> cm:stop(X) end, Cms).
