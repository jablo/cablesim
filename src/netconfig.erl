-module(netconfig).

-export([network_config/3]).

-include("debug.hrl").

network_config(Interface, _Gi = {I1, I2, I3, I4}, _Mask = {M1, M2, M3, M4}) ->
    ?debug("network_config(~p, ~p, ~p)~n", [Interface, _Gi, _Mask]),
    PD = priv_dir(?MODULE),
    ?debug("priv_dir: ~p~n", [PD]),
    os_cmd_exitstatus("netifcfg", PD ++ "/" ++ "ipconfig.sh " ++ 
                          Interface ++ " " ++ 
                          integer_to_list(I1) ++ "." ++ integer_to_list(I2) ++ "." ++ 
                          integer_to_list(I3) ++ "." ++ integer_to_list(I4) ++ " " ++ 
                          integer_to_list(M1) ++ "." ++ integer_to_list(M2) ++ "." ++ 
                          integer_to_list(M3) ++ "." ++ integer_to_list(M4) ++ " ");
network_config(_I, _G, _M) ->
    ?debug("netconfig:network_config(~p, ~p, ~p) no match ~n", [_I,_G,_M]),
    error.


%% in module util
os_cmd_exitstatus(Action, Cmd) ->
    ?debug("Action ~p Shell command: ~p", [Action, Cmd]),
    try erlang:open_port({spawn, Cmd}, [exit_status, stderr_to_stdout]) of
        Port -> 
            os_cmd_exitstatus_loop(Action, Port)
    catch
        _:Reason ->
            case Reason of
                badarg ->
                    Message = "Bad input arguments";
                system_limit ->
                    Message = "All available ports in the Erlang emulator are in use";
                _ ->
                    Message = file:format_error(Reason)
            end,
            ?error("~ts: shell command error: ~ts", [Action, Message]),
            error
    end.

os_cmd_exitstatus_loop(Action, Port) ->
    receive
        {Port, {data, _Data}} ->
            ?debug("~ts... Shell output: ~ts", [Action, _Data]),
            os_cmd_exitstatus_loop(Action, Port);
        {Port, {exit_status, 0}} ->
            ?info("~ts finished successfully", [Action]),
            ok;
        {Port, {exit_status, Status}} ->
            ?error("~ts failed with exit status ~p", [Action, Status]),
            error;
        {'EXIT', Port, Reason} ->
            ?error("~ts failed with port exit: reason ~ts", 
                         [Action, file:format_error(Reason)]),
            error
    end.

priv_dir(Mod) ->
    Ebin = filename:dirname(code:which(Mod)),
    filename:join(filename:dirname(Ebin), "priv").
