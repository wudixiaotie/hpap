-module(hpap_utility).

% APIs
-export([worker_name/2, send_task/2, save_messages/2]).



%% ===================================================================
%% APIs
%% ===================================================================

worker_name(PoolName, Index) ->
    PoolNameStr = erlang:atom_to_list(PoolName),
    IndexStr = erlang:integer_to_list(Index),
    erlang:list_to_atom(PoolNameStr ++ "_" ++ IndexStr).


send_task(N, Pid) when N > 0 ->
    receive
        {task, Task} ->
            Pid ! {task, Task},
            send_task(N - 1, Pid)
    after
        0 ->
            send_task(0, Pid)
    end;
send_task(0, _) ->
    ok.


save_messages(WarehousePid, Index) ->
    receive
        Message ->
            WarehousePid ! {Index, Message},
            save_messages(WarehousePid, Index)
    after
        0 ->
            ok
    end.