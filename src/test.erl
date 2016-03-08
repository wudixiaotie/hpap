-module (test).

-export ([send_msg/2]).

send_msg(Pid, Times) when Times > 0 ->
    Pid ! {job, <<"asdfaSDFAaksjdhfoaiwjef;alskdjflsdf">>},
    send_msg(Pid, Times - 1);
send_msg(_, 0) ->
    ok.