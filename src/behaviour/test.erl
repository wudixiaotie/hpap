-module (test).

-behaviour (gen_msg).

-export ([start_link/0]).

-export ([init/1, handle_msg/2, terminate/2]).

start_link() ->
    gen_msg:start_link(?MODULE, []).

init([]) ->
    {ok, [], 2000}.

handle_msg(timeout, State) ->
    io:format("=============hibernate~n"),
    % proc_lib:hibernate(gen_msg, enter_loop, [?MODULE, State]),
    {ok, State, 2000};
handle_msg(Any, State) ->
    io:format("=============handle_msg:~p~n", [Any]),
    {ok, State, 2000}.

terminate(Reason, _State) ->
    io:format("=============~p~n", [Reason]),
    ok.