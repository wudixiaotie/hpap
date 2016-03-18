-module(gen_msg).

% APIs
-export([start_link/2, init/3, start_link/3, init/4, enter_loop/2, enter_loop/3]).

% system message
-export([system_continue/3, system_terminate/4, system_get_state/1,
         system_replace_state/2]).



%% ===================================================================
%% APIs
%% ===================================================================

-callback init(Args :: term()) -> {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate}.
-callback handle_msg(Msg :: term(), State :: term()) -> {ok, NewState :: term()} | {ok, State :: term(), timeout() | hibernate}.
-callback terminate(Reason :: term(), State :: term()) -> ok.

start_link(Module, Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Module, Args]).


start_link(Name, Module, Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Module, Args]).


init(Parent, Module, Args) ->
    do_init(Parent, Module, Args).


init(Parent, Name, Module, Args) ->
    ok = register_name(Name),
    do_init(Parent, Module, Args).


enter_loop(Module, State) ->
    enter_loop(Module, State, infinity).


enter_loop(Module, State, Timeout) ->
    Parent = get_parent(),
    Debug = sys:debug_options([]),
    loop(Parent, Debug, Module, State, Timeout).



%% ===================================================================
%% system message
%% ===================================================================

system_continue(Parent, Debug, [State, Module, Timeout]) ->
    loop(Parent, Debug, Module, State, Timeout).


system_terminate(Reason, _Parent, _Debug, [State, Module, _Timeout]) ->
    terminate(Reason, Module, State).


system_get_state([State, _Module, _Timeout]) ->
    {ok, State, State}.


system_replace_state(StateFun, State) ->
    NewState = StateFun(State),
    {ok, NewState, NewState}.


%% ===================================================================
%% Internal functions
%% ===================================================================

register_name({local, Name}) when is_atom(Name) ->
    erlang:register(Name, self()),
    ok;
register_name({global, Name}) ->
    global:register_name(Name, self()),
    ok.


do_init(Parent, Module, Args) ->
    Debug = sys:debug_options([]),

    case Module:init(Args) of
        {ok, State} ->
            Timeout = infinity;
        {ok, State, Timeout} ->
            ok
    end,

    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, Debug, State, Module, Timeout).


loop(Parent, Debug, Module, State, Timeout) ->
    receive
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, [State, Module, Timeout]);
        Msg ->
            do_loop(Parent, Debug, Module, State, Timeout, Msg)
    after
        Timeout ->
            do_loop(Parent, Debug, Module, State, Timeout, timeout)
    end.


do_loop(Parent, Debug, Module, State, Timeout, Msg) ->
    case catch Module:handle_msg(Msg, State) of
        {ok, NewState} ->
            loop(Parent, Debug, Module, NewState, infinity);
        {ok, NewState, Timeout} ->
            loop(Parent, Debug, Module, NewState, Timeout);
        {'EXIT', Reason} ->
            terminate(Reason, Module, State)
    end.


terminate(Reason, Module, State) ->
    ok = Module:terminate(Reason, State),
    erlang:exit(Reason).


get_parent() ->
    case get('$ancestors') of
        [Parent | _] when is_pid(Parent)->
            Parent;
        [Parent | _] when is_atom(Parent)->
            name_to_pid(Parent);
        _ ->
            exit(process_was_not_started_by_proc_lib)
    end.


name_to_pid(Name) ->
    case whereis(Name) of
        undefined ->
            case global:whereis_name(Name) of
                undefined ->
                    exit(could_not_find_registered_name);
                Pid ->
                    Pid
                end;
        Pid ->
            Pid
    end.