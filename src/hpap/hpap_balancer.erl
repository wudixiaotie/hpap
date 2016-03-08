-module(hpap_balancer).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName) ->
    Name = hpap:balancer_name(PoolName),
    gen_server:start_link({local, Name}, ?MODULE, [], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) -> {ok, []}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> io:format("balancer:=============1~n"),{noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================