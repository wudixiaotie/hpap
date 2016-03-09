-module(hpap_balancer).

-behaviour(gen_server).

% APIs
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool_name, balance_threshold, task_pack_size}).

-include("hpap.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(PoolName, Opts) ->
    Name = hpap:balancer_name(PoolName),
    gen_server:start_link({local, Name}, ?MODULE, [PoolName, Opts], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PoolName, Opts]) ->
    State = #state{pool_name = PoolName,
                   balance_threshold = Opts#options.balance_threshold,
                   task_pack_size = Opts#options.task_pack_size},
    {ok, State}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({task_pack, TaskPack}, #state{pool_name = PoolName} = State) ->
    io:format("balancer:=============1~n"),
    MigrationPath = migration_path(PoolName),
    % ok = migrate(),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

migration_path(PoolName) ->
    WorkerList = supervisor:which_children(PoolName),
    migration_path(WorkerList, []).


migration_path([{WorkerName, Pid, worker, [hpap_worker]}|T], MigrationPath) ->
    {_, MQL} = erlang:process_info(Pid, message_queue_len),
    migration_path(T, [{WorkerName, MQL}|MigrationPath]);
migration_path([_|T], MigrationPath) ->
    migration_path(T, MigrationPath);
migration_path([], MigrationPath) ->
    {ok, MigrationPath}.
% migrate() ->
%     receive
%         {task_pack, TaskPack} ->
%             body
%     after
%         expression ->
%             body
%     end