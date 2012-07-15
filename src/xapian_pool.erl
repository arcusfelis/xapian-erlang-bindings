%% This module provides `start_link' function for Poolboy.
%% This module is used for grouping a set of __readers__.
-module(xapian_pool).
-define(SERVER, xapian_server).
-define(WORKER, ?MODULE).
-define(SUPERVISOR, xapian_pool_sup).

-ifdef(TEST).
-import(xapian_helper, [testdb_path/1]).
-include_lib("eunit/include/eunit.hrl").
-define(POOL, ?MODULE).
-record(test_pool, {names, pool_pids}).
-endif.

%% ------------------------------------------------------------------
%% Export
%% ------------------------------------------------------------------

%% API
-export([ open/3
        , checkout/2
        , close/1]).

%% Callbacks
-export([start_link/1]).


%% ------------------------------------------------------------------
%% Types
%% ------------------------------------------------------------------

-type pool_param() :: 
      {name, atom() | {local, atom()} | {global, atom()}}
    | {worker_module, atom()}
    | {size, non_neg_integer()}
    | {max_overflow, non_neg_integer()}.


%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% For PoolParams see https://github.com/devinus/poolboy
%%
%% * `name': the pool name
%% * `worker_module': the module that represents the workers
%% * `size': maximum pool size
%% * `max_overflow': maximum number of workers created if pool is empty

-spec open([pool_param()], iolist(), [term()]) -> {ok, pid()} | {error, term}.

open(PoolParams, Path, Params) ->
    Name = proplists:get_value(name, PoolParams), 
    FixedNameParam = 
    if
        is_atom(Name), Name =/= undefined ->
            [{name, {local, Name}}];
        true ->
            []
    end,
    ExtPoolParams = FixedNameParam ++ PoolParams ++  
        [ {worker_module, ?WORKER}
        , {worker_params, [Path, Params]}],
    xapian_pool_sup:start_pool(ExtPoolParams).


%% @doc Destroy the pool asynchronously.
-spec close(atom() | pid()) -> term().

close(PoolName) ->
    poolboy:stop(PoolName).


%% Gets poolers from the list and runs Fun with 
checkout(PoolNames, Fun) ->
    PoolWorkers = 
    [poolboy:checkout(PoolName) || PoolName <- PoolNames],
    try
        Fun(PoolWorkers)
    after
        lists:zipwith(fun(Name, Worker) ->
            poolboy:checkin(Name, Worker)
            end,  PoolNames, PoolWorkers)
    end.


%% Called by Poolboy
start_link(Args) ->
    [Path, Params] = proplists:get_value(worker_params, Args),
    ?SERVER:start_link(Path, Params).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

try_start_application() ->
    case application:start(xapian) of
        ok -> ok;
        {error,{already_started,xapian}} ->
            ok
    end.


anonym_pool_test_() ->
    try_start_application(),
    %% The database path is "priv/Name".
    Name = 'anonym_pool',
    create_database(Name),
    %% Create an anonymous pool
    {ok, Pool} = ?POOL:open([], testdb_path(Name), []),
    IsAlive = is_process_alive(Pool),
    ?POOL:close(Pool),
    MonRef = erlang:monitor(process, Pool),
    IsAliveAfterClose =
    receive
        %% Error in the transaction body.
        {'DOWN', MonRef, process, Pool, _Reason} ->
            false
    after 500 ->
            true
    end,
    [ ?_assert(IsAlive), ?_assertNot(IsAliveAfterClose) ].


pool_test_() ->
    try_start_application(),
    {foreach,
    fun pool_setup/0,
    fun pool_clean/1,
    [ fun access_by_name_case/1
    , fun access_by_pid_case/1
    ]}.


pool_setup() ->
    Names = [pool1, pool2, pool3],
    %% Create DBs 
    [create_database(Name) || Name <- Names],
    
    %% Create pools of readers
    Workers = 
    [begin
        {ok, Pid} = ?POOL:open([{name, Name}], testdb_path(Name), []),
        Pid
     end || Name <- Names],
    #test_pool{names=Names, pool_pids=Workers}.


pool_clean(#test_pool{names=Names}) ->
    [ok = ?POOL:close(Name) || Name <- Names],
    ok.


access_by_pid_case(#test_pool{pool_pids=Pools}) ->
    Fun = fun([W1, W2, W3] = Workers) ->
        lists:all(fun erlang:is_pid/1, Workers)
        end,
    Result = ?POOL:checkout(Pools, Fun),
    [ ?_assertEqual(Result, true)
    ].


access_by_name_case(#test_pool{names=Pools}) ->
    [].


create_database(Name) ->
    Modes = [write, create, overwrite],
    {ok, Server} = xapian_server:open(testdb_path(Name), Modes),
    xapian_server:close(Server).

-endif.
