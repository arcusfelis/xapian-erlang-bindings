%% This module provides `start_link' function for Poolboy.
%% This module is used for grouping a set of __readers__.
-module(xapian_pool).
-define(SERVER, xapian_drv).
-define(WORKER, ?MODULE).
-define(SUPERVISOR, xapian_pool_sup).

%% API
-export([open/3, checkout/2]).

%% Callbacks
-export([start_link/1]).


%% For PoolParams see https://github.com/devinus/poolboy
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
    {ok, GroupPid} = xapian_pool_sup:start_pool(ExtPoolParams),
    GroupPid.


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
    ?SERVER:open(Path, Params).

