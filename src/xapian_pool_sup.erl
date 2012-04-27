%% It is a pool of pools
-module(xapian_pool_sup).
-behaviour(supervisor).

-export([start_link/0, start_pool/1]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.
-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).


%% supervisor.
start_pool(Args) ->
    supervisor:start_child(?SUPERVISOR, [Args]).

init([]) ->
    ChildSpec = {pool,
                 {poolboy, start_link, []},
                 transient, infinity, worker, [poolboy]},
    {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.

