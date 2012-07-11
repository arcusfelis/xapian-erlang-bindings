%% It is a pool of pools
-module(xapian_server_sup).
-behaviour(supervisor).

-export([start_link/0, start_server/2]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.
-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).


%% supervisor.
start_server(Path, Params) ->
    supervisor:start_child(?SUPERVISOR, [Path, Params]).

init([]) ->
    ChildSpec = {server,
                 {xapian_server, start_link, []},
                 transient, infinity, worker, [xapian_server]},
    {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.

