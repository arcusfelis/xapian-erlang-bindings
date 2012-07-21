-module(xapian_wdb_proper_tests).
-include_lib("xapian/include/xapian.hrl").
-include_lib("xapian/src/xapian.hrl").
-compile([export_all]).
-import(xapian_helper, [testdb_path/1]).

%% Used for testing, then can be moved to an another file
-define(SRV, xapian_server).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-behaviour(proper_statem).

%% Behaviour callbacks
-export([initial_state/0, command/1,
         precondition/2, postcondition/3, next_state/3]).

%% Helpers
-export([untag_ok/1]).


-record(state, {server, document_ids = []}).

initial_state() -> #state{}.


command(#state{server = undefined}) ->
    Path = testdb_path(wdb_proper),  
    Params = [write, create, overwrite],         
    {call, ?SRV, start_link, [Path, Params]};

command(#state{server = Srv, document_ids = []}) ->
    {call, ?SRV, add_document, [Srv, []]};

command(#state{server = Srv} = S) ->
    AddDoc  = {call, ?SRV, add_document, [Srv, []]},
    RemDoc  = {call, ?SRV, delete_document, [Srv, document_id(S)]},
    CloseDb = {call, ?SRV, close, [Srv]},
    Freqs = 
    [
        {100, AddDoc},
        {10,  RemDoc},
        {1,   CloseDb}
    ],
    frequency(Freqs).


document_id(#state{document_ids = Ids}) ->
    oneof(Ids).


next_state(S, V, {call, _, start_link, _}) ->
    Srv = {call, ?MODULE, untag_ok, [V]},
    S#state{server = Srv};

next_state(S, _V, {call, _, close, _}) ->
    S#state{server = undefined};

next_state(S, V, {call, _, add_document, _}) ->
    S#state{document_ids = [V | S#state.document_ids]};

next_state(S, _V, {call, _, delete_document, [_Srv, Id]}) ->
    S#state{document_ids = S#state.document_ids -- [Id]}.


precondition(_S, _C) ->
    true.


postcondition(S, {call, _, add_document, _}, R) ->
    ?SRV:is_document_exist(S#state.server, R);

postcondition(S, {call, _, delete_document, [_Srv, Id]}, _R) ->
    not ?SRV:is_document_exist(S#state.server, Id);

postcondition(_S, _C, _R) ->
    true.


prop_main() ->
    ?FORALL(Cmds, more_commands(200, commands(?MODULE)),
       ?TRAPEXIT(
           begin
           {History,State,Result} = run_commands(?MODULE, Cmds),
           catch ?SRV:close(State#state.server),
           ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                               [History, State, Result]),
                     aggregate(command_names(Cmds), Result =:= ok))
           end)).


proper_test_() ->
    EunitLeader = erlang:group_leader(),     
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{numtests, 10}]),
    erlang:group_leader(EunitLeader, self()),
    {timeout, 600, ?_assertEqual([], Res)}.


%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

untag_ok({ok, X}) -> X.
