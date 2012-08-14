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


-record(state, {path, server, document_ids = [], deleted_document_ids = []}).

initial_state() -> 
    #state{}.

command(#state{server = undefined, path = undefined}) ->
    Path = testdb_path("wdb_proper" ++ integer_to_list(random:uniform(100000))),
    Params = [write, create, overwrite],         
    {call, ?SRV, start_link, [Path, Params]};

command(#state{server = undefined, path = Path}) ->
    Params = [write],         
    {call, ?SRV, start_link, [Path, Params]};

command(#state{server = Server} = S) ->
    AddDoc    = {call, ?SRV, add_document, [Server, []]},
    CloseDb   = {call, ?SRV, close, [Server]},
    LastDocId = {call, ?SRV, last_document_id, [Server]},
    Freqs = commands_for_non_empty_db(S) ++
    [ {100, AddDoc}
    , {40,  LastDocId}
    , {20,  CloseDb}
    ],
    frequency(Freqs).


commands_for_non_empty_db(#state{document_ids = []}) ->
    [];
commands_for_non_empty_db(#state{server = Server} = S) ->
    RemDoc = {call, ?SRV, delete_document, [Server, document_id(S)]},
    [{50,  RemDoc}].


document_id(#state{document_ids = Ids = [_|_]}) ->
    oneof(Ids).


next_state(S, V, {call, _, start_link, [Path, _Params]}) ->
    Server = {call, ?MODULE, untag_ok, [V]},
    S#state{server = Server, path = Path};

next_state(S, _V, {call, _, close, _}) ->
    S#state{server = undefined};

next_state(S, V, {call, _, add_document, _}) ->
    S#state{document_ids = [V | S#state.document_ids]};

next_state(S, _V, {call, _, delete_document, [_Server, Id]}) ->
%   io:format(user, "~n DEL: ~p ~p~n", [S#state.document_ids, Id]),
    S#state{document_ids = delete(S#state.document_ids, Id),
            deleted_document_ids = [Id|S#state.deleted_document_ids] };

next_state(S, _V, _C) ->
    S.


delete([X|T], X) -> T;
delete([H|T], X) -> [H|delete(T, X)].


precondition(_S, _C) ->
    true.


postcondition(S, {call, _, add_document, _}, R) ->
    ?SRV:is_document_exist(S#state.server, R);

postcondition(S, {call, _, delete_document, [_Server, Id]}, _R) ->
    not ?SRV:is_document_exist(S#state.server, Id);

%% Prop: If DB is not empty, then:
%%      add_document(Server, Doc) =:= last_document_id();
%%      otherwise last_document_id() =:= undefined.
postcondition(S, {call, _, last_document_id, [_Server]}, R) ->
    #state{document_ids = Ids, deleted_document_ids = DelIds} = S,

    Expected = 
       case lists:reverse(lists:sort(Ids ++ DelIds)) of
            [H|_]  -> H;
            []     -> undefined
        end,
%   io:format(user, "~n ~p ~p~n", [Ids, R]),
    Expected =:= R;

postcondition(_S, _C, _R) ->
    true.


prop_main() ->
    %% Run at least 200 commands
    ?FORALL(Cmds, more_commands(200, commands(?MODULE)),
       ?TRAPEXIT(
            begin
            {History,State,Result} = run_commands(?MODULE, Cmds),
            catch ?SRV:close(State#state.server),
            [remove_directory_with_files(DirName) 
                || DirName <- [State#state.path], 
                   DirName =/= undefined, filelib:is_dir(DirName)],
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                               [History, State, Result]),
                     aggregate(command_names(Cmds), Result =:= ok))
            end)).


%% Don't work with sub-folders.
remove_directory_with_files(DirName) ->
    SubFileNamesPattern = filename:join(DirName, "*"),
    SubFileNames = filelib:wildcard(SubFileNamesPattern),
    [ok = file:delete(FileName) || FileName <- SubFileNames],
    ok = file:del_dir(DirName).


proper_test_() ->
    EunitLeader = erlang:group_leader(),     
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{numtests, 10}]),
    erlang:group_leader(EunitLeader, self()),
    {timeout, 600, ?_assertEqual([], Res)}.


%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

untag_ok({ok, X})   -> X;
untag_ok(undefined) -> undefined.
