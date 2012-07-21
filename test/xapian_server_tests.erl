%% This module is a `gen_server' that handles a single port connection.
-module(xapian_server_tests).
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

-record(document, {docid}).
    

%% ------------------------------------------------------------------
%% Call C++ tests
%% ------------------------------------------------------------------
    
%% @doc Check basic memory operations (malloc, free).
memory_test() ->
    {ok, Server} = ?SRV:start_link([], []),
    ?SRV:internal_test_run(Server, memory, []),
    ?SRV:close(Server),
    ok.
    

echo_test() ->
    {ok, Server} = ?SRV:start_link([], []),
    ?assertEqual(?SRV:internal_test_run(Server, echo, <<0,5>>), <<0,5>>),
    Bin = list_to_binary(lists:duplicate(1100, 1)),
    ?assertEqual(?SRV:internal_test_run(Server, echo, Bin), Bin),
    ok.


-define(DOCUMENT_ID(X), X:32/native-unsigned-integer).
    
%% @doc This test checks the work of `ResultEncoder'.
result_encoder_test() ->
    {ok, Server} = ?SRV:start_link([], []),
    Reply = ?SRV:internal_test_run(Server, result_encoder, [1, 1000]),
    Reply = ?SRV:internal_test_run(Server, result_encoder, [1, 1000]),
    Reply = ?SRV:internal_test_run(Server, result_encoder, [1, 1000]),
    ?SRV:close(Server),
    ?assertEqual(lists:seq(1, 1000), [ Id || <<?DOCUMENT_ID(Id)>> <= Reply ]),
    ok.
    

%% @doc Check an exception.
exception_test() ->
    {ok, Server} = ?SRV:start_link([], []),
    % ?assertException(ClassPattern, TermPattern, Expr)
    ?assertException(error, 
        #x_error{type = <<"MemoryAllocationDriverError">>}, 
        ?SRV:internal_test_run(Server, exception, [])),
    ?SRV:close(Server),
    ok.


%% ------------------------------------------------------------------
%% Call test generators
%% ------------------------------------------------------------------


wrapper(Name) ->
    [{setup, 
      fun() -> ok end, 
      fun(_) -> [{atom_to_list(Name), ?MODULE:Name()}] end}].


run_test_generators_once_test_() ->
    AllFuns = ?MODULE:module_info(exports),
    [wrapper(Name) || {Name, Arity} <- AllFuns, 
                       Arity =:= 0, 
                       lists:suffix("_gen", atom_to_list(Name))].


%% This test tries to create a document with all kinds of fields.
simple_gen() ->
    % Open test
    Path = testdb_path(simple),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = slot1}, 
        #x_prefix_name{name = author, prefix = <<$A>>}],
    Document =
        [ #x_stemmer{language = <<"english">>}
        , #x_data{value = "My test data as iolist"} 
        %% It is a term without a position.
        , #x_term{value = "Simple"} 
        %% Posting (a term with a position).
        , #x_term{value = "term", position=1} 
        , #x_value{slot = 0, value = "Slot #0"} 
        , #x_value{slot = slot1, value = "Slot #1"} 
        , #x_text{value = "Paragraph 1"} 
        , #x_delta{}
        , #x_text{value = <<"Paragraph 2">>} 
        , #x_text{value = <<"Michael">>, prefix = author} 
        ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId = ?SRV:add_document(Server, Document),
        DocIdReplaced1 = ?SRV:replace_or_create_document(Server, DocId, Document),
        DocIdReplaced2 = ?SRV:replace_or_create_document(Server, "Simple", Document),

        ?SRV:delete_document(Server, DocId),
        ?SRV:delete_document(Server, "Simple"),

        [ ?_assert(is_integer(DocId))
        , ?_assertEqual(DocId, DocIdReplaced1)
        , ?_assertEqual(DocId, DocIdReplaced2)
        ]
    after
        ?SRV:close(Server)
    end.


last_document_id_gen() ->
    % Open test
    Path = testdb_path(last_docid),
    Params = [write, create, overwrite],
    Document = [],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% The DB is empty.
        NoId = ?SRV:last_document_id(Server),

        %% Add 1 document, check a last id.
        DocId = ?SRV:add_document(Server, Document),
        Last = ?SRV:last_document_id(Server),

        [ {"Db is empty.", ?_assertEqual(undefined, NoId)}
        , ?_assertEqual(DocId, Last)
        ]
    after
        ?SRV:close(Server)
    end.


open_and_register_local_name_test() ->
    Name = xapian_server_test_local_name,
    %% Register the empty server under the local name
    {ok, Server} = ?SRV:start_link([], [{name, Name}]),
    ?assertEqual(whereis(Name), Server), 
    ?SRV:close(Server),
    ?assertNot(is_process_alive(Server)).


open_and_register_local_name2_test() ->
    Name = xapian_server_test_local_name2,
    %% Register the empty server under the local name
    {ok, Server} = ?SRV:start_link([], [{name, {local, Name}}]),
    ?assertEqual(whereis(Name), Server), 
    ?SRV:close(Server).


open_and_register_global_name_test() ->
    Name = xapian_server_test_global_name,
    %% Register the empty server under the global name
    {ok, Server} = ?SRV:start_link([], [{name, {global, Name}}]),
    ?assertEqual(global:whereis_name(Name), Server), 
    ?SRV:close(Server).


update_document_test() ->
    Path = testdb_path(update_document),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId = ?SRV:add_document(Server, []),

        %% The document with DocId will be extended.
        DocId1 = ?SRV:update_document(Server, DocId, [#x_term{value = "more"}]),
        ?assertEqual(DocId, DocId1),

        %% Cannot add this term again, because the action is `add'.
        ?assertError(#x_error{type  = <<"BadArgumentDriverError">>}, 
            ?SRV:update_document(Server, DocId, 
                [#x_term{action = add, value = "more", ignore = false}])),

        %% Add an another term...
        ?SRV:update_document(Server, DocId, 
            [#x_term{action = add, value = "other", ignore = false}]),
        ?assert(?SRV:is_document_exist(Server, "other")),

        %% ... and delete it.
        ?SRV:update_document(Server, DocId, 
            [#x_term{action = remove, value = "other", ignore = false}]),
        ?assertNot(?SRV:is_document_exist(Server, "other")),

        %% Cannot find a document, using "bad_term" as UID.
        ?debugMsg("UPD_DOC_BAD_ID_MARK"),
        ?assertError(#x_error{type  = <<"BadArgumentDriverError">>}, 
                     ?SRV:update_document(Server, "bad_term", [])),

        %% One document with the term "more" was found.
        %% Because we use a term as a key, few documents can be matched.
        %% That is why, undefined is returned (and not a document id).
        %% ignore = true catches errors.
        ?assertEqual(undefined, 
            ?SRV:update_or_create_document(Server, "more", 
                [#x_term{action = add, value = "more", ignore = true}])),

        %% Cannot update the document that is not found.
        ?assertNot(?SRV:is_document_exist(Server, "fail")),
        ?assertError(#x_error{type  = <<"BadArgumentDriverError">>}, 
            ?SRV:update_document(Server, "fail", [])),

        %% Now we can.
        ?assertNot(?SRV:is_document_exist(Server, "fail")),
        DocId2 = ?SRV:update_or_create_document(Server, "fail", []),
        %% Document was created, but it us empty.
        ?assert(?SRV:is_document_exist(Server, DocId2)),
        ?assertNot(?SRV:is_document_exist(Server, "fail")),

        %% Try the same using the document id as a key.
        DocId3 = ?SRV:update_or_create_document(Server, DocId2, []),
        ?assertEqual(DocId2, DocId3)

    after
        ?SRV:close(Server)
    end.

    
%% REP_CRT_DOC_MARK
replace_or_create_document_test() ->
    Path = testdb_path(replace_or_create_document),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% Try update using non-existed DocId.
        ?assertNot(?SRV:is_document_exist(Server, 1)),
        DocId = ?SRV:replace_or_create_document(Server, "bad_term", []),
        ?assertEqual(DocId, 1),
        ?assert(?SRV:is_document_exist(Server, 1)),
        ?SRV:delete_document(Server, DocId),
        ?assertNot(?SRV:is_document_exist(Server, 1)),

        %% If there is no document, then the new one will be created.
        DocId0 = ?SRV:replace_or_create_document(Server, "bad_term", []),
        %% Even when the first document is deleted, the new document will have
        %% another document id.
        ?assertEqual(DocId0, 2),
        ?assertNot(?SRV:is_document_exist(Server, "bad_term")),
        ?assert(?SRV:is_document_exist(Server, DocId0)),

        %% Create a new document.
        DocId1 = ?SRV:add_document(Server, [#x_term{value = "good_term"}]),
        ?assert(?SRV:is_document_exist(Server, "good_term")),

        %% Replace the whole document with the new one.
        DocId2 = ?SRV:replace_or_create_document(Server, "good_term", 
                                       [#x_term{value = "nice_term"}]),
        %% It returns a document id of replaced document (but it can be more 
        %% then once).
        ?assertEqual(DocId1, DocId2),

        %% The old document was deleted,
        ?assertNot(?SRV:is_document_exist(Server, "good_term")),
        %% the new document was created.
        ?assert(?SRV:is_document_exist(Server, "nice_term")),

        %% Test few documents with the same term.
        %%
        %% Add another document with the same term.
        DocId3 = ?SRV:add_document(Server, [#x_term{value = "nice_term"}]),

        %% Only one document will left after replace_or_create_document.
        %% DocId2 and DocId3 are still here.
        ?assert(?SRV:is_document_exist(Server, DocId2)),
        ?assert(?SRV:is_document_exist(Server, DocId3)),
        DocId4 = ?SRV:replace_or_create_document(Server, "nice_term", 
                                       [#x_term{value = "mass_term"}]),
        %% Only document with DocId2 is here, other document with the same term
        %% was deleted.
        ?assertEqual(DocId4, DocId2),
        Ids = all_record_ids(Server, "mass_term"),
        ?assertEqual(Ids, [DocId4]),
        ?assertNot(?SRV:is_document_exist(Server, DocId3))

    after
        ?SRV:close(Server)
    end.


delete_document_gen() ->
    Path = testdb_path(delete_document),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% Documents are not exist.
        Exists1 = ?SRV:delete_document(Server, "test"),
        Exists2 = ?SRV:delete_document(Server, 1),
        DocId1 = ?SRV:add_document(Server, [#x_term{value = "term"}]),
        DocId2 = ?SRV:add_document(Server, []),
        Exists3 = ?SRV:delete_document(Server, "term"),
        Exists4 = ?SRV:delete_document(Server, DocId2),
        Exists5 = ?SRV:is_document_exist(Server, DocId1),
        Exists6 = ?SRV:is_document_exist(Server, DocId2),
        [ ?_assertNot(Exists1)
        , ?_assertNot(Exists2)
        , ?_assert(Exists3)
        , ?_assert(Exists4)
        , ?_assertNot(Exists5)
        , ?_assertNot(Exists6)
        ]
    after
        ?SRV:close(Server)
    end.


%% REP_DOC_MARK
replace_document_test() ->
    Path = testdb_path(replace_document),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% Try update using non-existed DocId.
        ?assertNot(?SRV:is_document_exist(Server, 1)),
        DocId = ?SRV:replace_document(Server, "bad_term", []),
        %% Nothing was updated.
        ?assertEqual(DocId, undefined),
        ?assertNot(?SRV:is_document_exist(Server, 1)),

        %% If there is no document, then there is no an error.
        DocId0 = ?SRV:replace_document(Server, "bad_term", []),
        ?assertEqual(DocId0, undefined),
        %% Nothing was created.
        ?assertNot(?SRV:is_document_exist(Server, "bad_term")),

        %% Create a new document.
        DocId1 = ?SRV:add_document(Server, [#x_term{value = "good_term"}]),
        ?assert(?SRV:is_document_exist(Server, "good_term")),
        ?assert(?SRV:is_document_exist(Server, DocId1)),
        ?assertEqual(DocId1, 1),

        %% Replace the whole document with the new one.
        DocId2 = ?SRV:replace_document(Server, "good_term", 
                                       [#x_term{value = "nice_term"}]),
        %% It returns a document id of replaced document (but it can be more 
        %% then once).
        ?assertEqual(DocId1, DocId2),

        %% The old document was deleted,
        ?assertNot(?SRV:is_document_exist(Server, "good_term")),
        %% the new document was created.
        ?assert(?SRV:is_document_exist(Server, "nice_term")),

        %% Test few documents with the same term.
        %%
        %% Add another document with the same term.
        DocId3 = ?SRV:add_document(Server, [#x_term{value = "nice_term"}]),

        %% Only one document will left after replace_document.
        %% DocId2 and DocId3 are still here.
        ?assert(?SRV:is_document_exist(Server, DocId2)),
        ?assert(?SRV:is_document_exist(Server, DocId3)),
        DocId4 = ?SRV:replace_document(Server, "nice_term", 
                                       [#x_term{value = "mass_term"}]),
        %% Only document with DocId2 is here, other document with the same term
        %% was deleted.
        ?assertEqual(DocId4, DocId2),
        Ids = all_record_ids(Server, "mass_term"),
        ?assertEqual(Ids, [DocId4]),
        ?assertNot(?SRV:is_document_exist(Server, DocId3))

    after
        ?SRV:close(Server)
    end.


%% REP_DOC_MARK
replace_document_by_id_test() ->
    Path = testdb_path(replace_document_by_id),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% Create a new document.
        DocId1 = ?SRV:add_document(Server, [#x_term{value = "new"}]),
        DocId2 = ?SRV:replace_document(Server, DocId1, 
                                       [#x_term{value = "other"}]),
        ?assertEqual(DocId1, DocId2),
        Ids = all_record_ids(Server, "other"),
        ?assertEqual(Ids, [DocId2])
    after
        ?SRV:close(Server)
    end.


is_document_exists_gen() ->
    Path = testdb_path(is_document_exists),
    Params = [write, create, overwrite],
    Doc = 
    [ #x_term{value = "monad"}
    ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        BeforeAddTerm = ?SRV:is_document_exist(Server, "monad"),
        BeforeAddId   = ?SRV:is_document_exist(Server, 1),
        ?SRV:add_document(Server, Doc),
        AfterAddTerm = ?SRV:is_document_exist(Server, "monad"),
        AfterAddId   = ?SRV:is_document_exist(Server, 1),
        [ ?_assertNot(BeforeAddTerm)
        , ?_assertNot(BeforeAddId)
        , ?_assert(AfterAddTerm)
        , ?_assert(AfterAddId)
        ]
    after
        ?SRV:close(Server)
    end.



frequency_test() ->
    Path = testdb_path(frequency),
    Params = [write, create, overwrite],
    Doc = 
    [ #x_term{value = "term", frequency = {cur, 1}}
    , #x_term{value = "term", frequency = {abs, 5}}
    , #x_term{value = "term", frequency = {cur, -1}}
    ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        ?SRV:add_document(Server, Doc)
    after
        ?SRV:close(Server)
    end.


term_actions_test() ->
    Path = testdb_path(actions),
    Params = [write, create, overwrite],
    Doc = 
    [ #x_term{action = add,     value = "term"}
    , #x_term{action = update,  value = "term"}
    , #x_term{action = set,     value = "term"}
    ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        ?SRV:add_document(Server, Doc)
    after
        ?SRV:close(Server)
    end.


-record(term, {value, wdf}).
-record(term_ext, {value, positions, position_count, freq, wdf}).
-record(term_pos, {value, positions, position_count}).
-record(short_term, {wdf}).
-record(spy_term, {value, freq}).


term_qlc_gen() ->
    Path = testdb_path(term_qlc),
    Params = [write, create, overwrite],

    %% Create a document with terms
    TermNames = 
    [erlang:list_to_binary(erlang:integer_to_list(X)) 
        || X <- lists:seq(1, 100)],

    Fields = [#x_term{value = Term} || Term <- TermNames], 

    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId = ?SRV:add_document(Server, Fields),
        Meta = xapian_term_record:record(term, record_info(fields, term)),
        Table = xapian_term_qlc:document_term_table(Server, DocId, Meta),

        Records = qlc:e(qlc:q([X || X <- Table])),
        Values = [Value || #term{value = Value} <- Records],
        Not1Wdf = [X || X = #term{wdf = Wdf} <- Records, Wdf =/= 1],

        %% Lookup order test.
        %% It is an important test.
        %% Actually, it tests the fact, that skip_to("") move an TermIterator 
        %% in the beginning of the document.
        OrderTestQuery = qlc:q([Value || #term{value = Value} <- Table, 
            Value =:= "2" orelse Value =:= "1" orelse Value =:= "3"]),
        OrderTestValues = qlc:e(OrderTestQuery),
        [ ?_assertEqual(Values, lists:sort(TermNames))
        , ?_assertEqual(Not1Wdf, [])
        , ?_assertEqual(OrderTestValues, [<<"1">>, <<"2">>, <<"3">>])
        ]
    after
        ?SRV:close(Server)
    end.


short_term_qlc_gen() ->
    Path = testdb_path(short_term_qlc),
    Params = [write, create, overwrite],
    %% Create a document with terms
    TermNames = 
    [erlang:list_to_binary(erlang:integer_to_list(X)) 
        || X <- lists:seq(1, 100)],

    Fields = [#x_term{value = Term} || Term <- TermNames], 
    
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId = ?SRV:add_document(Server, Fields),
        Meta = xapian_term_record:record(short_term, 
            record_info(fields, short_term)),
        Table = xapian_term_qlc:document_term_table(Server, DocId, Meta),
        Q = qlc:q([Wdf || #short_term{wdf = Wdf} <- Table]),
        WdfSum = qlc:fold(fun erlang:'+'/2, 0, Q),
        [ ?_assertEqual(WdfSum, 100) ]
    after
        ?SRV:close(Server)
    end.


term_ext_qlc_gen() ->
    Path = testdb_path(term_ext_qlc),
    Params = [write, create, overwrite],
    
    %% Create a document with terms
    TermNames = 
    [erlang:list_to_binary(erlang:integer_to_list(X)) 
        || X <- lists:seq(1, 100)],

    Fields = [#x_term{value = Term} || Term <- TermNames], 

    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId = ?SRV:add_document(Server, Fields),
        Meta = xapian_term_record:record(term_ext, 
                    record_info(fields, term_ext)),
        Table = xapian_term_qlc:document_term_table(Server, DocId, Meta),
        Records = qlc:e(qlc:q([X || X <- Table])),

        Not0Pos = 
        [X || X = #term_ext{position_count = Count} <- Records, Count =/= 0],

        NotEmptyPos = 
        [X || X = #term_ext{positions = Poss} <- Records, Poss =/= []],


        %% Shared table: changes in 1 query handler don't modify the second one.
        %%
        %% SUDDENLY! PAIN!
        %% In the next string the error can occur.
        %% http://trac.xapian.org/ticket/423
        QH1 = qlc:q([V || #term_ext{value=V} <- Table]),
        QH2 = qlc:q([V || #term_ext{value=V} <- Table]),
        C1  = qlc:cursor(QH1),
        C2  = qlc:cursor(QH2),
        C1E1 = qlc:next_answers(C1, 1),
        C2E1 = qlc:next_answers(C2, 1),
        C1E2 = qlc:next_answers(C1, 1),
        C1E3 = qlc:next_answers(C1, 1),
        C2E2 = qlc:next_answers(C2, 1),
        C2E3 = qlc:next_answers(C2, 1),
        C2E4 = qlc:next_answers(C2, 1),
        C1E4 = qlc:next_answers(C1, 1),
        C2E5 = qlc:next_answers(C2, 25),
        C1E5 = qlc:next_answers(C1, 25),
        C1E6 = qlc:next_answers(C1, 25),
        C2E6 = qlc:next_answers(C2, 25),

        [ ?_assertEqual(Not0Pos, [])
        , ?_assertEqual(NotEmptyPos, [])
        , {"Shared term QLC table.",
            [ ?_assertEqual(C1E1, C2E1)
            , ?_assertEqual(C1E2, C2E2)
            , ?_assertEqual(C1E3, C2E3)
            , ?_assertEqual(C1E4, C2E4)
            , ?_assertEqual(C1E5, C2E5)
            , ?_assertEqual(C1E6, C2E6)
            ]}
        ]
    after
        ?SRV:close(Server)
    end.


term_numbers(From, To) ->
    [erlang:list_to_binary(erlang:integer_to_list(X)) 
        || X <- lists:seq(From, To)].


terms(TermNames) ->
    [#x_term{value = Term} || Term <- TermNames].


term_qlc_join_gen() ->
    Path = testdb_path(term_qlc_join),
    Params = [write, create, overwrite],
    %% Create a document with terms
    TermNames0to99    = term_numbers(0, 99),
    TermNames100to199 = term_numbers(100, 199),
    TermNames200to299 = term_numbers(200, 299),
    TermNames300to399 = term_numbers(300, 399),

    
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        Doc1Terms = TermNames0to99 ++ TermNames100to199,
        Doc2Terms = TermNames100to199 ++ TermNames200to299,
        Doc3Terms = TermNames200to299 ++ TermNames300to399,
        Doc1Id = ?SRV:add_document(Server, terms(Doc1Terms)),
        Doc2Id = ?SRV:add_document(Server, terms(Doc2Terms)),
        Doc3Id = ?SRV:add_document(Server, terms(Doc3Terms)),
        Meta = xapian_term_record:record(term, record_info(fields, term)),
        Table1 = xapian_term_qlc:document_term_table(Server, Doc1Id, Meta),
        Table2 = xapian_term_qlc:document_term_table(Server, Doc2Id, Meta),
        Table3 = xapian_term_qlc:document_term_table(Server, Doc3Id, Meta),
        %% Search terms from 2 documents with the same names.
        %% It is a natural join.
        Q12 = qlc:q([Value1 || #term{value = Value1} <- Table1, 
                               #term{value = Value2} <- Table2,
                               Value1 =:= Value2]),
        Q23 = qlc:q([Value1 || #term{value = Value1} <- Table2, 
                               #term{value = Value2} <- Table3,
                               Value1 =:= Value2]),
        Q1223 = qlc:append(Q12, Q23),
        QE12 = qlc:e(Q12),
        QE23 = qlc:e(Q23),
        QE1223 = qlc:e(Q1223),
        {"Natural join of the terms from two document.",
            [ ?_assertEqual(QE12, TermNames100to199)  
            , ?_assertEqual(QE23, TermNames200to299) 
            , ?_assertEqual(QE1223, TermNames100to199 ++ TermNames200to299) 
            ]}
    after
        ?SRV:close(Server)
    end.


term_pos_qlc_gen() ->
    Path = testdb_path(term_pos_qlc),
    Params = [write, create, overwrite],

    Fields = 
    [ #x_term{value = "term1", position = [1,2,3]}
    , #x_term{value = "term2", position = [3,2,1]}
    , #x_term{value = "term3", position = [1]}
    , #x_term{value = "term3", position = [2,3]}
    ], 
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId = ?SRV:add_document(Server, Fields),
        Meta = xapian_term_record:record(term_pos, 
                record_info(fields, term_pos)),
        Table = xapian_term_qlc:document_term_table(Server, DocId, Meta),

        Term1Records = 
        qlc:e(qlc:q([X || X = #term_pos{value = <<"term1">>} <- Table])),
        Term2Records = 
        qlc:e(qlc:q([X || X = #term_pos{value = <<"term2">>} <- Table])),
        Term3Records = 
        qlc:e(qlc:q([X || X = #term_pos{value = <<"term3">>} <- Table])),
        AllRecords = 
        qlc:e(qlc:q([X || X <- Table])),
        Term1 = #term_pos{
            value = <<"term1">>, position_count = 3, positions = [1,2,3]},
        Term2 = #term_pos{
            value = <<"term2">>, position_count = 3, positions = [1,2,3]},
        Term3 = #term_pos{
            value = <<"term3">>, position_count = 3, positions = [1,2,3]},

        [ ?_assertEqual([Term1], Term1Records)
        , ?_assertEqual([Term2], Term2Records)
        , ?_assertEqual([Term3], Term3Records)
        , ?_assertEqual(erlang:length(AllRecords), 3)
        ]
    after
        ?SRV:close(Server)
    end.



value_count_match_spy_gen() ->
    Path = testdb_path(value_count_mspy),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = color}],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% There are 2 "green" documents.
        Colors = ["Red", "Blue", "green", "white", "black", "green"],
        [add_color_document(Server, Color) || Color <- Colors],

        %% Call with a slot name
        SpySlot1 = xapian_match_spy:value_count(Server, color),
        %% Call with a slot number
        xapian_match_spy:value_count(Server, 1),

        Query = "",
        EnquireResourceId = ?SRV:enquire(Server, Query),
        MSetParams = #x_match_set{
            enquire = EnquireResourceId, 
            spies = [SpySlot1]},
%       MSetResourceId = 
        ?SRV:match_set(Server, MSetParams),
        Meta = xapian_term_record:record(spy_term, 
                    record_info(fields, spy_term)),

        %% These elements sorted by value.
        Table = xapian_term_qlc:value_count_match_spy_table(
            Server, SpySlot1, Meta),

        %% These elements sorted by freq.
        TopTable = xapian_term_qlc:top_value_count_match_spy_table(
            Server, SpySlot1, 100, Meta),

        Values = qlc:e(qlc:q([Value || #spy_term{value = Value} <- Table])),

        %% "Red" was converted to <<"Red">> because of lookup function call.
        %% Erlang did not match it, but Xapian did.
        RedValues = qlc:e(qlc:q([Value 
            || #spy_term{value = Value} <- Table, Value =:= "Red"])),

        OrderValues = qlc:e(qlc:q([Value || #spy_term{value = Value} <- Table, 
            Value =:= "white" orelse Value =:= "black"])),

        TopAlphOrderValues = 
        qlc:e(qlc:q([Value || #spy_term{value = Value} <- TopTable, 
            Value =:= "white" orelse Value =:= "black"])),

        TopFreqOrderValues = 
        qlc:e(qlc:q([Value || #spy_term{value = Value} <- TopTable, 
            Value =:= "white" orelse Value =:= "green"])),

        [ ?_assertEqual(Values, 
            [<<"Blue">>, <<"Red">>, <<"black">>, <<"green">>, <<"white">>])
        , ?_assertEqual(RedValues, [<<"Red">>])
        , {"Check order", 
            [ ?_assertEqual(OrderValues,        [<<"black">>, <<"white">>])
            , ?_assertEqual(TopAlphOrderValues, [<<"black">>, <<"white">>])
            , ?_assertEqual(TopFreqOrderValues, [<<"green">>, <<"white">>])
            ]}
        , ?_assertEqual(RedValues, [<<"Red">>])
        ]
    after
        ?SRV:close(Server)
    end.
    
      
value_spy_to_type_or_slot_test() ->
    Path = testdb_path(value_spy_to_type_or_slot),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = color, type = float}],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% Call with a slot name
        MatchSpy = xapian_match_spy:value_count(Server, color),
        ?assertEqual(?SRV:value_spy_to_slot(Server, MatchSpy), 1),
        ?assertEqual(?SRV:value_spy_to_type(Server, MatchSpy), float)

    after
        ?SRV:close(Server)
    end.
    
      

add_color_document(Server, Color) ->
    Document = [ #x_value{slot = color, value = Color} ],
    ?SRV:add_document(Server, Document).


float_value_count_match_spy_gen() ->
    Path = testdb_path(value_count_mspy),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = page_count, type = float}],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        Doc1 = [ #x_value{slot = page_count, value = 10} ],
        Doc2 = [ #x_value{slot = page_count, value = 100} ],
        Doc3 = [ #x_value{slot = page_count, value = 200} ],
        Doc4 = [ #x_value{slot = page_count, value = 20} ],
        Docs = [ Doc1, Doc2, Doc3, Doc4 ],

%       DocIds = 
        [ ?SRV:add_document(Server, Doc) || Doc <- Docs ],

        %% Call with a slot name
        SpySlot1 = xapian_match_spy:value_count(Server, page_count),

        Query = "",
        EnquireResourceId = ?SRV:enquire(Server, Query),
        MSetParams = #x_match_set{
            enquire = EnquireResourceId, 
            spies = [SpySlot1]},
        %% Collect statistic 
%       MSetResourceId = 
        ?SRV:match_set(Server, MSetParams),
        Meta = xapian_term_record:record(spy_term, 
                    record_info(fields, spy_term)),

        %% These elements sorted by value.
        Table = xapian_term_qlc:value_count_match_spy_table(
            Server, SpySlot1, Meta),

        Values =
        qlc:e(qlc:q([Value || #spy_term{value = Value} <- Table])),
        FilteredValues = 
        qlc:e(qlc:q([Value || #spy_term{value = Value} <- Table, Value =:= 10])),
        JoinValues = 
        qlc:e(qlc:q([V1 || #spy_term{value = V1} <- Table, 
                           #spy_term{value = V2} <- Table, V1 =:= V2])),

        [ {"Float values inside MatchSpy.",
           ?_assertEqual(Values, [10.0, 20.0, 100.0, 200.0])}
        , {"Join float values.",
           ?_assertEqual(JoinValues, [10.0, 20.0, 100.0, 200.0])}
        , {"Lookup float values.",
           ?_assertEqual(FilteredValues, [10.0])}
        ]
    after
        ?SRV:close(Server)
    end.


%% Terms can be deleted, added or replaced using `#x_term{}'.
term_advanced_actions_gen() ->
    Path = testdb_path(adv_actions),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId = ?SRV:add_document(Server, []),
        U = fun(Doc) ->
            ?SRV:update_document(Server, DocId, Doc)
            end,
        
        Meta = xapian_term_record:record(term, record_info(fields, term)),
        FindTermFn = 
        fun(Value) ->
            DocRes = xapian_server:document(Server, DocId),
            Table = xapian_term_qlc:document_term_table(
                Server, DocRes, Meta, [ignore_empty]),
            ?SRV:release_resource(Server, DocRes),
            qlc:e(qlc:q([X || X = #term{value = V} <- Table, V =:= Value]))
            end,

        FF = fun() -> FindTermFn("term") end,
        UU = fun(Field) -> U([Field]), FF() end,

        Term             = #x_term{value = "term"},
        TermAdd          = Term#x_term{action = add}, 
        TermAddNotIgnore = Term#x_term{action = add, ignore = false}, 
        TermUpdate       = Term#x_term{action = update}, 
        TermUpdateNotIgnore = TermUpdate#x_term{ignore = false}, 
        TermSet          = Term#x_term{action = set}, 
        TermDec          = TermSet#x_term{frequency = -1}, 
        TermSetAbs       = TermSet#x_term{frequency = {abs, 10}}, 
        TermRemoveIgnore = Term#x_term{action = remove, frequency = 0},
        TermRemove       = TermRemoveIgnore#x_term{ignore = false},
        TermRemove2      = TermRemove#x_term{frequency = 123},

        Terms1 = FF(),
        Terms2 = UU(TermAddNotIgnore),

        %% Error will be thrown. Value was not changed.
        ?assertError(#x_error{type = <<"BadArgumentDriverError">>}, 
            UU(TermAddNotIgnore)),

        Terms3 = FF(),

        %% Error will be ignored. Value was not changed.
        Terms4 = UU(TermAdd),

        %% Start changing of WDF
        Terms5 = UU(TermUpdate),
        Terms6 = UU(TermSet),
        Terms7 = UU(TermDec),
        Terms8 = UU(TermSetAbs),

        %% Cannot remove term, because WDF is not matched.
        ?assertError(#x_error{type = <<"BadArgumentDriverError">>}, 
            UU(TermRemove2)),
        Terms9 = FF(),

        %% Delete the term
        Terms10 = UU(TermRemove),

        %% Cannot delete the term twoce
        ?assertError(#x_error{type = <<"InvalidArgumentError">>}, 
            UU(TermRemove)),
        Terms11 = FF(),


        %% Cannot update a non-existing term 
        ?assertError(#x_error{type = <<"BadArgumentDriverError">>}, 
            UU(TermUpdateNotIgnore)),
        Terms12 = FF(),

        %% It will be ignored.
        Terms13 = UU(TermUpdate),

        NormTerm1 = #term{value = <<"term">>, wdf = 1},
        NormTerm2 = #term{value = <<"term">>, wdf = 2},
        NormTerm3 = #term{value = <<"term">>, wdf = 3},
        NormTerm4 = #term{value = <<"term">>, wdf = 10},

        [ ?_assertEqual(Terms1, [])
        , ?_assertEqual(Terms2, [NormTerm1])
        , ?_assertEqual(Terms3, [NormTerm1])
        , ?_assertEqual(Terms4, [NormTerm1])
        , ?_assertEqual(Terms5, [NormTerm2])
        , ?_assertEqual(Terms6, [NormTerm3])
        , ?_assertEqual(Terms7, [NormTerm2])
        , ?_assertEqual(Terms8, [NormTerm4])
        , ?_assertEqual(Terms9, [NormTerm4])
        , ?_assertEqual(Terms10, [])
        , ?_assertEqual(Terms11, [])
        , ?_assertEqual(Terms12, [])
        , ?_assertEqual(Terms13, [])
        ]
    after
        ?SRV:close(Server)
    end.
    


reopen_test() ->
    % Open test
    Path = testdb_path(reopen),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    ?SRV:close(Server),

    {ok, ReadOnlyServer} = ?SRV:start_link(Path, []),
    ?SRV:close(ReadOnlyServer).


-record(stemmer_test_record, {docid, data}).

stemmer_test() ->
    % Open test with the default stemmer
    Path = testdb_path(stemmer),
    Params = [write, create, overwrite, 
        #x_stemmer{language = <<"english">>},
        #x_prefix_name{name = author, prefix = <<$A>>, is_boolean=true}],
    Document =
        [ #x_data{value = "My test data as iolist (NOT INDEXED)"} 
        , #x_text{value = "Return a list of available languages."} 
        , #x_text{value = "And filter it."} 
        , #x_delta{position=300}
        , #x_text{value = "And other string is here."} 
        , #x_text{value = <<"Michael">>, prefix = author} 
        ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% Test a term generator
        DocId = ?SRV:add_document(Server, Document),
        ?assert(is_integer(DocId)),

        %% Test a query parser
        Offset = 0,
        PageSize = 10,
        Meta = xapian_record:record(stemmer_test_record, 
            record_info(fields, stemmer_test_record)),

        Q0 = #x_query_string{parser=standard, value="return"},
        Q1 = #x_query_string{value="return AND list"},
        Q2 = #x_query_string{value="author:michael"},
        Q3 = #x_query_string{value="author:olly list"},
        Q4 = #x_query_string{value="author:Michael"},
        Q5 = #x_query_string{value="retur*", features=[default, wildcard]},
        Q6 = #x_query_string{value="other AND Return"},
        Q7 = #x_query_string{value="list NEAR here"},
        Q8 = #x_query_string{value="list NEAR filter"},

%%      {x_query_string,{x_query_parser,default,{x_stemmer,da},none,0,'AND',[]},
%%                       "trinitrotoluol",<<>>,undefined}
        Q9Stem = #x_stemmer{language=da},
        Q9Parser = #x_query_parser{stemmer=Q9Stem,
                                   stemming_strategy=none, 
                                   max_wildcard_expansion=0,
                                   default_op='AND'},
        Q9 = #x_query_string{value="return", parser=Q9Parser},

        F = fun(Query) ->
            RecList = ?SRV:query_page(Server, Offset, PageSize, Query, Meta),
            io:format(user, "~n~p~n", [RecList]),
            RecList
            end,
        Qs =
        [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9],
        [R0, R1, R2, R3, R4, R5, R6, R7, R8, R9] = 
        lists:map(F, Qs),
        ?assert(is_list(R0) andalso length(R0) =:= 1),
        ?assertEqual(R1, R0),
        ?assertEqual(R2, R0),
        ?assertEqual(R3, []),
        ?assertEqual(R4, []),
        ?assertEqual(R5, []),
        ?assertEqual(R6, R0),
        ?assertEqual(R7, []),
        ?assertEqual(R8, R0),
        ?assertEqual(R9, R0)
    after
        ?SRV:close(Server)
    end.


query_parser_test() ->
    Path = testdb_path(parser),
    Params = [write, create, overwrite],
    Document =
        [ #x_data{value = "My test data as iolist (NOT INDEXED)"} 
        , #x_text{value = "The quick brown fox jumps over the lazy dog."} 
        ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        %% Test a term generator
        DocId = ?SRV:add_document(Server, Document),
        ?assert(is_integer(DocId)),

        %% Test a query parser
        Offset = 0,
        PageSize = 10,
        Meta = xapian_record:record(document, record_info(fields, document)),

        F = fun(Query) ->
            RecList = ?SRV:query_page(Server, Offset, PageSize, Query, Meta),
            io:format(user, "~n~p~n", [RecList]),
            RecList
            end,

        P1 = #x_query_parser{},
        P2 = #x_query_parser{default_op='AND'},
        P4 = #x_query_parser{name=standard},

        Q1 = #x_query_string{parser=P1, value="dog"},
        Q2 = #x_query_string{parser=P2, value="dog fox"},

        %% Empty parsers
        Q3 = #x_query_string{parser=standard, value="dog"},
        Q4 = #x_query_string{parser=P4, value="dog"},

        F(Q1),
        F(Q2),
        F(Q3),
        F(Q4)
    after
        ?SRV:close(Server)
    end.


%% ------------------------------------------------------------------
%% Transations tests
%% ------------------------------------------------------------------

transaction_gen() ->
    % Open test
    Path1 = testdb_path(transaction1),
    Path2 = testdb_path(transaction2),
    Params = [write, create, overwrite],
    {ok, Server1} = ?SRV:start_link(Path1, Params),
    {ok, Server2} = ?SRV:start_link(Path2, Params),
    Fun = fun([_S1, _S2]) ->
        test_result
        end,
    BadFun = fun([_S1, _S2]) ->
        erlang:exit(badcat)
        end,
    %% Check fallback
    BadFun2 = fun([S1, _S2]) ->
        %% Try to kill S1.
        %% Server1 will be killed because of supervision.
        erlang:exit(S1, hello)
        end,
    %% Check fallback when the transaction process is still alive
    BadFun3 = fun([S1, _S2]) ->
        erlang:exit(S1, hello),
        %% Sleep for 1 second.
        %% Because this process is active, then the monitor process will 
        %% kill it, because one of the servers is dead.
        timer:sleep(1000)
        end,
    Result1 = ?SRV:transaction([Server1, Server2], Fun, infinity),
    Result2 = ?SRV:transaction([Server1, Server2], BadFun),
    erlang:unlink(Server1),

    %% Wait for DB closing.
    timer:sleep(1000),
    Result3 = ?SRV:transaction([Server1, Server2], BadFun2),

    %% Server1 was killed. Server2 will replace it.
    {ok, Server3} = ?SRV:start_link(Path1, Params),
    erlang:unlink(Server2),

    timer:sleep(1000),
    Result4 = ?SRV:transaction([Server2, Server3], BadFun3),

    %% Server3 is still alive.
    ?SRV:close(Server3),
    #x_transaction_result{
        is_committed=Committed1,
        is_consistent=Consistent1
    } = Result1,
    #x_transaction_result{
        is_committed=Committed2,
        is_consistent=Consistent2
    } = Result2,
    #x_transaction_result{
        is_committed=Committed3,
        is_consistent=Consistent3
    } = Result3,
    #x_transaction_result{
        is_committed=Committed4,
        is_consistent=Consistent4
    } = Result4,
    {"Check transactions' results for good and bad functions.",
        [ ?_assertEqual(Committed1, true)
        , ?_assertEqual(Consistent1, true)
        , ?_assertEqual(Committed2, false)
        , ?_assertEqual(Consistent2, true)
        , ?_assertEqual(Committed3, false)
        , ?_assertEqual(Consistent3, false)
        , ?_assertEqual(Committed4, false)
        , ?_assertEqual(Consistent4, false)
        , ?_assertEqual(erlang:is_process_alive(Server1), false)
        , ?_assertEqual(erlang:is_process_alive(Server2), false)
        , ?_assertEqual(erlang:is_process_alive(Server3), false)
        ]}.


transaction_timeout_gen() ->
    % Open test
    Path1 = testdb_path(tt1),
    Path2 = testdb_path(tt2),
    Params = [write, create, overwrite],
    {ok, Server1} = ?SRV:start_link(Path1, Params),
    {ok, Server2} = ?SRV:start_link(Path2, Params),
    Fun = fun([_S1, _S2]) ->
            timer:sleep(infinity)
        end,
    Result1 = ?SRV:transaction([Server1, Server2], Fun, 100),
    ?SRV:close(Server1),
    ?SRV:close(Server2),
    #x_transaction_result{
        is_committed=Committed1,
        is_consistent=Consistent1
    } = Result1,
    {"The transaction is killed by timeout.",
        [ ?_assertEqual(Committed1, false)
        , ?_assertEqual(Consistent1, true)
        ]}.


transaction_readonly_error_gen() ->
    % Open test
    Path1 = testdb_path(transaction1),
    Path2 = testdb_path(transaction4),
    Params1 = [],
    Params2 = [write, create, overwrite],
    {ok, Server1} = ?SRV:start_link(Path1, Params1),
    {ok, Server2} = ?SRV:start_link(Path2, Params2),
    Fun = fun([_S1, _S2]) ->
        test_result
        end,

    Result1 = ?SRV:transaction([Server1, Server2], Fun, infinity),
    Result2 = ?SRV:transaction([Server2, Server1], Fun, infinity),

    ?SRV:close(Server1),
    ?SRV:close(Server2),

    #x_transaction_result{
        is_committed=Committed1,
        is_consistent=Consistent1,
        reason=Reason1
    } = Result1,

    #x_transaction_result{
        is_committed=Committed2,
        is_consistent=Consistent2,
        reason=Reason2
    } = Result2,

    {"Cannot start transaction for readonly server.",
        [ {"read_only @ write",
          [ ?_assertEqual(Committed1, false)
          , ?_assertEqual(Consistent1, true)
          , ?_assertEqual(Reason1, readonly_db)
          ]}

        , {"write @ read_only",
          [ ?_assertEqual(Committed2, false)
          , ?_assertEqual(Consistent2, true)
          , ?_assertEqual(Reason2, readonly_db)
          ]}
        ]}.
    




%% ------------------------------------------------------------------
%% Extracting information
%% ------------------------------------------------------------------

%% The record will contain information about a document.
%% slot1 is a value.
%% docid and data are special fields.
-record(rec_test, {docid, slot1, data}).
-record(rec_test2, {docid, slot1, slot2, data}).
-record(short_rec_test, {data}).


read_document_test() ->
    % Open test
    Path = testdb_path(read_document),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = slot1}],
    Document =
        [ #x_stemmer{language = <<"english">>}
        , #x_data{value = "My test data as iolist"} 
        , #x_value{slot = slot1, value = "Slot #0"} 
        ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
    DocId = ?SRV:add_document(Server, Document),
    Meta = xapian_record:record(rec_test, record_info(fields, rec_test)),
    Rec = ?SRV:read_document(Server, DocId, Meta),
    ?assertEqual(Rec#rec_test.docid, 1),
    ?assertEqual(Rec#rec_test.slot1, <<"Slot #0">>),
    ?assertEqual(Rec#rec_test.data, <<"My test data as iolist">>)
    after
        ?SRV:close(Server)
    end.


document_info_test() ->
    % Open test
    Path = testdb_path(read_document),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = slot1}],
    Document =
        [ #x_stemmer{language = <<"english">>}
        , #x_data{value = "My test data as iolist"} 
        , #x_value{slot = slot1, value = "Slot #0"} 
        ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
    Meta = xapian_record:record(rec_test, record_info(fields, rec_test)),
    Rec = ?SRV:document_info(Server, Document, Meta),
    ?assertEqual(Rec#rec_test.docid, undefined),
    ?assertEqual(Rec#rec_test.slot1, <<"Slot #0">>),
    ?assertEqual(Rec#rec_test.data, <<"My test data as iolist">>)
    after
        ?SRV:close(Server)
    end.


read_float_value_gen() ->
    % Open test
    Path = testdb_path(read_float),
    Params = [write, create, overwrite
        , #x_value_name{slot = 1, name = slot1, type = float}
        , #x_value_name{slot = 2, name = slot2, type = string}
        ],
    Document1 =
        [ #x_data{value = "My test data as iolist"} 
        , #x_value{slot = slot1, value = 7} 
        ],
    Document2 =
        [ #x_data{value = "My test data as iolist"} 
        , #x_value{slot = slot1, value = 66} 
        , #x_value{slot = slot2, value = "tentacle"} 
        ],
    Document3 =
        [ #x_data{value = "My test data as iolist"} 
        ],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        DocId1 = ?SRV:add_document(Server, Document1),
        DocId2 = ?SRV:add_document(Server, Document2),
        DocId3 = ?SRV:add_document(Server, Document3),

        Meta = xapian_record:record(rec_test2, record_info(fields, rec_test2)),
        Rec1 = ?SRV:read_document(Server, DocId1, Meta),
        Rec2 = ?SRV:read_document(Server, DocId2, Meta),
        Rec3 = ?SRV:read_document(Server, DocId3, Meta),

        %% #document{} is the simple container.
        Meta2 = xapian_record:record(document, record_info(fields, document)),
        Offset    = 0,
        PageSize  = 10,
        Query68   = #x_query_value_range{slot=slot1, from=6, to=8},
        Query8    = #x_query_value{op=lower, slot=slot1, value=8},
        Query7    = #x_query_value_range{slot=slot1, from=7, to=7},

        RecList68 = ?SRV:query_page(Server, Offset, PageSize, Query68, Meta2),
        RecList8  = ?SRV:query_page(Server, Offset, PageSize, Query8, Meta2),
        RecList7  = ?SRV:query_page(Server, Offset, PageSize, Query7, Meta2),

        [ ?_assertEqual(Rec1#rec_test2.docid, 1)
        , ?_assertEqual(Rec1#rec_test2.slot1, 7.0)
        , ?_assertEqual(Rec1#rec_test2.slot2, undefined)
        , ?_assertEqual(Rec1#rec_test2.data, <<"My test data as iolist">>)
          
        , ?_assertEqual(Rec2#rec_test2.docid, 2)
        , ?_assertEqual(Rec2#rec_test2.slot1, 66.0)
        , ?_assertEqual(Rec2#rec_test2.slot2, <<"tentacle">>)
        , ?_assertEqual(Rec2#rec_test2.data, <<"My test data as iolist">>)
          
        , ?_assertEqual(Rec3#rec_test2.docid, 3)
        , ?_assertEqual(Rec3#rec_test2.slot1, undefined)
        , ?_assertEqual(Rec3#rec_test2.slot2, undefined)
        , ?_assertEqual(Rec3#rec_test2.data, <<"My test data as iolist">>)
          
        , ?_assertEqual(RecList68, [#document{docid=1}])
        , ?_assertEqual(RecList7,  [#document{docid=1}])
        , ?_assertEqual(RecList8,  [#document{docid=1}])
        ]
    after
        ?SRV:close(Server)
    end.


short_record_test() ->
    Path = testdb_path(short_rec_test),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    Document = [#x_data{value = "ok"}],
    DocId = ?SRV:add_document(Server, Document),
    Meta = xapian_record:record(short_rec_test, record_info(fields, short_rec_test)),
    Rec = ?SRV:read_document(Server, DocId, Meta),
    ?assertEqual(Rec#short_rec_test.data, <<"ok">>),
    ?SRV:close(Server).


%% @doc Check an exception.
read_bad_docid_test() ->
    % Open test
    Path = testdb_path(read_document),
    Params = [#x_value_name{slot = 1, name = slot1}],
    {ok, Server} = ?SRV:start_link(Path, Params),
    Meta = xapian_record:record(rec_test, record_info(fields, rec_test)),
    DocId = 2,
    % ?assertException(ClassPattern, TermPattern, Expr)
    ?assertException(error, 
        #x_error{type = <<"DocNotFoundError">>}, 
        ?SRV:read_document(Server, DocId, Meta)),
    ?SRV:close(Server).


%% ------------------------------------------------------------------
%% Books (query testing)
%% ------------------------------------------------------------------


%% See Driver::selectEncoderAndRetrieveDocument.
%% book fields are in Document.
%% book_ext fields are both in Document and in Iterator.
%% book_iter fields are both in in Iterator.

%% These records are used for tests.
%% They describe values of documents.
-record(book, {docid, author, title, data}).
-record(book_ext, {docid, author, title, data,   rank, weight, percent}).
-record(book_iter, {docid, rank, weight, percent}).


%% These cases will be runned sequencly.
%% `Server' will be passed as a parameter.
%% `Server' will be opened just once for all cases.
cases_gen() ->
    Cases = 
    [ fun single_term_query_page_case/1
    , fun value_range_query_page_case/1
    , fun double_terms_or_query_page_case/1
    , fun special_fields_query_page_case/1

    , fun document_case/1
    , fun enquire_case/1
    , fun enquire_sort_order_case/1
    , fun enquire_key_maker_case/1
    , fun resource_cleanup_on_process_down_case/1
    , fun enquire_to_mset_case/1
    , fun qlc_mset_case/1
    , fun qlc_mset_doc_case/1
    , fun qlc_mset_iter_case/1

    , fun create_user_resource_case/1
    , fun release_resource_case/1
    , fun release_table_case/1
    , fun release_table2_case/1

    %% Advanced enquires
    , fun advanced_enquire_case/1
    , fun advanced_enquire_weight_case/1

    %% Info
    , fun match_set_info_case/1
    , fun database_info_case/1
    ],
    Server = query_page_setup(),
    %% One setup for each test
    {setup, 
        fun() -> Server end, 
        fun query_page_clean/1,
        [Case(Server) || Case <- Cases]}.
    

query_page_setup() ->
    % Open test
	Path = testdb_path(query_page),
    ValueNames = [ #x_value_name{slot = 1, name = author}
                 , #x_value_name{slot = 2, name = title}],
    Params = [write, create, overwrite] ++ ValueNames,
    {ok, Server} = ?SRV:start_link(Path, Params),
    Base = [#x_stemmer{language = <<"english">>}],
    Document1 = Base ++
        [ #x_data{value = "Non-indexed data here"} 
        , #x_text{value = "erlang/OTP"} 
        , #x_text{value = "concurrency"} 
        , #x_term{value = "telecom"} 
        , #x_value{slot = title, value = "Software for a Concurrent World"} 
        , #x_value{slot = author, value = "Joe Armstrong"} 
        ],
    Document2 = Base ++
        [ #x_stemmer{language = <<"english">>}
        , #x_text{value = "C++"} 
        , #x_term{value = "game"} 
        , #x_value{slot = title, value = "Code Complete: "
            "A Practical Handbook of Software Construction"} 
        , #x_value{slot = author, value = "Steve McConnell"} 
        ],
    %% Put the documents into the database
    [1, 2] = 
    [ ?SRV:add_document(Server, Document) || Document <- [Document1, Document2] ],
    Server.

query_page_clean(Server) ->
    ?SRV:close(Server).

single_term_query_page_case(Server) ->
    Case = fun() ->
        Offset = 0,
        PageSize = 10,
        Query = "erlang",
        Meta = xapian_record:record(book, record_info(fields, book)),
        RecList = ?SRV:query_page(Server, Offset, PageSize, Query, Meta),
        io:format(user, "~n~p~n", [RecList])
        end,
    {"erlang", Case}.

value_range_query_page_case(Server) ->
    Case = fun() ->
        Offset = 0,
        PageSize = 10,
        Query = #x_query_value_range{slot=author, 
                                     from="Joe Armstrong", 
                                     to="Joe Armstrong"},
        Meta = xapian_record:record(book, record_info(fields, book)),
        RecList = ?SRV:query_page(Server, Offset, PageSize, Query, Meta),
        io:format(user, "~n~p~n", [RecList])
        end,
    {"Joe Armstrong - Joe Armstrong", Case}.

double_terms_or_query_page_case(Server) ->
    Case = fun() ->
        Offset = 0,
        PageSize = 10,
        Query = #x_query{op='OR', value=[<<"erlang">>, "c++"]},
        Meta = xapian_record:record(book, record_info(fields, book)),
        RecList = ?SRV:query_page(Server, Offset, PageSize, Query, Meta),
        io:format(user, "~n~p~n", [RecList])
        end,
    {"erlang OR c++", Case}.


%% You can get dynamicly calculated fields.
special_fields_query_page_case(Server) ->
    Case = fun() ->
        Offset = 0,
        PageSize = 10,
        Query = "erlang",
        Meta = xapian_record:record(book_ext, record_info(fields, book_ext)),
        RecList = ?SRV:query_page(Server, Offset, PageSize, Query, Meta),
        io:format(user, "~n~p~n", [RecList])
        end,
    {"erlang (with rank, weight, percent)", Case}.


document_case(Server) ->
    DocRes1 = xapian_server:document(Server, "telecom"),
    DocRes2 = xapian_server:document(Server, 1),
    Meta = xapian_term_record:record(term, record_info(fields, term)),
    AllDocumentTermsFn = 
    fun(DocRes) ->
        Table = xapian_term_qlc:document_term_table(
            Server, DocRes, Meta, [ignore_empty]),
        ?SRV:release_resource(Server, DocRes),
        qlc:e(qlc:q([X || X <- Table]))
        end,
    Doc1Terms = AllDocumentTermsFn(DocRes1),
    Doc2Terms = AllDocumentTermsFn(DocRes2),
    [ {"Get a document resource by a term or by an id.",
       [?_assertEqual(Doc1Terms, Doc2Terms)]}
    ].


%% Xapian uses `Xapian::Enquire' class as a hub for making queries.
%% Enquire object can be handled as a resource.
enquire_case(Server) ->
    Case = fun() ->
        Query = "erlang",
        ResourceId = ?SRV:enquire(Server, Query),
        io:format(user, "~n~p~n", [ResourceId]),
        ?SRV:release_resource(Server, ResourceId)
        end,
    {"Simple enquire resource", Case}.


enquire_sort_order_case(Server) ->
    Case = fun() ->
        %% Sort by value in the 'title' slot
        Order = #x_sort_order{type=value, value=title},
        %% telecom OR game
        Query = #x_query{op = 'OR', value = ["telecom", "game"]},
        EnquireDescriptor = #x_enquire{order=Order, value=Query},
        AllIds = all_record_ids(Server, EnquireDescriptor),

        %% Were two documents selected?
        ?assertMatch([_, _], AllIds),

        %% Check documents order
        %% Code = 2, Software = 1
        ?assertMatch([2, 1], AllIds),

        %% The same case, but it is sorted in the reversed order
        RevOrder = #x_sort_order{type=value, value=title, is_reversed = true},
        RevEnquireDescriptor = #x_enquire{order=RevOrder, value=Query},
        RevAllIds = all_record_ids(Server, RevEnquireDescriptor),

        ?assertEqual(lists:reverse(RevAllIds), AllIds)
        end,
    {"Enquire with sorting", Case}.


%% TODO: more strict testing
enquire_key_maker_case(Server) ->
    Case = fun() ->
        KeyMaker = xapian_resource:multi_value_key_maker(Server, [author, title]),
        Order = #x_sort_order{type=key, value=KeyMaker},
        %% telecom OR game
        Query = #x_query{op = 'OR', value = ["telecom", "game"]},
        EnquireDescriptor = #x_enquire{order=Order, value=Query},
        AllIds = all_record_ids(Server, EnquireDescriptor),
        xapian_server:release_resource(Server, KeyMaker),

        %% Check documents order
        %% Code = 2, Software = 1
        ?assertMatch([1, 2], AllIds)
        end,
    {"Enquire with sorting", Case}.


%% If the client is dead, then its resources will be released.
resource_cleanup_on_process_down_case(Server) ->
    Case = fun() ->
        Home = self(),
        Ref = make_ref(),
        spawn_link(fun() ->
                Query = "erlang",
                ResourceId = ?SRV:enquire(Server, Query),
                Home ! {resource_id, Ref, ResourceId}
                end),

        ResourceId = 
        receive
            {resource_id, Ref, ResourceIdI} ->
                ResourceIdI
        end,

        ?assertError(elem_not_found, ?SRV:release_resource(Server, ResourceId))
        end,
    {"Check garbidge collection for resources", Case}.


enquire_to_mset_case(Server) ->
    Case = fun() ->
        Query = "erlang",
        EnquireResourceId = ?SRV:enquire(Server, Query),
        MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),
        io:format(user, "~n ~p ~p~n", [EnquireResourceId, MSetResourceId]),
        ?SRV:release_resource(Server, EnquireResourceId),
        ?SRV:release_resource(Server, MSetResourceId)
        end,
    {"Check conversation", Case}.


qlc_mset_case(Server) ->
    Case = fun() ->
        %% Query is a query to make for retrieving documents from Xapian.
        %% Each document object will be mapped into a document record. 
        %% A document record is a normal erlang record, 
        %% it has structure, described by the user,
        %% using  the `xapian_record:record' call.
        Query = "erlang",
        EnquireResourceId = ?SRV:enquire(Server, Query),
        MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),

        %% Meta is a record, which contains some information about
        %% structure of a document record.
        %% The definition of Meta is incapsulated inside `xapian_record' module.
        Meta = xapian_record:record(book_ext, record_info(fields, book_ext)),

        %% Create QlcTable from MSet. 
        %% After creation of QlcTable, MSet can be removed.
        Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),
        ?SRV:release_resource(Server, MSetResourceId),

        %% QueryAll is a list of all matched records.
        QueryAll = qlc:q([X || X <- Table]),

        %% Check `lookup' function. This function is used by `qlc' module.
        %% It will be called to find a record by an index.
        QueryFilter = qlc:q(
            [X || X=#book_ext{docid=DocId} <- Table, DocId =:= 1]),
        Queries = [QueryAll, QueryFilter],

        %% For each query...
        [begin
            %% ... evaluate (execute) ...
            Records = qlc:e(Q),
            %% ... and print out.
            io:format(user, "~n ~p~n", [Records])
            end || Q <- Queries
        ],

        %% This case will cause an error, because DocId > 0.
        QueryBadFilter = qlc:q(
            [X || X=#book_ext{docid=DocId} <- Table, DocId =:= 0]),
        ?assertError(bad_docid, qlc:e(QueryBadFilter))
        end,
    {"Check internal_qlc_init", Case}.


qlc_mset_doc_case(Server) ->
    Case = fun() ->
        %% Query is a query to make for retrieving documents from Xapian.
        %% Each document object will be mapped into a document record. 
        %% A document record is a normal erlang record, 
        %% it has structure, described by the user,
        %% using  the `xapian_record:record' call.
        Query = "erlang",
        EnquireResourceId = ?SRV:enquire(Server, Query),
        MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),

        %% Meta is a record, which contains some information about
        %% structure of a document record.
        %% The definition of Meta is incapsulated inside `xapian_record' module.
        Meta = xapian_record:record(book, record_info(fields, book)),

        %% Create QlcTable from MSet. 
        %% After creation of QlcTable, MSet can be removed.
        Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),
        ?SRV:release_resource(Server, MSetResourceId),

        %% QueryAll is a list of all matched records.
        QueryAll = qlc:q([X || X <- Table]),

        %% Check `lookup' function. This function is used by `qlc' module.
        %% It will be called to find a record by an index.
        QueryFilter = qlc:q(
            [X || X=#book{docid=DocId} <- Table, DocId =:= 1]),
        QueryFilter2 = qlc:q(
            [X || X=#book{docid=DocId} <- Table, DocId =:= 1 orelse DocId =:= 2]),
        QueryFilter3 = qlc:q(
            [X || X=#book{docid=DocId} <- Table, DocId =:= 2 orelse DocId =:= 1]),
        Queries = [QueryAll, QueryFilter, QueryFilter2, QueryFilter3],

        %% For each query...
        [begin
            %% ... evaluate (execute) ...
            Records = qlc:e(Q),
            %% ... and print out.
            io:format(user, "~n ~p~n", [Records])
            end || Q <- Queries
        ],

        %% This case will cause an error, because DocId > 0.
        QueryBadFilter = qlc:q(
            [X || X=#book{docid=DocId} <- Table, DocId =:= 0]),
        ?assertError(bad_docid, qlc:e(QueryBadFilter))
        end,
    {"Check an iterator source.", Case}.


qlc_mset_iter_case(Server) ->
    Case = fun() ->
        %% Query is a query to make for retrieving documents from Xapian.
        %% Each document object will be mapped into a document record. 
        %% A document record is a normal erlang record, 
        %% it has structure, described by the user,
        %% using  the `xapian_record:record' call.
        Query = "erlang",
        EnquireResourceId = ?SRV:enquire(Server, Query),
        MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),

        %% Meta is a record, which contains some information about
        %% structure of a document record.
        %% The definition of Meta is incapsulated inside `xapian_record' module.
        Meta = xapian_record:record(book_iter, record_info(fields, book_iter)),

        %% Create QlcTable from MSet. 
        %% After creation of QlcTable, MSet can be removed.
        Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),
        ?SRV:release_resource(Server, MSetResourceId),

        %% QueryAll is a list of all matched records.
        QueryAll = qlc:q([X || X <- Table]),

        %% Check `lookup' function. This function is used by `qlc' module.
        %% It will be called to find a record by an index.
        QueryFilter = qlc:q(
            [X || X=#book_iter{docid=DocId} <- Table, DocId =:= 1]),
        Queries = [QueryAll, QueryFilter],

        %% For each query...
        [begin
            %% ... evaluate (execute) ...
            Records = qlc:e(Q),
            %% ... and print out.
            io:format(user, "~n ~p~n", [Records])
            end || Q <- Queries
        ],

        %% This case will cause an error, because DocId > 0.
        QueryBadFilter = qlc:q(
            [X || X=#book_iter{docid=DocId} <- Table, DocId =:= 0]),
        ?assertError(bad_docid, qlc:e(QueryBadFilter))
        end,
    {"Check an iterator source.", Case}.


create_user_resource_case(Server) ->
    Case = fun() ->
        %% User-defined resource is an object, which is created on C++ side.
        %% We using Erlang references for returning it back to the user.
        %% A reference can be used only with this Server.
        ResourceId = ?SRV:internal_create_resource(Server, bool_weight),
        io:format(user, "User-defined resource ~p~n", [ResourceId])
        end,
    {"Check creation of user-defined resources", Case}.


%% Additional parameters can be passed to `Xapian::Enquire'.
%% We use `#x_enquire' record for this.
advanced_enquire_case(Server) ->
    Case = fun() ->
        Query = #x_enquire{
            value = "Erlang"
        },
        EnquireResourceId = ?SRV:enquire(Server, Query),
        ?assert(is_reference(EnquireResourceId)),
        ?SRV:release_resource(Server, EnquireResourceId)
        end,
    {"Check #x_enquire{}", Case}.


%% We can pass other `Xapian::Weight' object, stored as an user resource.
%% We create new `Xapian::BoolWeight' object as a resource and pass it back
%% as an additional parameter.
advanced_enquire_weight_case(Server) ->
    Case = fun() ->
        Query = #x_enquire{
            value = "Erlang",
            weighting_scheme = xapian_resource:bool_weight(Server)
        },
        EnquireResourceId = ?SRV:enquire(Server, Query),
        ?assert(is_reference(EnquireResourceId)),
        ?SRV:release_resource(Server, EnquireResourceId)
        end,
    {"Check #x_enquire{weight=Xapian::BoolWeight}", Case}.


match_set_info_case(Server) ->
    Case = fun() ->
        Query = "erlang",
        EnquireResourceId = ?SRV:enquire(Server, Query),
        ?assert(is_reference(EnquireResourceId)),
        MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),

        try
            Info = 
            ?SRV:mset_info(Server, MSetResourceId, [matches_lower_bound, size]),
            ?assertEqual(1, ?SRV:mset_info(Server, MSetResourceId, size)),

            %% All atom props
            PropKeys = xapian_mset_info:properties(),
            AllItems1 = ?SRV:mset_info(Server, MSetResourceId, PropKeys),
            AllItems2 = ?SRV:mset_info(Server, MSetResourceId),
            ?assertEqual(AllItems1, AllItems2),

            io:format(user, "~nMSet Info: ~p~n", [Info]),

            %% All pair props
            [Pair1Key, Pair2Key] = 
            PairProps = [{term_weight, "erlang"}, {term_freq, "erlang"}],
            PairPropResult = ?SRV:mset_info(Server, MSetResourceId, PairProps),
            ?assertMatch([{Pair1Key, _0dot4}, {Pair2Key, 1}], 
                         PairPropResult)

        after
            ?SRV:release_resource(Server, EnquireResourceId),
            ?SRV:release_resource(Server, MSetResourceId)
        end
        end,
    {"Check mset_info function.", Case}.


release_resource_case(Server) ->
    Case = fun() ->
        EnquireResourceId = ?SRV:enquire(Server, "erlang"),
        ?SRV:release_resource(Server, EnquireResourceId),

        %% Try call it twice
        ?assertError(elem_not_found,
            ?SRV:release_resource(Server, EnquireResourceId))
        end,
    {"Check xapian_server:release_resource", Case}.


release_table_case(Server) ->
    Case = fun() ->
        %% Create a Qlc Table for query "erlang".
        Table = mset_table(Server, "erlang", document),
        Ref = ?SRV:qlc_table_to_reference(Server, Table),
        ?SRV:release_table(Server, Table),

        %% Try call it twice
        ?assertError(elem_not_found,
            ?SRV:release_resource(Server, Ref))
        end,
    {"Try delete the reference after deleting the table.", Case}.


release_table2_case(Server) ->
    Case = fun() ->
        %% Create a Qlc Table for query "erlang".
        Table = mset_table(Server, "erlang", document),
        Ref = ?SRV:qlc_table_to_reference(Server, Table),
        ?SRV:release_resource(Server, Ref),

        %% Try call it twice
        ?assertError(elem_not_found,
            ?SRV:release_table(Server, Table))
        end,
    {"Try delete the table after deleting the reference.", Case}.


database_info_case(Server) ->
    Case = fun() ->
        Info = 
        ?SRV:database_info(Server, [document_count]),
        io:format(user, "~nDB Info: ~p~n", [Info]),

        %% Atoms
        AllItems1 = ?SRV:database_info(Server, xapian_db_info:properties()),
        AllItems2 = ?SRV:database_info(Server),
        ?assertEqual(AllItems1, AllItems2),

        %% Pairs
        ?assertEqual(?SRV:database_info(Server, [{term_exists, <<"erlang">>}]),
                     [{{term_exists, <<"erlang">>}, true}]),
        ?assertEqual(?SRV:database_info(Server, [{term_exists, <<"prolog">>}]),
                     [{{term_exists, <<"prolog">>}, false}]),

        ?assert(?SRV:database_info(Server, {term_exists, <<"erlang">>})),
        ?assertNot(?SRV:database_info(Server, {term_exists, <<"prolog">>})),

        ?assertEqual(1, ?SRV:database_info(Server, 
                                           {term_freq, <<"erlang">>})),

        ?assertEqual(undefined, ?SRV:database_info(Server, 
                                           {term_freq, <<"prolog">>})),

        ?assertEqual(undefined, ?SRV:database_info(Server, 
                                           {collection_freq, <<"prolog">>})),

        ?assert(is_integer(?SRV:database_info(Server, 
                                              {document_length, 1}))),
        ?assertEqual(undefined, ?SRV:database_info(Server, 
                                           {document_length, 1000}))
        
        end,
    {"Check database_info function.", Case}.


metadata_gen() ->
    Path = testdb_path(metadata),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    ?SRV:set_metadata(Server, "key", "value"),
    Info = 
    ?SRV:database_info(Server, {metadata, "key"}),
    Info2 = 
    ?SRV:database_info(Server, {metadata, "bad_key"}),
    ?SRV:close(Server),
    [?_assertEqual(Info, <<"value">>)
    ,?_assertEqual(Info2, <<"">>)
    ].


%% http://trac.xapian.org/wiki/FAQ/ExtraWeight
extra_weight_gen() ->
    Path = testdb_path(extra_weight),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    Terms = ["Sxapian", "weight"],
    Document = [#x_term{value = X} || X <- Terms],
    DocId = ?SRV:add_document(Server, Document),
    Query = extra_weight_query(2.5, "Sxapian", "weight"),
    Ids = all_record_ids(Server, Query),
    [?_assertEqual(Ids, [DocId])].


large_db_and_qlc_test() ->
    Path = testdb_path(large_db_and_qlc),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    try
        Terms = ["xapian", "erlang"],

        Document = [#x_term{value = X} || X <- Terms],
        ExpectedDocIds = lists:seq(1, 1000),
        DocIds = [begin
            ?SRV:add_document(Server, Document)
            end || _ <- ExpectedDocIds],
        ?assertEqual(DocIds, ExpectedDocIds),

        Query = "erlang",
        Cursor = all_record_cursor(Server, Query),
        try
            cursor_walk(1, 1001, Cursor)
        after
            qlc:delete_cursor(Cursor)
        end
    after
        ?SRV:close(Server)
    end.


large_db_and_qlc_mset_with_joins_test() ->
    Path = testdb_path(large_db_and_qlc_joins),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),

    ExpectedDocIds = lists:seq(1, 1000),
    DocIds = [begin
            Document = [ #x_term{value = integer_to_list(Id)} ],
            ?SRV:add_document(Server, Document)
        end || Id <- ExpectedDocIds],
    ?assertEqual(DocIds, ExpectedDocIds),

    Query = "",
    Table = mset_table(Server, Query, document),
    QH1 = qlc:q([Id || #document{docid=Id} <- Table, Id =< 500]),
    QH2 = qlc:q([Id || #document{docid=Id} <- Table, Id > 500]),
    QH3 = qlc:append(QH1, QH2),
    Cursor = qlc:cursor(QH3),
    try
        cursor_walk(1, 1001, Cursor)
    after
        qlc:delete_cursor(Cursor)
    end.



%% Id =:= Max
cursor_walk(Id, Id, Cursor) ->
    Result = qlc:next_answers(Cursor, 1),
    ?assertEqual(Result, []),
    [];

cursor_walk(Id, Max, Cursor) ->
    Result = qlc:next_answers(Cursor, 1),
    ?assertEqual(Result, [Id]),
    cursor_walk(Id+1, Max, Cursor).


%% `Title' and `Body' are queries.
extra_weight_query(Factor, Title, Body) ->
    Scale = #x_query_scale_weight{factor = Factor, value = Title},
    #x_query{value = [Scale, Body]}.



%% -------------------------------------------------------------------
%%  Multi-DB support
%% -------------------------------------------------------------------

-record(mdocument, {docid, db_name, multi_docid, db_number}).


mset_table(Server, Query, document) ->
    Meta = xapian_record:record(document, record_info(fields, document)),
    mset_table(Server, Query, Meta);

mset_table(Server, Query, mdocument) ->
    Meta = xapian_record:record(mdocument, record_info(fields, mdocument)),
    mset_table(Server, Query, Meta);

mset_table(Server, Query, Meta) ->
    EnquireResourceId = ?SRV:enquire(Server, Query),
    MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),
    Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),
    %% Table has a pointer on resources.
    ?SRV:release_resource(Server, EnquireResourceId),
    ?SRV:release_resource(Server, MSetResourceId),
    Table.


all_record_ids(Server, Query) ->
    Table = mset_table(Server, Query, document),
    Ids = qlc:e(qlc:q([Id || #document{docid=Id} <- Table])),
    Ids.


all_record_cursor(Server, Query) ->
    Table = mset_table(Server, Query, document),
    qlc:cursor(qlc:q([Id || #document{docid=Id} <- Table])).


all_multidb_records(Server, Query) ->
    Table = mset_table(Server, Query, mdocument),
    qlc:e(qlc:q([X || X <- Table])).


record_by_id(Server, Query, Id) ->
    Table = mset_table(Server, Query, mdocument),
    qlc:e(qlc:q([X || X=#mdocument{docid=DocId} <- Table, Id =:= DocId])).


multidb_record_by_id(Server, Query, Id) ->
    Table = mset_table(Server, Query, mdocument),
    qlc:e(qlc:q([X || X=#mdocument{multi_docid=DocId} <- Table, Id =:= DocId])).


%% Simple usage of a merged DB.
multi_db_gen() ->
    Path1 = #x_database{name=multi1, path=testdb_path(multi1)},
    Path2 = #x_database{name=multi2, path=testdb_path(multi2)},
    Params = [write, create, overwrite],
    Document = [#x_term{value = "test"}],
    {ok, Server1} = ?SRV:start_link(Path1, Params),
    {ok, Server2} = ?SRV:start_link(Path2, Params),
    DocId1 = ?SRV:add_document(Server1, Document),
    DocId2 = ?SRV:add_document(Server2, Document),
    ?SRV:close(Server1),
    ?SRV:close(Server2),

    %% Merged server
    {ok, Server} = ?SRV:start_link([Path1, Path2], []),
    Query    = "test",
    Ids      = all_record_ids(Server, Query),
    Records  = all_multidb_records(Server, Query),
    DbNames  = elements(#mdocument.db_name, Records),
    MultiIds = elements(#mdocument.multi_docid, Records),
    DbNums   = elements(#mdocument.db_number, Records),
    LookupRecords1 = record_by_id(Server, Query, 1),
    LookupRecords2 = record_by_id(Server, Query, 5),
    LookupRecords3 = multidb_record_by_id(Server, Query, 1),
    LookupRecords4 = multidb_record_by_id(Server, Query, 2),
    LookupRecords5 = multidb_record_by_id(Server, Query, 5),

    [?_assertEqual([DocId1, DocId2], [1,1])
    ,?_assertEqual(Ids, [1,1])
    ,?_assertEqual(DbNames, [multi1, multi2])
    ,?_assertEqual(MultiIds, [1,2])
    ,?_assertEqual(DbNums, [1,2])
    ,{"Document is not found by id.",
        [?_assertEqual(LookupRecords5, [])
        ,?_assertEqual(LookupRecords2, [])]}
    ,?_assertEqual(length(LookupRecords1), 2)
    ,?_assertEqual(length(LookupRecords3), 1)
    ,?_assertEqual(length(LookupRecords4), 1)
    ].


multi_docid_gen() ->
    Path1 = #x_database{name=multi_docid1, path=testdb_path(multi1)},
    Path2 = #x_database{name=multi_docid2, path=testdb_path(multi2)},
    Params = [write, create, overwrite],
    {ok, Server1} = ?SRV:start_link(Path1, Params),
    {ok, Server2} = ?SRV:start_link(Path2, Params),
    ?SRV:close(Server1),
    ?SRV:close(Server2),

    %% Merged server
    {ok, Server} = ?SRV:start_link([Path1, Path2], []),

    %% xapian_server:multi_docid
    [ ?_assertEqual(1, ?SRV:multi_docid(Server, 1, multi_docid1))
    , ?_assertEqual(2, ?SRV:multi_docid(Server, 1, multi_docid2))
    , ?_assertEqual(3, ?SRV:multi_docid(Server, 2, multi_docid1))
    , ?_assertEqual(4, ?SRV:multi_docid(Server, 2, multi_docid2))
    ].


elements(Pos, Records) ->
    [erlang:element(Pos, Rec) || Rec <- Records].


remote_db_test() ->
    Params = [writable, link, {port, 6666}],
    DBList = [testdb_path(tcp_remote)],
    xapian_utility:tcp_server(DBList, Params),
    timer:sleep(1000),
    DBConfig = #x_tcp_database{port = 6666, host = "127.0.0.1"},
    {ok, Server} = ?SRV:start_link(DBConfig, [write]),
    ?SRV:close(Server).



get_state_fields_gen() ->
    Params = 
        [ #x_value_name{slot = 1, name = slot1, type = float}
        , #x_value_name{slot = 2, name = slot2, type = string}
        ],
    {ok, Server} = ?SRV:start_link([], Params),

    try
    N2S = ?SRV:name_to_slot(Server),
    Num1_1 = xapian_common:slot_id(slot1, N2S),
    Num1_2 = xapian_common:slot_id(1, N2S),
    Num2_1 = xapian_common:slot_id(slot2, N2S),
    Num2_2 = xapian_common:slot_id(2, N2S),

    Slot2SlotTests = [{"Slot number is the same.",
                     [ ?_assertEqual(Num1_2, 1)
                     , ?_assertEqual(Num2_2, 2)]}],

    Name2SlotTests = [{"Name to number conversation.",
                     [ ?_assertEqual(Num1_1, 1)
                     , ?_assertEqual(Num2_1, 2)]}],

    S2T = ?SRV:slot_to_type(Server),

    Type1 = xapian_common:slot_type(1, S2T),
    Type2 = xapian_common:slot_type(2, S2T),

    SlotTypeTests2 = [{"Name or number to type conversation.",
                     [ ?_assertEqual(Type1, float)
                     , ?_assertEqual(Type2, string)
                     ]}],

    Slot1_1 = ?SRV:name_to_slot(Server, slot1),
    Slot1_2 = ?SRV:name_to_slot(Server, 1),

    Slot2_1 = ?SRV:name_to_slot(Server, slot2),
    Slot2_2 = ?SRV:name_to_slot(Server, 2),

    Slot2SlotTests2 = [{"Slot number is the same.",
                      [ ?_assertEqual(Slot1_2, 1)
                      , ?_assertEqual(Slot2_2, 2)]}],

    Name2SlotTests2 = [{"Name to number conversation.",
                      [ ?_assertEqual(Slot1_1, 1)
                      , ?_assertEqual(Slot2_1, 2)]}],

    SlotType1_1 = ?SRV:slot_to_type(Server, slot1),
    SlotType1_2 = ?SRV:slot_to_type(Server, 1),

    SlotType2_1 = ?SRV:slot_to_type(Server, slot2),
    SlotType2_2 = ?SRV:slot_to_type(Server, 2),

    SlotTypeTests = [{"Name or number to type conversation.",
                     [ ?_assertEqual(SlotType1_1, float)
                     , ?_assertEqual(SlotType1_2, float)
                     , ?_assertEqual(SlotType2_1, string)
                     , ?_assertEqual(SlotType2_2, string)
                     ]}],

    Slot2SlotTests ++ Name2SlotTests ++
    Slot2SlotTests2 ++ Name2SlotTests2 ++
    SlotTypeTests ++ SlotTypeTests2
    after
        ?SRV:close(Server)
    end.
