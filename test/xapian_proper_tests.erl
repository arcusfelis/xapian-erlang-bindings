-module(xapian_proper_tests).
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
-record(mdocument, {docid, db_name, multi_docid, db_number}).
    

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


%% This test tests the lookup function of a mset qlc table.
%% Besides, it tests `xapian_server:multi_docid/3' function and 
%% other functions for conversation between: 
%% * `docid', `db_number' and `multi_docid';
%% * `db_name' and `db_number'.
prop_multi_db() ->
    Path1 = #x_database{name=multi1, path=testdb_path(multi_docid1)},
    Path2 = #x_database{name=multi2, path=testdb_path(multi_docid2)},
    Path3 = #x_database{name=multi3, path=testdb_path(multi_docid3)},
    Params = [write, create, overwrite],
    Document = [#x_term{value = "test"}],
    {ok, Server1} = ?SRV:start_link(Path1, Params),
    {ok, Server2} = ?SRV:start_link(Path2, Params),
    {ok, Server3} = ?SRV:start_link(Path3, Params),
    Servers = [Server1, Server2, Server3],
    Paths = [Path1, Path2, Path3],
    try
        [?SRV:add_document(S, Document) 
            || _N <- lists:seq(1, 1000), 
                S <- Servers ]
    after
        [ ?SRV:close(S) || S <- Servers ]
    end,

    %% Merged server
    {ok, Server} = ?SRV:start_link(Paths, []),
    Query = "test",
    multidb_record_by_id(Server, Query, 1),
    multidb_record_by_id(Server, Query, 2),
    multidb_record_by_id(Server, Query, 5),

    ?FORALL({DocId, Db}, 
            {range(1,1000), oneof([1, 2, 3, multi1, multi2, multi3])},
        begin
        MultiDocId = ?SRV:multi_docid(Server, DocId, Db),
        Doc = #mdocument{docid=DocId, 
                    multi_docid=MultiDocId, 
                    db_name=multi_sub_db_id_to_name(Db), 
                    db_number=multi_sub_db_name_to_id(Db)},
        equals([Doc], multidb_record_by_id(Server, Query, MultiDocId))
        end).


prop_large_db_and_qlc_index() ->
    Path = testdb_path(large_db_and_qlc_index),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    Terms = ["xapian", "erlang"],

    Document = [#x_term{value = X} || X <- Terms],
    ExpectedDocIds = lists:seq(1, 1000),
    DocIds = [ ?SRV:add_document(Server, Document) || _ <- ExpectedDocIds ],
    ?assertEqual(DocIds, ExpectedDocIds),

    Meta = xapian_record:record(document, record_info(fields, document)),
    ?FORALL({DocId, Term}, 
            {range(1,1000), oneof(Terms)},
        begin
        EnquireResourceId = ?SRV:enquire(Server, Term),
        MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),
        Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),
        try
            Query = qlc:q([Id || #document{docid=Id} <- Table, Id =:= DocId]),
            Ids = qlc:e(Query),
            equals([DocId], Ids)
        after
            ?SRV:release_table(Server, Table)
        end
        end).


prop_echo() ->
    {ok, Server} = ?SRV:start_link([], []),
    ?FORALL(Bin, binary(),
        begin
        equals(Bin, ?SRV:internal_test_run(Server, echo, Bin))
        end).



prop_query_parser() ->
    Path   = testdb_path(prop_parser),
    Params = [write, create, overwrite],

    Text   = "a b c",
    Terms  = string:tokens(Text, " "),

    {ok, Server} = ?SRV:start_link(Path, Params),

    %% Test a term generator
    %% TODO: There are different results with and without stemmer.
    Document = [#x_stemmer{language = <<"english">>}, #x_text{value = Text}],
    DocId = ?SRV:add_document(Server, Document),

    %% Test a query parser
    Meta = xapian_record:record(document, record_info(fields, document)),

    F = fun(Query) ->
        Offset   = 0,
        PageSize = 10,
        RecList  = ?SRV:query_page(Server, Offset, PageSize, Query, Meta),
%       io:format(user, "~n~p~n~p~n", [Query, RecList]),
        RecList
        end,

    ?FORALL({Parser, Query}, 
            {valid_query_parser(xapian_proper:x_query_parser()), oneof(Terms)},
        begin
            QS = #x_query_string{parser=Parser, value=Query},
            equals([#document{docid = DocId}], F(QS))
        end).



%% Standard QueryParser must be cloned, not just assigned.
clone_query_parser_test() ->
    Path   = testdb_path(clone_prop_parser),
    Params = [write, create, overwrite],

    {ok, Server} = ?SRV:start_link(Path, Params),
    %% Can't use add_prefix() and add_boolean_prefix() on the same field name.
    Author  = #x_prefix_name{name = author, prefix = $A, is_boolean = true},
    User    = #x_prefix_name{name = author, prefix = $U, is_boolean = false},
    Parser1 = #x_query_parser{prefixes = [Author]},
    Parser2 = #x_query_parser{prefixes = [User]},
    Query1 = #x_query_string{parser=Parser1, value = "sphinx"},
    Query2 = #x_query_string{parser=Parser2, value = "sphinx"},
    Enq1 = ?SRV:enquire(Server, Query1),
    ?SRV:release_resource(Server, Enq1),
    %% Here can be an exception
    Enq2 = ?SRV:enquire(Server, Query2),
    ?SRV:release_resource(Server, Enq2).


query_parser_and_exclusive_boolean_test() ->
    Path   = testdb_path(ex_prop_parser),
    Params = [write, create, overwrite],

    {ok, Server} = ?SRV:start_link(Path, Params),
    User    = #x_prefix_name{prefix = $U, is_boolean = true,
                             name = user, is_exclusive = true},
    Author  = User#x_prefix_name{name = author, is_exclusive = false},
    Parser  = #x_query_parser{prefixes = [Author, User]},
    Query   = #x_query_string{parser=Parser, value = "sphinx"},
    Enq     = ?SRV:enquire(Server, Query),
    ?SRV:release_resource(Server, Enq).


%% Same name, different is_exclusive == Error
query_parser_and_exclusive_boolean_error_test() ->
    Path   = testdb_path(ex_prop_parser_err),
    Params = [write, create, overwrite],

    {ok, Server} = ?SRV:start_link(Path, Params),
    User    = #x_prefix_name{name = author, is_boolean = true,
                             prefix = $U, is_exclusive = true},
    Author  = User#x_prefix_name{prefix = $A, is_exclusive = false},
    Parser  = #x_query_parser{prefixes = [Author, User]},
    Query   = #x_query_string{parser=Parser, value = "sphinx"},
    ?assertError(#x_error{type = <<"InvalidOperationError">>}, 
                 ?SRV:enquire(Server, Query)).


%% @doc Return a proper generator for the x_query_parser() type with 
%%      fixed values.
valid_query_parser(Gen) ->
    ?SUCHTHAT(X, Gen, is_valid_query_parser(X)).


is_valid_query_parser(#x_query_parser{prefixes=Prefixes}) ->
    check_prefixes(Prefixes).


check_prefixes(Prefixes) when is_list(Prefixes) ->
    are_well_formed_prefixes(Prefixes)
        andalso are_valid_prefixes(Prefixes).


are_well_formed_prefixes(Prefixes) ->
    lists:all(fun check_prefix/1, Prefixes).


check_prefix(#x_prefix_name{name = Name, prefix = Prefix}) ->
    is_valid_prefix_long_name(Name) andalso is_valid_prefix_short_name(Prefix).


%% @doc Check a name pseudonym for Erlang.
is_valid_prefix_long_name(Name) when is_atom(Name) ->
    is_valid_prefix_long_name(atom_to_list(Name));

is_valid_prefix_long_name(Name) ->
    is_valid_non_empty_unicode_string(Name).


%% @doc Check a name id for Xapian (used in C++).
is_valid_prefix_short_name(Char) when is_integer(Char) ->
    is_valid_unicode_char(Char);

is_valid_prefix_short_name(Prefix) ->
    is_valid_non_empty_unicode_string(Prefix).


%% Return true, if this true:
%% Long names can used only with boolean or normal term.
%%
%% If this function works bad, then error occures <<"InvalidOperationError">>:
%% <<"Can't use add_prefix() and add_boolean_prefix() on the same field name, 
%%   or add_boolean_prefix() with different values of the 'exclusive' parameter">>
are_valid_prefixes([_,_|_] = Prefixes) -> 
    %% GROUP BY name
    KeyMaker = fun(#x_prefix_name{name = Name}) -> to_unicode_binary(Name) end,
    GroupsAndKeys = lists2:group_with(KeyMaker, Prefixes),
    Groups = lists2:keys(2, GroupsAndKeys),
    %% true, when each Prefix is used only with one type of the term.
    lists:all(fun is_only_with_one_type/1, Groups)
        andalso lists:all(fun is_same_exclusive_value/1, Groups);

are_valid_prefixes(_NotEnoughPrefixes) -> 
    true.


to_unicode_binary(A) when is_atom(A) ->
    to_unicode_binary(atom_to_list(A));

to_unicode_binary(S) ->
    unicode:characters_to_binary(S).


is_valid_operation(Server, Query) ->
    try
        ?SRV:enquire(Server, Query),
        true
    catch error:#x_error{type = <<"InvalidOperationError">>} ->
        false
    end.


are_really_valid_prefixes(Server, Prefixes) ->
    try
        Parser = #x_query_parser{prefixes = Prefixes},
        Query = #x_query_string{value = "don't care", parser = Parser},
        ?SRV:enquire(Server, Query),
        true
    catch error:#x_error{type = <<"InvalidOperationError">>} ->
        false
    end.


are_really_valid_prefixes_gen() ->
    U = #x_prefix_name{name = user, prefix = $A, 
                       is_boolean = true, is_exclusive = false},
    A = U#x_prefix_name{name = author},

    Ps1 = [ A#x_prefix_name{}, 
            A#x_prefix_name{is_boolean = false}],
    Ps2 = [ A#x_prefix_name{}, 
            A#x_prefix_name{is_exclusive = true} ],
    Path = testdb_path(are_really_valid_prefixes),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    IsPs1Valid = are_really_valid_prefixes(Server, Ps1),
    IsPs2Valid = are_really_valid_prefixes(Server, Ps2),
    ?SRV:close(Server),
    [ ?_assertNot(IsPs1Valid) 
    , ?_assertNot(IsPs2Valid) 
    ].


are_valid_prefixes_test_() -> 
    F = fun are_valid_prefixes/1,
    U = #x_prefix_name{name = user, prefix = $A, 
                       is_boolean = true, is_exclusive = false},
    A = U#x_prefix_name{name = author},

    [{"One value list produces always true."
    , [ ?_assert(F([ U#x_prefix_name{}  ]))
      , ?_assert(F([ A#x_prefix_name{} ]))
      , ?_assert(F([ U#x_prefix_name{is_boolean = false}  ]))
      , ?_assert(F([ A#x_prefix_name{is_boolean = false} ]))]}
    , {"With different names (author and user)"
    , [ ?_assert(F([ U#x_prefix_name{}, A#x_prefix_name{} ]))
      , ?_assert(F([ U#x_prefix_name{is_boolean = false}
                   , A#x_prefix_name{is_boolean = false} ]))
      , ?_assert(F([ U#x_prefix_name{}
                   , A#x_prefix_name{is_boolean = false} ]))]}
    , {"With same name (author)"
      , ?_assertNot(F([ A#x_prefix_name{}, 
                        A#x_prefix_name{is_boolean = false}]))}

    , {"Boolean and exclusive."
    , [ ?_assert(F([ U#x_prefix_name{}, A#x_prefix_name{}  ]))
      , ?_assert(F([ U#x_prefix_name{is_exclusive = true}, 
                     A#x_prefix_name{is_exclusive = true} ]))
      , ?_assert(F([ U#x_prefix_name{is_exclusive = true}, 
                     A#x_prefix_name{} ]))
      , {"Same name, different is_exclusive == Error"
        , ?_assertNot(F([ A#x_prefix_name{}, 
                          A#x_prefix_name{is_exclusive = true} ]))}

      , {"Other",
         ?_assertNot(F([ #x_prefix_name{name=a,    prefix="z", is_boolean=false},
                         #x_prefix_name{name=[$a], prefix="z", is_boolean=true} ]))}
      ]}
    ].


%% Prefixes are prefixes with same Name.
is_only_with_one_type(Prefixes) ->
    lists:all(fun is_boolean_prefix/1, Prefixes) orelse   
        lists:all(fun is_normal_prefix/1, Prefixes).


%% For booleans with same name.
is_same_exclusive_value(Prefixes) ->
    IsTrue  = at_position_hof(#x_prefix_name.is_exclusive, true),
    IsFalse = at_position_hof(#x_prefix_name.is_exclusive, false),
    %% Are they not booleans?
    lists:all(fun is_normal_prefix/1, Prefixes) orelse
        lists:all(IsTrue, Prefixes) orelse
            lists:all(IsFalse, Prefixes).


at_position_hof(Pos, Val) ->
    fun(Tuple) -> erlang:element(Pos, Tuple) =:= Val end.


is_boolean_prefix(#x_prefix_name{is_boolean = X}) ->
    X.


is_normal_prefix(#x_prefix_name{is_boolean = X}) ->
    not X.




is_valid_non_empty_unicode_string(Str) ->
    try
        Bin = unicode:characters_to_binary(Str),
        Chars = unicode:characters_to_list(Str),
        is_binary(Bin) andalso is_list(Chars) andalso are_valid_chars(Chars)
    catch error:badarg ->
        false
    end.


are_valid_chars(Chars) ->
    ValidChars = [C || C <- Chars, C =/= 0],
    ValidChars =/= [] andalso ValidChars =:= Chars.


is_valid_unicode_char(Char) when is_integer(Char) ->
    is_valid_non_empty_unicode_string([Char]).



multi_sub_db_id_to_name(Id) when is_integer(Id) -> 
    list_to_existing_atom("multi" ++ integer_to_list(Id));

multi_sub_db_id_to_name(Name) when is_atom(Name) -> 
    Name.


multi_sub_db_name_to_id(Id) when is_integer(Id) -> 
    Id;

multi_sub_db_name_to_id(Name) when is_atom(Name) -> 
    "multi" ++ IdList = atom_to_list(Name),
    list_to_integer(IdList).


%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_gen() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{constraint_tries, 500}]),
    erlang:group_leader(EunitLeader, self()),
    ?_assertEqual([], Res). 


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



%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

multidb_record_by_id(Server, Query, Id) ->
    Table = mset_table(Server, Query, mdocument),
    qlc:e(qlc:q([X || X=#mdocument{multi_docid=DocId} <- Table, Id =:= DocId])).


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
