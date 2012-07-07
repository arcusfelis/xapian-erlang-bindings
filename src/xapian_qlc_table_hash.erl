%% @doc This structure is for converting beetween qlc_reference and table_hash.
-module(xapian_qlc_table_hash).

%% Helpers.
-export([hash/1]).

%% Functions for working with store.
-export([new/0,
         get/2,
         erase/2,
         put/3]).

-record(qlc_table_hash_register, {
        hash_to_ref = gb_trees:empty() 
            :: gb_trees:gb_tree(table_hash(), qlc_reference()),
        ref_to_hash = gb_trees:empty()
            :: gb_trees:gb_tree(qlc_reference(), table_hash())
}).

-type qlc_reference() :: reference().
-type table_hash() :: non_neg_integer().
-type hr_store() :: #qlc_table_hash_register{}.
-type hr_key() :: qlc_reference() | table_hash().
-type hr_error() :: elem_not_found.

-type hr_read_result() :: {ok, {qlc_reference(), table_hash()}}
                        | {error, hr_error()}.

-type hr_write_result() :: {ok, {hr_store(), qlc_reference(), table_hash()}} 
                        | {error, hr_error()}.


-spec hash(term()) -> table_hash().
hash(Table) ->
    erlang:phash2(Table).


-spec new() -> hr_store().

new() ->
    #qlc_table_hash_register{}.


-spec put(hr_store(), qlc_reference(), table_hash()) -> hr_store().

put(Store, Ref, Hash)
    when is_reference(Ref), is_integer(Hash) ->
    #qlc_table_hash_register{hash_to_ref = H2R, ref_to_hash = R2H} = Store,
    NewR2H = gb_trees:insert(Ref, Hash, R2H),
    NewH2R = gb_trees:insert(Hash, Ref, H2R),
    Store#qlc_table_hash_register{hash_to_ref = NewH2R, ref_to_hash = NewR2H}.


-spec erase(hr_store(), hr_key()) -> hr_write_result().
erase(Store, Key) ->
    #qlc_table_hash_register{hash_to_ref = H2R, ref_to_hash = R2H} = Store,
    case key_to_ref_and_hash(Store, Key) of
    {ok, {Ref, Hash}} ->
        NewR2H = gb_trees:delete(Ref, R2H),
        NewH2R = gb_trees:delete(Hash, H2R),
        NewStore = Store#qlc_table_hash_register{hash_to_ref = NewH2R, 
                                                 ref_to_hash = NewR2H},
        {ok, {NewStore, Ref, Hash}};

    {error, _Reason} = Error ->
        Error
    end.



-spec get(hr_store(), hr_key()) -> hr_read_result().

get(Store, Key) ->
    key_to_ref_and_hash(Store, Key).


%% @doc Key can be a hash or a reference.
-spec key_to_ref_and_hash(hr_store(), hr_key()) -> hr_read_result().

key_to_ref_and_hash(Store, Key) ->
    #qlc_table_hash_register{hash_to_ref = H2R, ref_to_hash = R2H} = Store,
    if
        is_reference(Key) ->
            Ref = Key,
            case gb_trees:is_defined(Ref, R2H) of
            true ->
                Hash = gb_trees:get(Ref, R2H),
                {ok, {Ref, Hash}};
            false ->
                {error, elem_not_found}
            end;

        is_integer(Key) ->
            Hash = Key,
            case gb_trees:is_defined(Hash, H2R) of
            true ->
                Ref = gb_trees:get(Hash, H2R),
                {ok, {Ref, Hash}};
            false ->
                {error, elem_not_found}
            end
    end.
