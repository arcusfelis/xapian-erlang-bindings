%%% @doc It contains helpers for extracting information from a document.
%%% It is used with MSet.
-module(xapian_record).
-export([record/2, 
         encode/4, 
         decode/3, 
         decode_list/3, 
         decode_list2/3]).
-export([key_position/2,
         append_key_field/2,
         tuple/1]).

-compile({parse_transform, gin}).
-compile({parse_transform, seqbind}).
-record(rec, {name, fields}).
-import(xapian_common, [ 
        append_uint/2,
        append_uint8/2,
        read_doccount/1,
        read_document_id/1,
        read_db_id/1,
        read_weight/1,
        read_rank/1,
        read_string/1,
        read_strings/1,
        read_strings_and_positions/1,
        read_slot_and_values/1,
        read_percent/1,
        read_uint8/1,
        read_unknown_type_value/1,
        slot_type/2,
        index_of/2]).

-import(xapian_const, [
        source_type_id/1, 
        document_field_id/1, 
        slot_type_to_command_type/1]).

-type x_document_meta() :: term().


%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc Create a term, that contains information about document fields.
%% This information is used for building a record term for each document in
%% the MSet.
%% `TupleFields' is a list of special names for the record fields:
%% 
%% <ul><li>
%% </li><li> docid
%% </li><li> data
%% </li><li> all_values
%% </li><li> all_terms
%% </li><li> all_terms_pos
%% </li><li> weight
%% </li><li> rank
%% </li><li> percent
%% </li><li> collapse_key
%% </li><li> collapse_count
%% </li></ul>
-spec record(TupleName, TupleFields) -> Meta when
    TupleName :: atom(),
    TupleFields :: [atom()],
    Meta :: x_document_meta().

record(TupleName, TupleFields) ->
    #rec{name=TupleName, fields=TupleFields}.


%% @doc Return an index of the `Field' or `undefined' if there is no a key.
-spec key_position(Meta, Field) -> Pos when
    Meta :: x_document_meta(),
    Field :: atom(),
    Pos :: non_neg_integer() | undefined.
key_position(#rec{fields=TupleFields}, Field) ->
    case index_of(Field, TupleFields) of
    not_found -> undefined;
    I -> I + 1
    end.

%% @doc Convert `Meta' into a tuple, where fields are fields' names.
-spec tuple(Meta) -> Rec when
    Meta :: x_document_meta(),
    Rec :: record().
tuple(#rec{name=TupleName, fields=TupleFields}) ->
    list_to_tuple([TupleName | TupleFields]).


%% Append information about fields to `Bin'.
encode(Meta, Name2Slot, Value2TypeArray, Bin) ->
    #rec{name=_TupleName, fields=TupleFields} = Meta,
    enc(TupleFields, Name2Slot, Value2TypeArray, 
        append_uint8(encoder_source_type_id(TupleFields), Bin)).


encoder_source_type_id(TupleFields) ->
    source_type_id(encoder_source_type(TupleFields)).


%% Select from few encoders: some of them need Document, MsetIterator or both.
encoder_source_type([_|_] = TupleFields) ->
    case type(TupleFields) of
        {true, false}   -> document;
        {true, true}    -> both;
        _Other          -> iterator
    end.


%% @doc Read a record from binary.
-spec decode(term(), orddict:orddict(), binary()) -> {term(), binary()}.

decode(Meta, I2N, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    dec(TupleFields, I2N, Bin, [TupleName]).


%% @doc Read a list of records from a binary.
decode_list(Meta, I2N, Bin@) ->
    {Count, Bin@} = read_doccount(Bin@),
    decode_cycle(Count, Meta, I2N, Bin@, []).


decode_cycle(0, _Meta, _I2N, Bin, Acc) ->
    {lists:reverse(Acc), Bin};

decode_cycle(Count, Meta, I2N, Bin@, Acc) 
    when Count > 0 ->
    {Rec, Bin@} = decode(Meta, I2N, Bin@),
    decode_cycle(Count-1, Meta, I2N, Bin@, [Rec|Acc]).



%% @doc This encoding schema is used when a total size is unknown.
decode_list2(Meta, I2N, Bin) ->
    decode_list2(Meta, I2N, Bin, []).


decode_list2(Meta, I2N, Bin@, Acc) ->
    {Flag, Bin@} = read_uint8(Bin@),
    case Flag of
        1 -> 
            {Rec, Bin@} = decode(Meta, I2N, Bin@),
            decode_list2(Meta, I2N, Bin@, [Rec|Acc]);
        0 -> 
            {lists:reverse(Acc), Bin@}
    end.



%% ------------------------------------------------------------------
%% Encode data helpers (Bin will be passed into a port)
%% ------------------------------------------------------------------

%% Return parts of a document to build an answer in form: {bool(), bool()}
type(List) ->
    type(List, false, false).

%% @param List 
%% @param IsDoc 
%% @param IsIter
type(_,  true, true) ->
    {true, true};

%% They are from MsetIterator.
%% `multi_docid' is calculated using dereferencing. 
%% Next fields are calculated from `multi_docid': 
%% * `db_number' => `db_name';
%% * `docid'.
type([H  | T],  IsDoc, _IsIter) 
    when in(H, [weight, rank, percent, db_number, db_name, multi_docid,
                collapse_count, collapse_key]) ->
    type(T, IsDoc, true);

%% Can be both
type([docid | T], IsDoc, IsIter) ->
    type(T, IsDoc, IsIter);

%% Value? `data'?
type([_H | T], _IsDoc, IsIter) ->
    type(T, true, IsIter);

type([], IsDoc, IsIter) ->
    {IsDoc, IsIter}.


%% Encode a field without parameters:
enc([H  | T], N2S, V2T, Bin) 
    when in(H, [data, docid, weight, rank, percent, multi_docid, 
                db_number, db_name, collapse_count, collapse_key,
                all_terms, all_values, all_terms_pos]) ->
    enc(T, N2S, V2T, append_type(H, Bin));

%% ... with a parameter:
enc([Name | T], N2S, V2T, Bin) ->
    SlotNum = orddict:fetch(Name, N2S),
    CmdType = slot_to_command_type(SlotNum, V2T),
    enc(T, N2S, V2T, append_value(SlotNum, append_type(CmdType, Bin)));

enc([], _N2S, _V2T, Bin) -> 
    append_type(stop, Bin).


slot_to_command_type(SlotNum, V2T) ->
    slot_type_to_command_type(slot_type(SlotNum, V2T)).


%% Type of the field
append_type(Type, Bin) ->
    append_uint8(document_field_id(Type), Bin).

%% Type of the key field
append_key_field(Key, Bin) ->
    append_uint8(document_field_id(Key), Bin).

append_value(Slot, Bin) ->
    append_uint(Slot, Bin).



%% ------------------------------------------------------------------
%% Decode data helpers (Bin is readed from a port)
%% ------------------------------------------------------------------

dec([H|T], I2N, Bin, Acc) ->
    {Val, NewBin} =
        case H of
            data             -> read_string(Bin);
            docid            -> read_document_id(Bin);
            weight           -> read_weight(Bin);  % double
            rank             -> read_rank(Bin);    % unsigned
            percent          -> read_percent(Bin); % int -> uint8_t
            collapse_key     -> read_string(Bin); 
            collapse_count   -> read_doccount(Bin); 
            multi_docid      -> read_document_id(Bin);
            db_number        -> read_db_id(Bin);
            db_name          -> read_db_name(Bin, I2N);
            all_terms        -> read_strings(Bin);
            all_terms_pos    -> read_strings_and_positions(Bin);
            all_values       -> read_slot_and_values(Bin);
            _ValueField      -> read_unknown_type_value(Bin)
        end,
    dec(T, I2N, NewBin, [Val|Acc]);

dec([], _I2N, Rem, Acc) -> 
    {erlang:list_to_tuple(lists:reverse(Acc)), Rem}.


read_db_name(Bin, I2N) ->
    {Id, NewBin} = read_db_id(Bin),
    {db_id_to_name(Id, I2N), NewBin}.

db_id_to_name(Id, I2N) ->
    get_tuple_value(Id, I2N, Id).


-spec get_tuple_value(non_neg_integer(), tuple(), term()) -> term().

get_tuple_value(KeyPos, Tuple, Def) 
    when KeyPos > 0, is_integer(KeyPos) ->
    case erlang:element(KeyPos, Tuple) of
        undefined -> Def;
        Val -> Val
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(POOL, ?MODULE).


tuple_test_() ->
    [ ?_assertEqual(tuple(#rec{name=book, fields=[author, docid]}),
                    {book, author, docid})
    ].

get_tuple_value_test_() ->
    [ ?_assertEqual(get_tuple_value(#rec.name, #rec{name=book}, default), book)
    , ?_assertEqual(get_tuple_value(#rec.name, #rec{}, default), default)
    ].

type_test_() ->
    [ ?_assertEqual(encoder_source_type([docid]), iterator)
    , ?_assertEqual(encoder_source_type([docid, data]), document)
    , ?_assertEqual(encoder_source_type([docid, rank]), iterator)
    , ?_assertEqual(encoder_source_type([docid, rank, data]), both)
    ].

-endif.
