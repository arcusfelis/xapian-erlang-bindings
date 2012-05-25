%% It contains helpers for extracting information from a document.
-module(xapian_record).
-export([record/2, 
         encode/2, 
         encode/3, 
         decode/3, 
         decode_list/3, 
         decode_list2/3]).
-export([key_position/2,
         append_key_field/2,
         tuple/1]).

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
    read_percent/1,
    read_uint8/1,
    index_of/2]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc You can use special names for fields:
%% 
%% * docid
%% * data
%% * weight
%% * rank
%% * percent
record(TupleName, TupleFields) ->
    #rec{name=TupleName, fields=TupleFields}.


key_position(#rec{fields=TupleFields}, Field) ->
    case index_of(Field, TupleFields) of
    not_found -> undefined;
    I -> I + 1
    end.


tuple(#rec{name=TupleName, fields=TupleFields}) ->
    list_to_tuple([TupleName | TupleFields]).


append_key_field(Key, Bin) ->
    append_uint8(part_id(Key), Bin).


%% Creates tuples {Name, Field1, ....}
encode(Meta, Name2Slot) ->
    encode(Meta, Name2Slot, <<>>).

encode(Meta, Name2Slot, Bin) ->
    #rec{name=_TupleName, fields=TupleFields} = Meta,
    enc(TupleFields, Name2Slot, 
        append_uint8(encoder_type_id(TupleFields), Bin)).


%% Select from few encoders: some of them need Document, MsetIterator or both.
encoder_type_id([_|_] = TupleFields) ->
    case type(TupleFields) of
        {true, false} ->
            type_id(document);

        {true, true} ->
            type_id(both);

        _Other ->
            type_id(iterator)
    end.

type_id(document) ->    0;
type_id(iterator) ->    1;
type_id(both) ->        2.


-spec decode(term(), orddict:orddict(), binary()) -> {term(), binary()}.

decode(Meta, I2N, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    dec(TupleFields, I2N, Bin, [TupleName]).


decode_list(Meta, I2N, Bin@) ->
    {Count, Bin@} = read_doccount(Bin@),
    decode_cycle(Count, Meta, I2N, Bin@, []).


decode_cycle(0, _Meta, _I2N, Bin, Acc) ->
    {lists:reverse(Acc), Bin};

decode_cycle(Count, Meta, I2N, Bin@, Acc) 
    when Count > 0 ->
    {Rec, Bin@} = decode(Meta, I2N, Bin@),
    decode_cycle(Count-1, Meta, I2N, Bin@, [Rec|Acc]).



%% This encoding schema is used when a total size is unknown.
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

type([H  | T],  _IsDoc, IsIter) 
    when H =:= data ->
    type(T, true, IsIter);

type([H  | T],  IsDoc, _IsIter) 
    when H =:= weight; H =:= rank; H =:= percent; H =:= db_number; 
         H =:= db_name; H =:= multi_docid ->
    type(T, IsDoc, true);

%% Can be both
type([docid | T], IsDoc, IsIter) ->
    type(T, IsDoc, IsIter);

%% Value?
type([_H | T], _IsDoc, IsIter) ->
    type(T, true, IsIter);

type([], IsDoc, IsIter) ->
    {IsDoc, IsIter}.


enc([H  | T], N2S, Bin) 
    when H =:= data; H =:= docid; H =:= weight; H =:= rank; H =:= percent;
         H =:= multi_docid; H =:= db_number; H =:= db_name ->
    enc(T, N2S, append_type(H, Bin));

enc([Name | T], N2S, Bin) ->
    Slot = orddict:fetch(Name, N2S),
    enc(T, N2S, append_value(Slot, append_type(value, Bin)));

enc([], _N2S, Bin) -> 
    append_type(stop, Bin).


append_type(Type, Bin) ->
    append_uint8(part_id(Type), Bin).

append_value(Slot, Bin) ->
    append_uint(Slot, Bin).

   
part_id(stop)        -> 0;
part_id(value)       -> 1;
part_id(data)        -> 2;
part_id(docid)       -> 3;
part_id(weight)      -> 4;
part_id(rank)        -> 5;
part_id(percent)     -> 6;
part_id(multi_docid) -> 7;
part_id(db_number)   -> 8;
part_id(db_name)     -> part_id(db_number).




%% ------------------------------------------------------------------
%% Decode data helpers (Bin is readed from a port)
%% ------------------------------------------------------------------

dec([H|T], I2N, Bin, Acc) ->
    {Val, NewBin} =
        case H of
            docid       -> read_document_id(Bin);
            weight      -> read_weight(Bin);  % double
            rank        -> read_rank(Bin);    % unsigned
            percent     -> read_percent(Bin); % int -> uint8_t
            multi_docid -> read_document_id(Bin);
            db_name     -> read_db_name(Bin, I2N);
            db_number   -> read_db_id(Bin);
            _ -> read_string(Bin)
        end,
    dec(T, I2N, NewBin, [Val|Acc]);

dec([], _I2N, Rem, Acc) -> 
    {erlang:list_to_tuple(lists:reverse(Acc)), Rem}.


read_db_name(Bin, I2N) ->
    {Id, NewBin} = read_db_id(Bin),
    {db_id_to_name(Id, I2N), NewBin}.

db_id_to_name(Id, I2N) ->
    get_tuple_value(Id, I2N, Id).


get_tuple_value(Key, Tuple, Def) ->
    case erlang:element(Key+1, Tuple) of
        undefined -> Def;
        Val -> Val
    end.
