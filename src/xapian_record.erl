%% It contains helpers for extracting.
-module(xapian_record).
-export([record/2, encode/2, encode/3, decode/2, decode_list/2, decode_list2/2]).
-export([key_position/1]).

-compile({parse_transform, seqbind}).
-record(rec, {name, fields}).
-import(xapian_common, [ 
    append_uint/2,
    append_uint8/2,
    append_int8/2,
    read_doccount/1,
    read_document_id/1,
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


key_position(#rec{fields=TupleFields}) ->
    case index_of(docid, TupleFields) of
    not_found -> undefined;
    I -> I + 1
    end.


%% Creates tuples {Name, Field1, ....}
encode(Meta, Name2Slot) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    enc(TupleFields, Name2Slot, <<>>).

encode(Meta, Name2Slot, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    enc(TupleFields, Name2Slot, Bin).


-spec decode(term(), binary()) -> {term(), binary()}.

decode(Meta, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    dec(TupleFields, Bin, [TupleName]).


decode_list(Meta, Bin@) ->
    {Count, Bin@} = read_doccount(Bin@),
    decode_cycle(Count, Meta, Bin@, []).


decode_cycle(0, _Meta, Bin, Acc) ->
    {lists:reverse(Acc), Bin};

decode_cycle(Count, Meta, Bin@, Acc) 
    when Count > 0 ->
    {Rec, Bin@} = decode(Meta, Bin@),
    decode_cycle(Count-1, Meta, Bin@, [Rec|Acc]).



%% This encoding schema is used when a total size is unknown.
decode_list2(Meta, Bin) ->
    decode_list2(Meta, Bin, []).


decode_list2(Meta, Bin@, Acc) ->
    {Flag, Bin@} = read_uint8(Bin@),
    case Flag of
        1 -> 
            {Rec, Bin@} = decode(Meta, Bin@),
            decode_list2(Meta, Bin@, [Rec|Acc]);
        0 -> 
            {lists:reverse(Acc), Bin@}
    end.



%% ------------------------------------------------------------------
%% Encode data helpers (Bin will be passed into a port)
%% ------------------------------------------------------------------

enc([H  | T], N2S, Bin) 
    when H =:= data; H =:= docid; H =:= weight; H =:= rank; H =:= percent ->
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

   
part_id(stop)       -> 0;
part_id(value)      -> 1;
part_id(data)       -> 2;
part_id(docid)      -> 3;
part_id(weight)     -> 4;
part_id(rank)       -> 5;
part_id(percent)    -> 6.




%% ------------------------------------------------------------------
%% Decode data helpers (Bin is readed from a port)
%% ------------------------------------------------------------------

dec([H|T], Bin, Acc) ->
    {Val, NewBin} =
        case H of
            docid   -> read_document_id(Bin);
            weight  -> read_weight(Bin);  % double
            rank    -> read_rank(Bin);    % unsigned
            percent -> read_percent(Bin); % int -> uint8_t
            _ -> read_string(Bin)
        end,
    dec(T, NewBin, [Val|Acc]);

dec([], Rem, Acc) -> 
    {erlang:list_to_tuple(lists:reverse(Acc)), Rem}.
