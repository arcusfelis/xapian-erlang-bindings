%% It contains helpers for extracting.
-module(xapian_term_record).
-export([record/2, encode/1, encode/2, decode/2, decode_list/2]).
-export([key_position/1]).

-compile({parse_transform, seqbind}).
-record(rec, {name, fields}).
-import(xapian_common, [ 
    append_uint8/2,
    append_iolist/2,
    read_document_count/1,
    read_term_count/1,
    read_string/1,
    read_position_list/1,
    index_of/2]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc You can use special names for fields:
%% 
%% * wdf
%% * freq
%% * value
%% * positions
record(TupleName, TupleFields) ->
    #rec{name=TupleName, fields=TupleFields}.


key_position(#rec{fields=TupleFields}) ->
    case index_of(value, TupleFields) of
    I -> I + 1;
    not_found -> undefined
    end.


%% Creates tuples {Name, Field1, ....}
encode(Meta) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    enc(TupleFields, <<>>).

encode(Meta, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    enc(TupleFields, Bin).


-spec decode(term(), binary()) -> {term(), binary()}.

decode(Meta, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    dec(TupleFields, Bin, [TupleName]).


decode_list(Meta, Bin@) ->
    {Count, Bin@} = read_document_count(Bin@),
    decode_cycle(Count, Meta, Bin@, []).


decode_cycle(0, _Meta, Bin, Acc) ->
    {lists:reverse(Acc), Bin};

decode_cycle(Count, Meta, Bin@, Acc) 
    when Count > 0 ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    {Rec, Bin@} = decode(Meta, Bin@),
    decode_cycle(Count-1, Meta, Bin@, [Rec|Acc]).



%% ------------------------------------------------------------------
%% Encode data helpers (Bin will be passed into a port)
%% ------------------------------------------------------------------

enc(Parts, Bin) ->
    append_type(stop, 
        lists:foldl(fun append_type/2, Bin, Parts)).


append_type(Type, Bin) ->
    append_uint8(part_id(Type), Bin).

   
part_id(stop)       -> 0;
part_id(value)      -> 1;
part_id(wdf)        -> 2;
part_id(freq)       -> 3;
part_id(positions)  -> 4;
part_id(position_count) -> 5.



%% ------------------------------------------------------------------
%% Decode data helpers (Bin is readed from a port)
%% ------------------------------------------------------------------

dec([H|T], Bin, Acc) ->
    {Val, NewBin} =
        case H of
            freq      -> read_document_count(Bin);    
            wdf       -> read_term_count(Bin); 
            value     -> read_string(Bin);
            positions -> read_position_list(Bin);
            position_count -> read_term_count(Bin)
        end,
    dec(T, NewBin, [Val|Acc]);

dec([], Rem, Acc) -> 
    {erlang:list_to_tuple(lists:reverse(Acc)), Rem}.
