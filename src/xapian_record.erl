%% It contains helpers for extracting.
-module(xapian_record).
-export([record/2, encode/2, decode/2]).

-record(rec, {name, fields}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

record(TupleName, TupleFields) ->
    #rec{name=TupleName, fields=TupleFields}.


%% Creates tuples {Name, Field1, ....}
encode(Meta, Name2Slot) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    enc(TupleFields, Name2Slot, <<>>).


decode(Meta, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    dec(TupleFields, Bin, [TupleName]).



%% ------------------------------------------------------------------
%% Encode data helpers (Bin will be passed into a port)
%% ------------------------------------------------------------------

enc([data  | T], N2S, Bin) ->
    enc(T, N2S, append_type(data, Bin));

enc([docid | T], N2S, Bin) ->
    enc(T, N2S, append_type(docid, Bin));

enc([Name | T], N2S, Bin) ->
    Slot = orddict:fetch(Name, N2S),
    enc(T, N2S, append_value(Slot, append_type(value, Bin)));

enc([], _N2S, Bin) -> 
    append_type(stop, Bin).


append_type(Type, Bin) ->
    PartId = part_id(Type),  
    <<Bin/binary, PartId:8/native-signed-integer>>.

append_value(Slot, Bin) ->
    append_uint(Slot, Bin).

%% Encode to unsigned int32_t (for example, it is Xapian::valueno)
append_uint(Num, Bin) ->
    <<Bin/binary, Num:32/native-unsigned-integer>>.

   
part_id(stop)       -> 0;
part_id(value)      -> 1;
part_id(data)       -> 2;
part_id(docid)      -> 3.





%% ------------------------------------------------------------------
%% Decode data helpers (Bin is readed from a port)
%% ------------------------------------------------------------------

dec([H|T], Bin, Acc) ->
    {Val, NewBin} =
        case H of
            docid -> read_docid(Bin);
            _ -> read_string(Bin)
        end,
    dec(T, NewBin, [Val|Acc]);

dec([], <<>>, Acc) -> 
    erlang:list_to_tuple(lists:reverse(Acc)).


read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    <<Str:Num/binary, Bin3/binary>> = Bin2,
    {Str, Bin3}.


read_docid(Bin) ->
    <<Id:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Id, Bin2}.
