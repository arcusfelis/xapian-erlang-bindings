%% It contains helpers for extracting.
-module(xapian_record).
-export([record/2, encode/2, encode/3, decode/2, decode_list/2]).

-compile({parse_transform, seqbind}).
-record(rec, {name, fields}).

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
    #rec{name=TupleName, fields=TupleFields} = Meta,
    {Rec, Bin@} = decode(Meta, Bin@),
    decode_cycle(Count-1, Meta, Bin@, [Rec|Acc]).



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
            docid   -> read_docid(Bin);
            weight  -> read_weight(Bin);  % double
            rank    -> read_rank(Bin);    % unsigned
            percent -> read_percent(Bin); % int -> uint8_t
            _ -> read_string(Bin)
        end,
    dec(T, NewBin, [Val|Acc]);

dec([], Rem, Acc) -> 
    {erlang:list_to_tuple(lists:reverse(Acc)), Rem}.


%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    <<Str:Num/binary, Bin3/binary>> = Bin2,
    {Str, Bin3}.


read_docid(Bin) ->
    <<Id:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Id, Bin2}.

read_weight(Bin) ->
    %% `native-float' is `double' from C++.
    <<W/native-float, Bin2/binary>> = Bin,  
    {W, Bin2}.

read_rank(Bin) ->
    <<Rank:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Rank, Bin2}.

read_percent(Bin) ->
    <<P:8/native-unsigned-integer, Bin2/binary>> = Bin,  
    {P, Bin2}.


read_doccount(Bin) ->
    <<Count:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Count, Bin2}.
