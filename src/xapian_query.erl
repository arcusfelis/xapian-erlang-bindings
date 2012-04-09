-module(xapian_query).
-export([encode/3]).

-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).

operator_id('AND')          -> 1;
operator_id('OR')           -> 2;
operator_id('AND NOT')      -> 3;
operator_id('XOR')          -> 4;
operator_id('AND MAYBE')    -> 5;
operator_id('FILTER')       -> 6;
operator_id('NEAR')         -> 7;
operator_id('PHRASE')       -> 8;
operator_id('VALUE RANGE')  -> 9;
operator_id('SCALE WEIGHT') -> 10;
operator_id('ELITE SET')    -> 11;
operator_id('VALUE GE')     -> 12;
operator_id('VALUE LE')     -> 13;
operator_id('SYNONYM')      -> 14.


query_id(query_group)       -> 1;
query_id(query_value)       -> 2;
query_id(query_value_range) -> 3;
query_id(query_term)        -> 4.


encode(#x_query{op=Op, value=Value, parameter=Param}, N2S, Bin@) ->
    Bin@ = append_type(query_group, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(Param, Bin@),
    %% Defines when to stop.
    SubQueryCount = erlang:length(Value),
    Bin@ = append_uint(SubQueryCount, Bin@),
    lists:foldl(fun(Rec, Acc) -> encode(Rec, N2S, Acc) end, Bin@, Value);

encode(#x_query_value{op=Op, slot=Slot, value=Value}, N2S, Bin@) ->
    Bin@ = append_type(query_value, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_slot(Slot, N2S, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@;

encode(#x_query_value_range{op=Op, slot=Slot, from=From, to=To}, N2S, Bin@) ->
    Bin@ = append_type(query_value_range, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_slot(Slot, N2S, Bin@),
    Bin@ = append_iolist(From, Bin@),
    Bin@ = append_iolist(To, Bin@),
    Bin@;

encode(#x_query_term{name=Name, wqf=WQF, position=Pos}, _N2S, Bin@) ->
    Bin@ = append_type(query_term, Bin@),
    Bin@ = append_iolist(Name, Bin@),
    Bin@ = append_uint(WQF, Bin@),
    Bin@ = append_uint(Pos, Bin@),
    Bin@;

encode(Term, N2S, Bin) ->
    encode(#x_query_term{name=Term}, N2S, Bin).


append_slot(Slot, N2S, Bin) ->
    append_uint(slot_id(Slot, N2S), Bin).


slot_id(Name, N2S) when is_atom(Name) -> 
    orddict:fetch(Name, N2S);

slot_id(Slot, _N2S) when is_integer(Slot) -> 
    Slot.


append_type(Type, Bin) ->
    append_uint8(query_id(Type), Bin).


append_operator(Op, Bin) ->
    append_uint8(operator_id(Op), Bin).


append_uint8(Value, Bin) ->
    <<Bin/binary, Value:8/native-unsigned-integer>>.


%% Append iolist as a string
append_iolist(Str, Bin) ->
    StrBin = erlang:iolist_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


append_uint(Value, Bin) ->
    <<Bin/binary, Value:32/native-unsigned-integer>>.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Result = encode(#x_query{value=["test1", <<"test2">>]}, [], <<>>),
    io:write(user, Result).

-endif.

