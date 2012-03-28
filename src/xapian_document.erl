-module(xapian_document).
-export([encode/1]).
-export([test/0]).

-include_lib("xapian/include/xapian.hrl").

part_id(stemmer)    -> 0;
part_id(data)       -> 1;
part_id(value)      -> 2;
part_id(delta)      -> 3;
part_id(text)       -> 4;
part_id(term)       -> 5;
part_id(posting)    -> 6.


-spec encode([xapian:x_document_index_part()]) -> binary().
encode(List) ->
    enc(List, <<>>).


enc([], Bin) -> Bin;

enc([#x_stemmer{language=Language}|T], Bin) ->
    enc(T, append_stemmer(Language, Bin));

enc([#x_data{value=Value}|T], Bin) ->
    enc(T, append_data(Value, Bin));

enc([#x_term{value=Value, position=Pos, wdf=WDF}|T], Bin) ->
    enc(T, append_term(Value, Pos, WDF, Bin));

enc([#x_value{slot=Slot, value=Value}|T], Bin) ->
    enc(T, append_value(Slot, Value, Bin));

enc([#x_delta{position=Pos}|T], Bin) ->
    enc(T, append_delta(Pos, Bin));

enc([#x_text{value=Value, position=Pos, prefix=Prefix}|T], Bin) ->
    enc(T, append_text(Value, Pos, Prefix, Bin)).

    

append_stemmer(Language, Bin) ->
    append_iolist(Language, append_type(stemmer, Bin)).


append_data(Value, Bin) ->
    append_iolist(Value, append_type(data, Bin)).


append_term(Value, _Pos = undefined, WDF, Bin) ->
    Bin1 = append_type(term, Bin),
    Bin2 = append_iolist(Value, Bin1),
    append_int(WDF, Bin2);

append_term(Value, Pos, WDF, Bin) ->
    Bin1 = append_type(posting, Bin),
    Bin2 = append_iolist(Value, Bin1),
    Bin3 = append_int(Pos, Bin2),
    append_int(WDF, Bin3).


append_value(Slot, Value, Bin) ->
    Bin1 = append_type(value, Bin),
    Bin2 = append_int(Slot, Bin1),
    append_iolist(Value, Bin2).


append_delta(Pos, Bin) ->
    append_int(Pos, append_type(delta, Bin)).


append_text(Value, Pos, Prefix, Bin) ->
    Bin1 = append_type(term, Bin),
    Bin2 = append_iolist(Value, Bin1),
    Bin3 = append_int(Pos, Bin2),
    append_iolist(Prefix, Bin3).


append_type(Type, Bin) ->
    PartId = part_id(Type),  
    <<Bin/binary, PartId:8/native-signed-integer>>.


%% Append iolist as a string
append_iolist(Str, Bin) ->
    StrBin = erlang:iolist_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


append_int(Num, Bin) ->
    <<Bin/binary, Num:32/native-signed-integer>>.


test() ->
    encode([ #x_stemmer{language = <<"english">>}
           , #x_data{value = "My test data as iolist"} 
           , #x_term{value = "Simple term"} 
           , #x_value{slot = 0, value = "Slot #0"} 
           , #x_text{value = "Paragraph 1"} 
           , #x_delta{}
           , #x_text{value = "Paragraph 2"} 
           ]).
