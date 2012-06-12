-module(xapian_common).
-compile(export_all).
-compile({parse_transform, seqbind}).


%% Order of functions:
%% * Append
%% * Read

read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    <<Str:Num/binary, Bin3/binary>> = Bin2,
    {Str, Bin3}.


%% Append iolist as a string
append_iolist(Str, Bin) ->
    StrBin = erlang:iolist_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


append_not_empty_iolist(Str, Bin) ->
    StrBin = erlang:iolist_to_binary(Str),
    [erlang:error(iolist_is_empty) || Str =:= <<>>],
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


%% Encode to unsigned int32_t (for example, it is Xapian::valueno)
append_uint(Value, Bin) when is_integer(Value), is_binary(Bin) ->
    <<Bin/binary, Value:32/native-unsigned-integer>>.


%% Encode to unsigned int16_t (for example, it is a tcp port)
append_uint16(Value, Bin) when is_integer(Value), is_binary(Bin) ->
    <<Bin/binary, Value:16/native-unsigned-integer>>.


read_uint(Bin) ->
    <<Value:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Value, Bin2}.


append_int8(Num, Bin) ->
    <<Bin/binary, Num:8/native-signed-integer>>.


append_int(Num, Bin) ->
    <<Bin/binary, Num:32/native-signed-integer>>.


append_uint8(Value, Bin) ->
    <<Bin/binary, Value:8/native-unsigned-integer>>.


read_uint8(Bin) ->
    <<Value:8/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Value, Bin2}.


append_document_id(Id, Bin) ->
    append_uint(Id, Bin).


%% Appends a document id or a unique term.
append_unique_document_id(Id, Bin) when is_integer(Id) ->
    append_document_id(Id, append_uint8(1, Bin));

append_unique_document_id(Term, Bin) ->
    append_iolist(Term, append_uint8(2, Bin)).


%% Zero id will be replaced by `undefined'.
read_document_id(Bin) ->
    case read_uint(Bin) of
        0 -> undefined;
        DocId -> DocId
    end.


read_db_id(Bin) ->
    read_uint(Bin).


read_document_count(Bin) ->
    read_uint(Bin).


read_document_length(Bin) ->
    read_double(Bin).


read_term_count(Bin) ->
    read_uint(Bin).


read_term_position(Bin) ->
    read_uint(Bin).


append_slot(Slot, N2S, Bin) ->
    append_uint(slot_id(Slot, N2S), Bin).


append_slot(Slot, Bin) ->
    append_uint(Slot, Bin).


append_double(Value, Bin) ->
        %% `native-float' is `double' from C++.
    <<Bin/binary, Value/native-float>>.


read_double(Bin) ->
    %% `native-float' is `double' from C++.
    <<W/native-float, Bin2/binary>> = Bin,  
    {W, Bin2}.


%% Append bool
append_boolean(Value, Bin) ->
    append_uint8(boolean_to_integer(Value), Bin).


boolean_to_integer(false) -> 0;
boolean_to_integer(true) ->  1.


read_boolean(Bin) ->
    case read_uint8(Bin) of
        {1, RemBin} -> {true, RemBin};
        {0, RemBin} -> {false, RemBin}
    end.


read_weight(Bin) ->
    read_double(Bin).


read_rank(Bin) ->
    read_uint(Bin).


read_percent(Bin) ->
    read_uint8(Bin).


read_doccount(Bin) ->
    read_uint(Bin).


slot_id(Name, N2S) when is_atom(Name) -> 
    orddict:fetch(Name, N2S);

slot_id(Slot, _N2S) when is_integer(Slot) -> 
    Slot.


index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).



read_position_list(Bin@) ->
    {Count, Bin@} = read_uint(Bin@),
    read_position_list(Count, [], Bin@).


read_position_list(Count, Acc, Bin@) when Count > 0 ->
    {Pos, Bin@} = read_term_position(Bin@),
    read_position_list(Count - 1, [Pos|Acc], Bin@);

read_position_list(0, Acc, Bin@) ->
    {lists:reverse(Acc), Bin@}.
    

append_docids(DocIds, Bin@) ->
    Bin@ = lists:foldl(fun append_document_id/2, Bin@, DocIds),
    Bin@ = append_document_id(0, Bin@),
    Bin@.


append_terms(Terms, Bin@) ->
    Bin@ = lists:foldl(fun append_not_empty_iolist/2, Bin@, Terms),
    Bin@ = append_iolist("", Bin@),
    Bin@.

