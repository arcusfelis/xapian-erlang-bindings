-module(xapian_common).
-compile(export_all).


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


%% Encode to unsigned int32_t (for example, it is Xapian::valueno)
append_uint(Value, Bin) ->
    <<Bin/binary, Value:32/native-unsigned-integer>>.


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


read_document_id(Bin) ->
    read_uint(Bin).


read_document_count(Bin) ->
    read_uint(Bin).


read_document_length(Bin) ->
    read_double(Bin).


read_term_count(Bin) ->
    read_uint(Bin).


append_slot(Slot, N2S, Bin) ->
    append_uint(slot_id(Slot, N2S), Bin).


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
