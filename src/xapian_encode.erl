-module(xapian_encode).
-export([ append_stemmer/2
        , append_prefix/2]).
-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).

append_stemmer(#x_stemmer{language=Language}, Bin) ->
    append_iolist(Language, Bin).


%% Prefix must be handled with `xapian_check:check_prefix/1'.
%% @see `XapianErlangDriver::addPrefix'
append_prefix(#x_prefix_name{name=Name, prefix=Prefix, 
    is_boolean=Bool, is_exclusive=Ex, is_default=Def}, Bin@) ->
    Bin@ = append_iolist(prefix_name_to_binary(Name), Bin@),
    Bin@ = append_iolist(Prefix, Bin@),
    Bin@ = append_boolean(Bool, Bin@),
    Bin@ = append_boolean(Ex, Bin@),
    Bin@.


prefix_name_to_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));

prefix_name_to_binary(X) ->
    X.


%% Append iolist as a string
append_iolist(Str, Bin) ->
    StrBin = erlang:iolist_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


append_uint8(Value, Bin) ->
    <<Bin/binary, Value:8/native-unsigned-integer>>.


%% Append bool
append_boolean(Value, Bin) ->
    append_uint8(boolean_to_integer(Value), Bin).


boolean_to_integer(false) -> 0;
boolean_to_integer(true) ->  1.

