%%% @doc This module contains functions, which are used by other modules.
-module(xapian_common).
-compile({parse_transform, seqbind}).


%% Basic decoding functions
-export([read_string/1,
         read_uint8/1,
         read_uint/1,
         read_double/1,
         read_boolean/1
        ]).

%% Basic encoding functions
-export([append_iolist/2,
         append_string/2,
         append_int/2,
         append_int8/2,
         append_uint/2,
         append_uint8/2,
         append_uint16/2,
         append_double/2,
         append_boolean/2,
         append_binary/2,
         append_weight/2,
         append_percent/2
        ]).

%% Advanced decoding functions
-export([read_document_id/1,
         read_document_count/1,
         read_document_length/1,
         read_doccount/1,
         read_position_list/1,
         read_unknown_type_value/1,
         read_term_count/1,
         read_rank/1,
         read_slot/1,
         read_weight/1,
         read_percent/1,
         read_db_id/1,
         read_maybe/2
        ]).

%% Advanced encoding functions
-export([append_document_id/2,
         append_docids/2,
         append_unique_document_id/2,
         append_slot/2,
         append_slot/3,
         append_slots/3,
         append_slots_with_order/3,
         append_value/3,
         append_value/4,
         append_terms/2,
         append_floats/2,
         append_stop/1,
         append_param/2,
         append_flags/2
        ]).

%% Other functions
-export([string_to_binary/1,
         index_of/2,
         index_one_of/2,
         slot_id/2, 
         slot_type/2, 
         resource_appender/2,
         append_resource/3,
         append_resource/4,
         resource_reader/2,
         resource_register/1,
         read_resource/2
        ]).


-include("xapian.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



%% @doc If the value is defined, then read it with `Fn' function.
%% Otherwise, the value is `undefined'.
-spec read_maybe(Fn, Bin) -> {Result, Bin} | {undefined, Bin} when
    Fn     :: fun((Bin) -> Result),
    Bin    :: binary(),
    Result :: term().
read_maybe(Fn, Bin) ->
    {IsDefined, Bin2} = read_uint8(Bin),
    case IsDefined of
        0 -> {undefined, Bin2}; 
        1 -> Fn(Bin2)
    end.


%% Note: Order of the functions:
%% * Append "something";
%% * Read "something".

%% @doc Split the binary on a string and a tail.
-spec read_string(Bin) -> {Str, Bin} when
    Bin :: binary(),
    Str :: xapian_type:x_string().
read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    <<Str:Num/binary, Bin3/binary>> = Bin2,
    {Str, Bin3}.


%% @doc Run `read_string'. Return `undefined' for empty strings.
save_read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    case Num of
        0 -> {undefined, Bin2};
        _ ->
            <<Str:Num/binary, Bin3/binary>> = Bin2,
            {Str, Bin3}
    end.

%% @doc Convert characters into a binary.
-spec string_to_binary(Str) -> Bin when
    Str :: unicode:chardata(),
    Bin :: binary().
string_to_binary(Str) ->
    StrBin = unicode:characters_to_binary(Str),
    [ erlang:error({not_unicode, StrBin}) || not is_binary(StrBin) ],
    StrBin.


%% @doc Append iolist as a list of bytes.
append_iolist(Str, Bin) ->
    StrBin = erlang:iolist_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


%% @doc Append characters as an unicode string.
append_string(Str, Bin) ->
    StrBin = string_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.

%% @doc Append non-empty string or throw an `string_is_empty' error.
append_non_empty_string(Str, Bin) ->
    StrBin = string_to_binary(Str),
    [erlang:error(string_is_empty) || Str =:= <<>>],
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


%% @doc Encode to unsigned `int32_t' (for example, it is `Xapian::valueno').
append_uint(Value, Bin) when is_integer(Value), is_binary(Bin) ->
    <<Bin/binary, Value:32/native-unsigned-integer>>.


%% @doc Encode to unsigned `int16_t' (for example, it is a tcp port).
append_uint16(Value, Bin) when is_integer(Value), is_binary(Bin) ->
    <<Bin/binary, Value:16/native-unsigned-integer>>.

%% @doc Read an unsigned integer `int32_t'.
read_uint(Bin) ->
    <<Value:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Value, Bin2}.


%% @doc Append a unsigned integer `int8_t'.
append_int8(Num, Bin) ->
    <<Bin/binary, Num:8/native-signed-integer>>.


%% @doc Append a signed integer `int32_t'.
append_int(Num, Bin) ->
    <<Bin/binary, Num:32/native-signed-integer>>.


%% @doc Append a signed integer `uint8_t'.
append_uint8(Value, Bin) ->
    <<Bin/binary, Value:8/native-unsigned-integer>>.


%% @doc Append a parameter or a field as `uint8_t'.
%% It is used primarily for encoding command types, that are handled inside 
%% `switch' constructings on the C++ side.
append_param(0, _Bin) ->
    erlang:error(bad_field);
append_param(Value, Bin) ->
    append_uint8(Value, Bin).


%% @doc Append flags, used in `XapianErlangDriver::decodeParserFeatureFlags'.
append_flags(Flags, Bin) ->
    FlagsBin = << <<X/native-signed-integer>> || X <- Flags, X =/= 0 >>,
    <<Bin/binary, FlagsBin/binary, 0>>.

%% @doc Append a special parameter, which shows the end of the list of commands.
%% Append 0, because it is easy to handle it inside the `while' construction.
append_stop(Bin) ->
    <<Bin/binary, 0>>.


% @doc Append an unsigned byte (`uint8_t').
read_uint8(Bin) ->
    <<Value:8/native-unsigned-integer, Bin2/binary>> = Bin,  
    {Value, Bin2}.


%% @doc Append a document identifier (as `uint32_t').
append_document_id(Id, Bin) ->
    append_uint(Id, Bin).


%% @doc Appends a document id or a unique term.
%% Add a prefix tag (1,2) also.
append_unique_document_id(Id, Bin) when is_integer(Id) ->
    append_document_id(Id, append_uint8(1, Bin));

append_unique_document_id(Term, Bin) ->
    append_iolist(Term, append_uint8(2, Bin)).


%% @doc Zero id will be replaced by `undefined'.
read_document_id(Bin) ->
    case read_uint(Bin) of
        {0, Bin1} -> {undefined, Bin1};
        Other -> Other
    end.


%% @doc Read a database number (uint32_t).
read_db_id(Bin) ->
    read_uint(Bin).


%% @doc Read a count of documents (uint32_t).
read_document_count(Bin) ->
    read_uint(Bin).


%% @doc Read a document length (double).
%% This value is used as, for example, 
%% "an average length of document in the list".
read_document_length(Bin) ->
    read_double(Bin).


%% @doc Read a term count (uint32_t).
read_term_count(Bin) ->
    read_uint(Bin).


%% @doc Read a term position (uint32_t, used with `#x_term.position').
read_term_position(Bin) ->
    read_uint(Bin).


%% @doc Append a value slot (uint32_t).
%% Aliases are allowed (when `Slot' is an atom).
append_slot(Slot, N2S, Bin) ->
    append_uint(slot_id(Slot, N2S), Bin).


%% @doc Append a value slot (uint32_t).
%% Aliases are not allowed (when `Slot' is an atom).
append_slot(Slot, Bin) ->
    append_uint(Slot, Bin).


%% @doc Append a list of slots.
append_slots(N2S, Slots, Bin) ->
    Values = [ slot_id(Slot, N2S) || Slot <- Slots ],
    SlotsBin = 
    <<  <<Value:32/native-unsigned-integer>>  || Value <- Values >>,
    BadValue = bad_value(),
    <<Bin/binary, SlotsBin/binary, BadValue/binary>>.


%% @doc Append a list of slots. `{reverse, Slot}' is allowed.
append_slots_with_order(N2S, Slots, Bin) ->
    ValueAndOrders = [ slot_id_and_order(Slot, N2S) || Slot <- Slots ],
    SlotsBin = 
    <<  <<Value:32/native-unsigned-integer,
           Order:8/native-unsigned-integer>>  
            || {Value, Order} <- ValueAndOrders >>,
    BadValue = bad_value(),
    <<Bin/binary, SlotsBin/binary, BadValue/binary>>.


slot_id_and_order({reverse, Value}, N2S) ->
    {slot_id(Value, N2S), 1}; %% reverse = true
slot_id_and_order(Value, N2S) ->
    {slot_id(Value, N2S), 0}. %% reverse = false (default)


%% @doc Append the maximum `uint32_t'.
bad_value() ->     
    <<(bnot 0):32>>.


%% @doc Append a float.
append_double(Value, Bin) ->
    %% `native-float' is `double' from C++.
    <<Bin/binary, Value/native-float>>.


%% @doc Read a float.
read_double(Bin) ->
    %% `native-float' is `double' from C++.
    <<W/native-float, Bin2/binary>> = Bin,  
    {W, Bin2}.

%% @doc Read a float, don't crush, if this float is a malformed.
save_read_double(Bin) ->
    %% `native-float' is `double' from C++.
    try
        read_double(Bin)
    catch error:{badmatch, _Bin} ->
        %% It is `sizeof(double)' in C.
        BitCount = bit_size(<<0/native-float>>),
        <<_Skip:BitCount, Bin2/binary>> = Bin,  
        {undefined, Bin2}
    end.


%% @doc Concat 2 binaries in the reverse order.
append_binary(Value, Bin) ->
    <<Bin/binary, Value/binary>>.


%% @doc Append a bool.
append_boolean(Value, Bin) ->
    append_uint8(boolean_to_integer(Value), Bin).


boolean_to_integer(false) -> 0;
boolean_to_integer(true) ->  1.


%% @doc Read a bool. Return `true' or `false'.
-spec read_boolean(Bin) -> {Bool, Bin} when
    Bin :: binary(),
    Bool :: boolean().
read_boolean(Bin) ->
    case read_uint8(Bin) of
        {1, RemBin} -> {true, RemBin};
        {0, RemBin} -> {false, RemBin}
    end.


%% @doc Read weight as a double.
read_weight(Bin) ->
    read_double(Bin).

%% @doc Write weight as a double.
append_weight(Value, Bin) ->
    append_double(Value, Bin).

%% @doc Read a document rank (uint32_t).
read_rank(Bin) ->
    read_uint(Bin).

%% @doc Read a value slot (uint32_t).
read_slot(Bin) ->
    read_uint(Bin).

%% @doc Read a percent value (uint8_t).
-spec read_percent(Bin) -> Percent when
    Bin :: binary(),
    Percent :: 0 .. 100.
read_percent(Bin) ->
    read_uint8(Bin).

%% @doc Write percent as a double.
append_percent(Value, Bin) ->
    append_uint8(Value, Bin).

%% @doc Read a document count (uint32_t).
read_doccount(Bin) ->
    read_uint(Bin).


%% @doc Convert slot name to its number.
slot_id(Name, N2S) when is_atom(Name) -> 
    orddict:fetch(Name, N2S);

slot_id(Slot, _N2S) when is_integer(Slot) -> 
    Slot.

%% @doc Convert a slot number (not name) into a slot type (`float' or `string').
-spec slot_type(Slot, N2S | undefined) -> Type when
    Slot :: non_neg_integer(),
    N2S  :: term(),
    Type :: xapian_type:x_slot_type().
slot_type(_Slot, undefined) -> 
    string;

slot_type(Slot, N2S) when is_integer(Slot) -> 
    case array:get(Slot, N2S) of
        undefined -> string;
        Type -> Type
    end.


%% @doc Return a position of the element `Item' in the list `List'.
-spec index_of(Item, List) -> Pos | not_found when
    Pos :: non_neg_integer(),
    Item :: term(),
    List :: [Item].
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


%% @doc Search any term from `Items' inside `List'. 
%% Return its position in `List'.
-spec index_one_of([term()], [term()]) -> non_neg_integer() | not_found.

index_one_of(Items, List) -> index_one_of(Items, List, 1).

index_one_of(_, [], _)  -> not_found;
index_one_of(Items, [H|T], Index) -> 
    case lists:member(H, Items) of
        true -> Index;
        false -> index_one_of(Items, T, Index+1)
    end.


-ifdef(TEST).

index_of_test_() ->
    [ ?_assertEqual(index_of(1, [1,2,3]), 1)
    , ?_assertEqual(index_of(c, [a,b,c]), 3)
    , ?_assertEqual(index_of(x, [a,b,c]), not_found)
    ].

index_one_of_test_() ->
    [ ?_assertEqual(index_one_of([1], [1,2,3]), 1)
    , ?_assertEqual(index_one_of([2, 1], [1,2,3]), 1)
    , ?_assertEqual(index_one_of([3, 2], [1,2,3]), 2)
    , ?_assertEqual(index_one_of([c], [a,b,c]), 3)
    , ?_assertEqual(index_one_of([x], [a,b,c]), not_found)
    ].

-endif.


%% @doc Read a fixed count of positions from `Bin'.
-spec read_position_list(Bin) -> {Positions, Bin} when
    Positions :: [Pos],
    Pos :: non_neg_integer(),
    Bin :: binary().
read_position_list(Bin@) ->
    {Count, Bin@} = read_uint(Bin@),
    read_position_list(Count, [], Bin@).


read_position_list(Count, Acc, Bin@) when Count > 0 ->
    {Pos, Bin@} = read_term_position(Bin@),
    read_position_list(Count - 1, [Pos|Acc], Bin@);

read_position_list(0, Acc, Bin@) ->
    {lists:reverse(Acc), Bin@}.
    

%% @doc Append a list of document identifiers.
append_docids(DocIds, Bin@) ->
    Bin@ = lists:foldl(fun append_document_id/2, Bin@, DocIds),
    Bin@ = append_document_id(0, Bin@),
    Bin@.


%% @doc Append a list of terms.
-spec append_terms(Terms, Bin) -> Bin when
    Terms :: [xapian_type:x_non_empty_string()],
    Bin :: binary().
append_terms(Terms, Bin@) ->
    Bin@ = lists:foldl(fun append_non_empty_string/2, Bin@, Terms),
    Bin@ = append_iolist("", Bin@),
    Bin@.


%% @doc Append a list of double numbers.
-spec append_floats(Nums, Bin) -> Bin when
    Nums :: [float()],
    Bin :: binary().
append_floats(Nums, Bin@) ->
    Bin@ = append_uint(length(Nums), Bin@),
    Bin@ = lists:foldl(fun append_double/2, Bin@, Nums),
    Bin@.


%% @doc Append a value of the slot. 
%% `Type' is a user defined type (for example, `string', `float' or `bytes').
%% See `XapianErlang::Driver::decodeValue'.
-spec append_value(Type, Value, Bin) -> Bin when
    Type :: atom(),
    Value :: float(),
    Bin :: binary().
append_value(string, Value, Bin) ->
    append_string(Value, append_uint8(xapian_const:value_type_id(string), Bin));

append_value(float, Value, Bin) when is_number(Value) ->
    append_double(Value, append_uint8(xapian_const:value_type_id(double), Bin)); 

append_value(bytes, Value, Bin) ->
    append_iolist(Value, append_uint8(xapian_const:value_type_id(string), Bin)).


%% @doc Append a value `Value' of the slot `SlotId'.
append_value(SlotId, Value, undefined, Bin) when is_integer(SlotId) ->
    %% All values are strings.
    append_value(string, Value, Bin);

append_value(SlotId, Value, S2T, Bin) when is_integer(SlotId) ->
    ExpectedType = array:get(SlotId, S2T),
    append_value(ExpectedType, Value, Bin).


%% @doc Read a value from the binary.
-spec read_value(Type, Bin) -> {Value, Bin} when
    Bin   :: binary(),
    Type  :: atom(),
    Value :: float() | xapian_type:x_string() | undefined.
read_value(string, Bin) ->
    save_read_string(Bin);

read_value(double, Bin) ->
    save_read_double(Bin).


read_unknown_type_value(Bin1) ->
    {Type, Bin2} = read_uint8(Bin1), 
    read_value(xapian_const:value_type_name(Type), Bin2).


%% It is a black box.
-type x_resource_appender() :: {xapian_type:x_state(), pid()}.

-spec resource_appender(State, Client) -> x_resource_appender() when
    State :: xapian_type:x_state(),
    Client :: pid() | {pid(), reference()}.

%% @doc Create a resource appender.
resource_appender(State, {ClientPid, _ClientRef}) ->
    {State, ClientPid};
resource_appender(State, ClientPid) ->
    {State, ClientPid}.


%% @doc Encode a resource `Res' using the resource appender `RA'.
%% Concat it with `Bin'.
%% @see resource_appender/2
%% @see append_resource/4
-spec append_resource(RA, Res, Bin) -> Bin when
    RA :: x_resource_appender(),
    Res :: xapian_type:x_resource(),
    Bin :: binary().

append_resource({State, ClientPid} = _RA, Res, Bin) ->
    append_resource(State, Res, Bin, ClientPid).


%% @doc Append a resource reference or constructor.
%% @see append_resource/3
append_resource(State, Res, Bin, ClientPid) ->
    {ok, F} = xapian_server:internal_compile_resource(State, Res, ClientPid),
    F(Bin).


%% Resource Register State
%% Used, while decoding.
-record(rr_state, {client, register}).

%% @doc Create a resource reader.
resource_reader(Register, ClientPid) ->
    #rr_state{client = ClientPid, register = Register}.

resource_register(#rr_state{register = Register}) ->
    Register.


%% @doc Decode a resource from `Bin' using a resource reader `RR'.
%% @see resource_reader/2
read_resource(RR = #rr_state{client = ClientPid, register = Register}, Bin) ->
    {ResNum, Bin2} = read_uint(Bin),
    {ok, {NewRegister, ResRef}} 
        = xapian_register:put(Register, ClientPid, ResNum),
    RR2 = RR#rr_state{register = NewRegister},
    {ResRef, RR2, Bin2}.
