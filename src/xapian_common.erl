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
         append_binary/2
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
         append_value/2,
         append_terms/2,
         append_floats/2,
         append_stop/1,
         append_param/2
        ]).

%% Other functions
-export([string_to_binary/1,
         index_of/2,
         index_one_of/2,
         slot_id/2, 
         slot_type/2, 
         fix_value/3,
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



read_maybe(Fn, Bin) ->
    {IsDefined, Bin2} = read_uint8(Bin),
    case IsDefined of
        0 -> {undefined, Bin2}; 
        1 -> Fn(Bin2)
    end.


%% Order of functions:
%% * Append
%% * Read

read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    <<Str:Num/binary, Bin3/binary>> = Bin2,
    {Str, Bin3}.


save_read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    case Num of
        0 -> {undefined, Bin2};
        _ ->
            <<Str:Num/binary, Bin3/binary>> = Bin2,
            {Str, Bin3}
    end.


string_to_binary(Str) ->
    StrBin = unicode:characters_to_binary(Str),
    [ erlang:error({not_unicode, StrBin}) || not is_binary(StrBin) ],
    StrBin.

%% Append iolist as a string
append_iolist(Str, Bin) ->
    StrBin = erlang:iolist_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


%% Append characters as a string
append_string(Str, Bin) ->
    StrBin = string_to_binary(Str),
    StrLen = erlang:byte_size(StrBin),
    <<Bin/binary, StrLen:32/native-signed-integer, StrBin/binary>>.


append_not_empty_iolist(Str, Bin) ->
    StrBin = string_to_binary(Str),
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


append_param(0, _Bin) ->
    erlang:error(bad_field);
append_param(Value, Bin) ->
    append_uint8(Value, Bin).

append_stop(Bin) ->
    <<Bin/binary, 0>>.

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
        {0, Bin1} -> {undefined, Bin1};
        Other -> Other
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

append_slots(N2S, Slots, Bin) ->
    Values = [ slot_id(Slot, N2S) || Slot <- Slots ],
    SlotsBin = 
    <<  <<Value:32/native-unsigned-integer>>  || Value <- Values >>,
    BadValue = bad_value(),
    <<Bin/binary, SlotsBin/binary, BadValue/binary>>.


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

bad_value() ->      
    <<(bnot 0):32>>.



append_double(Value, Bin) ->
        %% `native-float' is `double' from C++.
    <<Bin/binary, Value/native-float>>.


read_double(Bin) ->
    %% `native-float' is `double' from C++.
    <<W/native-float, Bin2/binary>> = Bin,  
    {W, Bin2}.


save_read_double(Bin) ->
    %% `native-float' is `double' from C++.
    try
        read_double(Bin)
    catch error:{badmatch, _Bin} ->
        BitCount = bit_size(<<0/native-float>>),
        <<_Skip:BitCount, Bin2/binary>> = Bin,  
        {undefined, Bin2}
    end.


%% @doc Concat 2 binaries in the reverse order.
append_binary(Value, Bin) ->
    <<Bin/binary, Value/binary>>.


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


read_slot(Bin) ->
    read_uint(Bin).


read_percent(Bin) ->
    read_uint8(Bin).


read_doccount(Bin) ->
    read_uint(Bin).


%% Convert slot name to number.
slot_id(Name, N2S) when is_atom(Name) -> 
    orddict:fetch(Name, N2S);

slot_id(Slot, _N2S) when is_integer(Slot) -> 
    Slot.


slot_type(_Slot, undefined) -> 
    string;

slot_type(Slot, N2S) when is_integer(Slot) -> 
    case array:get(Slot, N2S) of
        undefined -> string;
        Type -> Type
    end.


index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


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


append_floats(Nums, Bin@) ->
    Bin@ = append_uint(length(Nums), Bin@),
    Bin@ = lists:foldl(fun append_double/2, Bin@, Nums),
    Bin@.


%% see XapianErlang::Driver::decodeValue
append_value(Value, Bin@) when is_number(Value) ->
    append_double(Value, append_uint8(xapian_const:value_type_id(double), Bin@)); 

append_value(Value, Bin@) ->
    append_iolist(Value, append_uint8(xapian_const:value_type_id(string), Bin@)).



%% @doc Returns the fixed value: float or string if all is ok.
%%      Othervise, throws an error.
%% @end 
%% All slots has the string type.
fix_value(_Slot, Value, undefined) ->
    Value;

fix_value(Slot, Value, Slot2TypeArray) 
    when is_number(Value) -> 
    %% The passed value has type `float'.
    %% The field type must be `float' too.
    ExpectedType = array:get(Slot, Slot2TypeArray),
    %% If Type is not float, then throw an error.
    [xapian_error:bad_slot_value_type_error(bad_value, Slot, Value, ExpectedType) 
        || ExpectedType =/= float], 
    %% No errors, continue.
    Value;

fix_value(_Slot, Value, _Slot2TypeArray) ->
    Value.


read_value(string, Bin) ->
    save_read_string(Bin);

read_value(double, Bin) ->
    save_read_double(Bin).


read_unknown_type_value(Bin1) ->
    {Type, Bin2} = read_uint8(Bin1), 
    read_value(value_type(Type), Bin2).


value_type(0) -> string;
value_type(1) -> double.


%% It is a black box.
-type x_resource_appender() :: {xapian_type:x_state(), pid()}.

-spec resource_appender(State, Client) -> x_resource_appender() when
    State :: xapian_type:x_state(),
    Client :: pid() | {pid(), reference()}.

resource_appender(State, {ClientPid, _ClientRef}) ->
    {State, ClientPid};
resource_appender(State, ClientPid) ->
    {State, ClientPid}.


-spec append_resource(RA, Res, Bin) -> Bin when
    RA :: x_resource_appender(),
    Res :: xapian_type:x_resource(),
    Bin :: binary().

append_resource({State, ClientPid} = _RA, Res, Bin) ->
    append_resource(State, Res, Bin, ClientPid).


%% @doc Append a resource reference or constructor.
append_resource(State, Res, Bin, ClientPid) ->
    {ok, F} = xapian_server:compile_resource(State, Res, ClientPid),
    F(Bin).


%% Resource Register State
%% Used, while decoding.
-record(rr_state, {client, register}).
resource_reader(Register, ClientPid) ->
    #rr_state{client = ClientPid, register = Register}.

resource_register(#rr_state{register = Register}) ->
    Register.

read_resource(RR = #rr_state{client = ClientPid, register = Register}, Bin) ->
    {ResNum, Bin2} = read_uint(Bin),
    {ok, {NewRegister, ResRef}} 
        = xapian_register:put(Register, ClientPid, ResNum),
    RR2 = RR#rr_state{register = NewRegister},
    {ResRef, RR2, Bin2}.
