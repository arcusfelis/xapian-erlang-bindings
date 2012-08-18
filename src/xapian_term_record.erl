%% It contains helpers for extracting.
-module(xapian_term_record).
-export([record/2, 
        encoder/2,
        encode/2, 
        decode/2, 
        decode_list/2, 
        decode_list2/2,
        decode_list3/2]).
-export([key_position/1,
         field_position_to_name/2,
         fix_spy_meta/3]).

-compile({parse_transform, seqbind}).
-record(rec, {name, fields}).
-import(xapian_common, [ 
    append_uint8/2,
    read_document_count/1,
    read_term_count/1,
    read_string/1,
    read_double/1,
    read_position_list/1,
    read_uint8/1,
    index_one_of/2]).

-import(xapian_const, [term_field_id/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------


%% Lookup encoder appends terms for searching.
%% See `Driver::qlcTermIteratorLookup'.
encoder(Values = [_|_], value) ->
    fun(Bin@) -> 
            Bin@ = append_type(value, Bin@),
            xapian_common:append_terms(Values, Bin@)
        end;

encoder(Values = [_|_], float_value) ->
    fun(Bin@) -> 
            Bin@ = append_type(float_value, Bin@),
            xapian_common:append_floats(Values, Bin@)
        end.


%% @doc If the type of the value is `float', then replace `value' field's name
%% on `float_value'.
fix_spy_meta(Server, SpyRes, Meta) ->
    #rec{fields=TupleFields} = Meta,
    HasValueField = lists:member(value, TupleFields),
    case HasValueField of
        true ->
            Slot = xapian_server:match_spy_info(Server, SpyRes, value_slot),
            ValueType  = xapian_server:slot_to_type(Server, Slot),
            case ValueType of
                string -> Meta;
                float  -> Meta#rec{fields = [value_to_float_value(X) 
                                                || X <- TupleFields]}
            end;
        false -> Meta
    end.


value_to_float_value(value) -> float_value;
value_to_float_value(Field) -> Field.


%% @doc Create a record with information about the record for term QLC talbe.
%% You can use special names for fields:
%% 
%% <ul> <li>
%% wdf
%% </li><li>
%% freq
%% </li><li>
%% value
%% </li><li>
%% positions
%% </li></ul>
%%
%% Only `freq' and `value' can be used with `Xapian::ValueCountMatchSpy'.
record(TupleName, TupleFields) ->
    #rec{name=TupleName, fields=TupleFields}.


key_position(#rec{fields=TupleFields}) ->
    case index_one_of([value, float_value], TupleFields) of
    not_found -> undefined;
    I -> I + 1 %% First field is a tuple name, add it (qlc uses a tuple index).
    end.


%% `KeyPos' is a num, such as `{1, 2, 3, 4}' for fields of record 
%% `{TupleName, F1, F2, F3}' when `TupleFields' are `[F1, F2, F3]'.
field_position_to_name(#rec{fields=TupleFields}, KeyPos) 
    when is_integer(KeyPos) ->
    PosWithoutRecordName = KeyPos-1, 
    elem_at(TupleFields, PosWithoutRecordName);

field_position_to_name(#rec{}, undefined) ->
    undefined.


elem_at([_|T], N) when N > 1 -> 
    elem_at(T, N-1);

elem_at([H|_], 1) -> 
    H.


-ifdef(TEST).

elem_at_test_() ->
    [ ?_assertEqual(elem_at([a,b,c], 1), a)
    , ?_assertEqual(elem_at([a,b,c], 2), b)
    , ?_assertEqual(elem_at([a,b,c], 3), c)
    ].

-endif.


%% Creates tuples {Name, Field1, ....}
encode(Meta, Bin) ->
    #rec{fields=TupleFields} = Meta,
    enc(TupleFields, Bin).


-spec decode(term(), binary()) -> {term(), binary()}.

decode(Meta, Bin) ->
    #rec{name=TupleName, fields=TupleFields} = Meta,
    dec(TupleFields, Bin, [TupleName]).


%% The count is known.
decode_list(Meta, Bin@) ->
    {Count, Bin@} = read_document_count(Bin@),
    decode_cycle(Count, Meta, Bin@, []).


%% This encoding schema is used when a total size is unknown.
decode_list2(Meta, Bin) ->
    decode_list2(Meta, Bin, []).


%% The count can be known or not.
decode_list3(Meta, Bin@) ->
    %% see QlcTable::UNKNOWN_SIZE (0), KNOWN_SIZE (1)
    {Flag, Bin@} = read_uint8(Bin@),
    %% Select an encoding  schema
    case Flag of
        0 -> 
            decode_list2(Meta, Bin@);
        1 -> 
            decode_list(Meta, Bin@)
    end.


decode_list2(Meta, Bin@, Acc) ->
    {Flag, Bin@} = read_uint8(Bin@),
    %% see QlcTable::MORE and QlcTable::STOP 
    %%  and XapianErlangDriver::qlcTermIteratorLookup
    case Flag of
        1 -> 
            {Rec, Bin@} = decode(Meta, Bin@),
            decode_list2(Meta, Bin@, [Rec|Acc]);
        0 -> 
            {lists:reverse(Acc), Bin@}
    end.


decode_cycle(0, _Meta, Bin, Acc) ->
    {lists:reverse(Acc), Bin};

decode_cycle(Count, Meta, Bin@, Acc) 
    when Count > 0 ->
    {Rec, Bin@} = decode(Meta, Bin@),
    decode_cycle(Count-1, Meta, Bin@, [Rec|Acc]).



%% ------------------------------------------------------------------
%% Encode data helpers (Bin will be passed into a port)
%% ------------------------------------------------------------------

enc(Parts, Bin) ->
    append_type(stop, 
        lists:foldl(fun append_type/2, Bin, Parts)).


append_type(Type, Bin) ->
    append_uint8(term_field_id(Type), Bin).


%% ------------------------------------------------------------------
%% Decode data helpers (Bin is readed from a port)
%% ------------------------------------------------------------------

dec([H|T], Bin, Acc) ->
    {Val, NewBin} =
        case H of
            freq        -> read_document_count(Bin);    
            wdf         -> read_term_count(Bin); 
            value       -> read_string(Bin);
            float_value -> read_double(Bin);
            positions   -> read_position_list(Bin);
            position_count -> read_term_count(Bin)
        end,
    dec(T, NewBin, [Val|Acc]);

dec([], Rem, Acc) -> 
    {erlang:list_to_tuple(lists:reverse(Acc)), Rem}.
