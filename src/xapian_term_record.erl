%% It contains helpers for extracting.
-module(xapian_term_record).
-export([record/2, 
        encode/1, 
        encode/2, 
        decode/2, 
        decode_list/2, 
        decode_list2/2,
        decode_list3/2]).
-export([key_position/1]).

-compile({parse_transform, seqbind}).
-record(rec, {name, fields}).
-import(xapian_common, [ 
    append_uint8/2,
    append_iolist/2,
    read_document_count/1,
    read_term_count/1,
    read_string/1,
    read_position_list/1,
    read_uint8/1,
    index_of/2]).

-import(xapian_const, [term_field_id/1]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc You can use special names for fields:
%% 
%% * wdf
%% * freq
%% * value
%% * positions
record(TupleName, TupleFields) ->
    #rec{name=TupleName, fields=TupleFields}.


key_position(#rec{fields=TupleFields}) ->
    case index_of(value, TupleFields) of
    not_found -> undefined;
    I -> I + 1
    end.


%% Creates tuples {Name, Field1, ....}
encode(Meta) ->
    #rec{fields=TupleFields} = Meta,
    enc(TupleFields, <<>>).

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
            freq      -> read_document_count(Bin);    
            wdf       -> read_term_count(Bin); 
            value     -> read_string(Bin);
            positions -> read_position_list(Bin);
            position_count -> read_term_count(Bin)
        end,
    dec(T, NewBin, [Val|Acc]);

dec([], Rem, Acc) -> 
    {erlang:list_to_tuple(lists:reverse(Acc)), Rem}.
