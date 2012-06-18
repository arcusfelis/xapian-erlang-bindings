%% It contains helpers for inserting.
-module(xapian_document).

%% Internal functions
-export([encode/4]).


-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_common, [ 
    append_iolist/2,
    append_uint/2,
    append_int/2,
    append_uint8/2,
    append_boolean/2,
    slot_id/2,
    append_value/2]).

-import(xapian_const, [ 
    term_type/1,
    posting_type/1,
    value_type/1,
    document_part_id/1]).


%% @doc Encode parts of the document to a binary.
-spec encode([xapian:x_document_index_part()], 
             orddict:orddict(), orddict:orddict(), array()) -> binary().

encode(List, Name2Prefix, Name2Slot, Slot2TypeArray) ->
    Pre = preprocess_hof(Name2Prefix, Name2Slot, Slot2TypeArray),
    List2 = [Pre(X) || X <- List], % lists:map(Pre, List)
    enc(List2, <<>>).


%% @doc Replace all pseudonames on real values.
preprocess_hof(Name2Prefix, Name2Slot, Slot2TypeArray) ->
    fun(Rec=#x_value{}) ->
            %% NOTE: Name can be a number
            #x_value{slot = Name, value=Value} = Rec, 
            %% Convert the name into the slot id (integer number).
            SlotId = slot_id(Name, Name2Slot),
            %% Throw an error if there is no this name inside `Slot2TypeArray' 
            %% __and__ `Value' is a number.
            CheckedValue = xapian_common:fix_value(SlotId, Value, Slot2TypeArray),
            Rec#x_value{slot = SlotId, value=CheckedValue};

       (Rec=#x_text{}) ->
            #x_text{prefix = Name} = Rec,
            %% Set short version for prefixes.
            %% Left as is if there is no this name.
            case orddict:find(Name, Name2Prefix) of
                {ok, Prefix} -> Rec#x_text{prefix = Prefix};
                error -> Rec
            end;

        %% Skip all other parts
        (Rec) -> Rec
        end.


fix_float(Rec, undefined) -> 
    Rec;

fix_float(Rec=#x_value{slot = Slot, value = Value}, Slot2TypeArray) 
    when is_number(Value) ->
    Rec;

fix_float(Rec, _Slot2TypeArray) ->
    Rec.


%% @doc Build a binary from a list of parts.
enc([], Bin) -> append_stop(Bin);

enc([#x_stemmer{}=Stemmer|T], Bin) ->
    enc(T, append_stemmer(Stemmer, Bin));

enc([#x_data{value = Value}|T], Bin) ->
    enc(T, append_data(Value, Bin));

enc([#x_term{position = [HT|TT] = _Positions} = Rec|T], Bin) ->
    enc([Rec#x_term{position = HT}, 
         Rec#x_term{position = TT} | T], Bin);

enc([#x_term{} = H|T], Bin) ->
    #x_term{
        action = Action, 
        value = Value, 
        position = Pos,
        frequency = WDF, 
        ignore = Ignore} = H,
    enc(T, append_posting(Action, Value, Pos, WDF, Ignore, Bin));

enc([#x_value{} = H|T], Bin) ->
    #x_value{
        action = Action, 
        slot = Slot, 
        value = Value, 
        ignore = Ignore} = H,
    enc(T, append_value(Action, Slot, Value, Ignore, Bin));

enc([#x_delta{position = Pos}|T], Bin) ->
    enc(T, append_delta(Pos, Bin));

enc([#x_text{value = Value, frequency = WDF, prefix = Prefix}|T], Bin) ->
    enc(T, append_text(Value, WDF, Prefix, Bin)).

    
append_stop(Bin) ->
    append_type(stop, Bin).


append_stemmer(Stemmer, Bin) ->
    xapian_encode:append_stemmer(Stemmer, append_type(stemmer, Bin)).


append_data(Value, Bin) ->
    append_iolist(Value, append_type(data, Bin)).


append_term(Action, Value, WDF, Ignore, Bin@) ->
    Bin@ = append_type(term_type(Action), Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@ = append_int(WDF, Bin@),
    Bin@ = append_boolean(Ignore, Bin@),
    Bin@.


append_posting(Action, Value, Pos, WDF, Ignore, Bin) 
    when is_integer(WDF), WDF < 0 ->
    append_decrease_wdf(Value, -WDF, Ignore,
        append_posting(Action, Value, Pos, 0, Ignore, Bin));

append_posting(Action, Value, Pos, {abs, WDF}, Ignore, Bin) ->
    append_set_wdf(Value, WDF, Ignore,
        append_posting(Action, Value, Pos, 0, Ignore, Bin));

append_posting(Action, Value, Pos, {cur, WDF}, Ignore, Bin) ->
    append_posting(Action, Value, Pos, WDF, Ignore, Bin);

append_posting(Action, Value, [], WDF, Ignore, Bin) ->
    append_term(Action, Value, WDF, Ignore, Bin);

append_posting(Action, Value, undefined, WDF, Ignore, Bin) ->
    append_term(Action, Value, WDF, Ignore, Bin);

append_posting(Action, Value, Pos, WDF, Ignore, Bin@) ->
    Bin@ = append_type(posting_type(Action), Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@ = append_int(Pos, Bin@),
    Bin@ = append_int(WDF, Bin@),
    Bin@ = append_boolean(Ignore, Bin@),
    Bin@.


append_set_wdf(Value, WDF, Ignore, Bin@) ->
    append_term_wdf(set_wdf, Value, WDF, Ignore, Bin@).


append_decrease_wdf(Value, WDF, Ignore, Bin@) ->
    append_term_wdf(dec_wdf, Value, WDF, Ignore, Bin@).


append_term_wdf(Type, Value, WDF, Ignore, Bin@) ->
    Bin@ = append_type(Type, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@ = append_int(WDF, Bin@),
    Bin@ = append_boolean(Ignore, Bin@),
    Bin@.


append_value(Action, Slot, Value, Ignore, Bin@) ->
    Bin@ = append_type(value_type(Action), Bin@),
    Bin@ = append_uint(Slot, Bin@),
    Bin@ = append_value(Value, Bin@),
    Bin@ = append_boolean(Ignore, Bin@),
    Bin@.


append_delta(Pos, Bin) ->
    append_int(Pos, append_type(delta, Bin)).


append_text(Value, WDF, Prefix, Bin@) ->
    Bin@ = append_type(text, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@ = append_int(WDF, Bin@),
    Bin@ = append_iolist(Prefix, Bin@),
    Bin@.


%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

append_type(Type, Bin) ->
    append_uint8(document_part_id(Type), Bin).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    encode([ #x_stemmer{language = <<"english">>}
           , #x_data{value = "My test data as iolist"} 
           , #x_term{value = "Simple term"} 
           , #x_value{slot = 0, value = "Slot #0"} 
           , #x_text{value = "Paragraph 1"} 
           , #x_delta{}
           , #x_text{value = <<"Paragraph 2">>} 
           ], [], [], undefined).

-endif.
