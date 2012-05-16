%% It contains helpers for inserting.
-module(xapian_document).

%% Internal functions
-export([encode/3]).


-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_common, [ 
    append_iolist/2,
    append_uint/2,
    append_int/2,
    append_uint8/2,
    append_boolean/2]).

part_id(stop)       -> 0;
part_id(stemmer)    -> 1;
part_id(data)       -> 2;
part_id(delta)      -> 3;
part_id(text)       -> 4;

part_id(set_posting)       -> 15;
part_id(add_posting)       -> 25;
part_id(update_posting)    -> 35;
part_id(remove_posting)    -> 45;

part_id(set_term)          -> 16;
part_id(add_term)          -> 26;
part_id(update_term)       -> 36;
part_id(remove_term)       -> 46;

part_id(add_value)         -> 17;
part_id(set_value)         -> 27;
part_id(update_value)      -> 37;
part_id(remove_value)      -> 47;

part_id(dec_wdf)                   -> 101;
part_id(set_wdf)                   -> 111;
part_id(remove_values)             -> 103;
part_id(remove_terms)              -> 104;
part_id(remove_positions)          -> 105;
part_id(remove_term_positions)     -> 106;
part_id(remove_term_positions_save)-> 116.


value_type(add)       -> add_value;
value_type(set)       -> set_value;
value_type(update)    -> update_value;
value_type(remove)    -> remove_value.


posting_type(add)     -> add_posting;
posting_type(set)     -> set_posting;
posting_type(update)  -> update_posting;
posting_type(remove)  -> remove_posting.


term_type(add)     -> add_term;
term_type(set)     -> set_term;
term_type(update)  -> update_term;
term_type(remove)  -> remove_term.


%% @doc Encode parts of the document to a binary.
-spec encode([xapian:x_document_index_part()], 
        orddict:orddict(), orddict:orddict()) -> binary().

encode(List, Name2Prefix, Name2Slot) ->
    Pre = preprocess_hof(Name2Prefix, Name2Slot),
    List2 = [Pre(X) || X <- List], % lists:map(Pre, List)
    enc(List2, <<>>).


%% @doc Replace all pseudonames on real values.
preprocess_hof(Name2Prefix, Name2Slot) ->
    fun(Rec=#x_value{slot = Name}) when is_atom(Name) ->
            %% Set real slot id (integer) for values.
            %% Throw an error if there is no this name.
            Rec#x_value{slot = orddict:fetch(Name, Name2Slot)};

       (Rec=#x_text{prefix = Name}) ->
            %% Set short version for prefixes.
            %% Left as is if there is no this name.
            case orddict:find(Name, Name2Prefix) of
                {ok, Prefix} -> Rec#x_text{prefix = Prefix};
                error -> Rec
            end;

        %% Skip all other parts
        (Rec) -> Rec
        end.


%% @doc Build a binary from a list of parts.
enc([], Bin) -> append_stop(Bin);

enc([#x_stemmer{}=Stemmer|T], Bin) ->
    enc(T, append_stemmer(Stemmer, Bin));

enc([#x_data{value = Value}|T], Bin) ->
    enc(T, append_data(Value, Bin));

enc([#x_term{position = [HT|TT] = Positions} = Rec|T], Bin) ->
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
    Bin@ = append_iolist(Value, Bin@),
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
    append_uint8(part_id(Type), Bin).




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
           ], [], []).

-endif.
