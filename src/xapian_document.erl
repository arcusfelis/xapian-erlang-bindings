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
    append_int8/2]).

part_id(stop)       -> 0;
part_id(stemmer)    -> 1;
part_id(data)       -> 2;
part_id(value)      -> 3;
part_id(delta)      -> 4;
part_id(text)       -> 5;
part_id(term)       -> 6;
part_id(posting)    -> 7.


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

enc([#x_term{value = Value, position = Pos, wdf = WDF}|T], Bin) ->
    enc(T, append_term(Value, Pos, WDF, Bin));

enc([#x_value{slot = Slot, value = Value}|T], Bin) ->
    enc(T, append_value(Slot, Value, Bin));

enc([#x_delta{position = Pos}|T], Bin) ->
    enc(T, append_delta(Pos, Bin));

enc([#x_text{value = Value, position = Pos, prefix = Prefix}|T], Bin) ->
    enc(T, append_text(Value, Pos, Prefix, Bin)).

    
append_stop(Bin) ->
    append_type(stop, Bin).


append_stemmer(Stemmer, Bin) ->
    xapian_encode:append_stemmer(Stemmer, append_type(stemmer, Bin)).


append_data(Value, Bin) ->
    append_iolist(Value, append_type(data, Bin)).


append_term(Value, _Pos = undefined, WDF, Bin@) ->
    Bin@ = append_type(term, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@ = append_int(WDF, Bin@),
    Bin@;

append_term(Value, Pos, WDF, Bin@) ->
    Bin@ = append_type(posting, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@ = append_int(Pos, Bin@),
    Bin@ = append_int(WDF, Bin@),
    Bin@.


append_value(Slot, Value, Bin@) ->
    Bin@ = append_type(value, Bin@),
    Bin@ = append_uint(Slot, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@.


append_delta(Pos, Bin) ->
    append_int(Pos, append_type(delta, Bin)).


append_text(Value, Pos, Prefix, Bin@) ->
    Bin@ = append_type(text, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@ = append_int(Pos, Bin@),
    Bin@ = append_iolist(Prefix, Bin@),
    Bin@.


%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

append_type(Type, Bin) ->
    append_int8(part_id(Type), Bin).




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
