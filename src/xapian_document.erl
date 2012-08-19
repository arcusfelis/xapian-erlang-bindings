%% It contains helpers for inserting.
-module(xapian_document).

%% Internal functions
-export([encode/5]).


-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, mead}).
-compile({parse_transform, seqbind}).
-import(xapian_common, [ 
    append_iolist/2,
    append_string/2,
    append_slot/2,
    append_int/2,
    append_uint/2,
    append_uint8/2,
    append_boolean/2,
    slot_id/2,
    append_value/4,
    append_stop/1,
    append_param/2,
    append_flags/2]).

-import(xapian_const, [ 
    term_type/1,
    posting_type/1,
    action_type/1,
    document_part_id/1,
    generator_feature_id/1,
    generator_type_id/1,
    generator_command_id/1,
    stem_strategy_id/1
]).


%% @doc Encode parts of the document to a binary.
%% N2P - Name2Prefix, N2S - Name2Slot, S2T - Slot2TypeArray, RA - ResourceAppender.
-spec encode([xapian_type:x_document_index_part()], 
             orddict:orddict(), orddict:orddict(), array(),
             term()) -> binary().

encode(List, N2P, N2S, S2T, RA) ->
    enc(List, N2P, N2S, S2T, RA, <<>>).


%% @doc Build a binary from a list of parts.
enc([], _, _, _, _, Bin) -> append_stop(Bin);

enc([#x_stemmer{}=Stemmer|T], _, _, _, _, Bin) ->
    me(T, _, _, _, _, append_stemmer(Stemmer, Bin));

enc([#x_data{value = Value}|T], _, _, _, _, Bin) ->
    me(T, _, _, _, _, append_data(Value, Bin));

enc([#x_term{position = [HT|TT] = _Positions} = Rec|T], _, _, _, _, _) ->
    me([Rec#x_term{position = HT}, 
        Rec#x_term{position = TT} | T], _, _, _, _, _);

enc([#x_term{} = H|T], _, _, _, _, Bin) ->
    #x_term{
        action = Action, 
        value = Value, 
        position = Pos,
        frequency = WDF, 
        ignore = Ignore} = H,
    me(T, _, _, _, _, append_posting(Action, Value, Pos, WDF, Ignore, Bin));

enc([#x_value{} = H|T], _, N2S, S2T, _, Bin) ->
    #x_value{
        action = Action, 
        slot = Slot, 
        value = Value, 
        ignore = Ignore} = H,
    %% Convert the name into the slot id (integer number).
    SlotId = slot_id(Slot, N2S),
    me(T, _, _, _, _, append_value_rec(Action, SlotId, Value, Ignore, S2T, Bin));

enc([#x_delta{position = Pos}|T], _, _, _, _, Bin) ->
    me(T, _, _, _, _, append_delta(Pos, Bin));

enc([#x_text{value = Value, frequency = WDF, prefix = Prefix, position = Pos,
             features = Features}|T], N2P, _, _, _, Bin) ->
    Prefix2 = replace_prefix_name(Prefix, N2P),
    me(T, _, _, _, _, append_text(Value, WDF, Prefix2, Features, Pos, Bin));

enc([#x_term_generator{}=H|T], _, _, _, RA, Bin@) ->
    Bin@ = append_type(term_generator, Bin@),
    me(T, _, _, _, _, append_generator(H, RA, Bin@)).



append_stemmer(Stemmer, Bin) ->
    xapian_encode:append_stemmer(Stemmer, append_type(stemmer, Bin)).


append_data(Value, Bin) ->
    append_iolist(Value, append_type(data, Bin)).


append_term(Action, Value, WDF, Ignore, Bin@) ->
    Bin@ = append_type(term_type(Action), Bin@),
    Bin@ = append_string(Value, Bin@),
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
    Bin@ = append_string(Value, Bin@),
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
    Bin@ = append_string(Value, Bin@),
    Bin@ = append_int(WDF, Bin@),
    Bin@ = append_boolean(Ignore, Bin@),
    Bin@.


append_value_rec(Action, SlotId, Value, Ignore, S2T, Bin@) ->
    Bin@ = append_type(action_type(Action), Bin@),
    Bin@ = append_slot(SlotId, Bin@),
    Bin@ = append_value(SlotId, Value, S2T, Bin@),
    Bin@ = append_boolean(Ignore, Bin@),
    Bin@.


append_delta(Pos, Bin) ->
    append_int(Pos, append_type(delta, Bin)).


append_text(Value, WDF, Prefix, Features, undefined, Bin@) ->
    Bin@ = append_type(text, Bin@),
    Bin@ = append_string(Value, Bin@),
    Bin@ = append_uint(WDF, Bin@),
    Bin@ = append_string(Prefix, Bin@),
    Bin@ = append_features(Features, Bin@),
    Bin@;
append_text(Value, WDF, Prefix, Features, Pos, Bin@) ->
    Bin@ = append_type(set_term_gen_pos, Bin@),
    Bin@ = append_uint(Pos, Bin@),
    append_text(Value, WDF, Prefix, Features, undefined, Bin@).


append_features(undefined, Bin) ->
    append_features([default], Bin);
append_features(Features, Bin) ->
    Nums = encode_features(Features),
    append_flags(Nums, Bin).


%% Unset group
encode_features([{except, [_|_] = H}|T]) ->
    toggle_group(encode_features(H)) ++ encode_features(T);
%% Unset single
encode_features([{except, H}|T]) ->
    [- generator_feature_id(H) | encode_features(T)];
%% Set single
encode_features([H|T]) ->
    [generator_feature_id(H) | encode_features(T)];
encode_features([]) ->
    [].


toggle_group(Nums) ->
    [-N || N <- Nums].

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

append_type(Type, Bin) ->
    append_param(document_part_id(Type), Bin).




append_generator(#x_term_generator{name = Type, stopper = Stopper,
                       stemmer = Stem, stemming_strategy = StemStrategy},
                 RA, Bin@) ->
    Bin@ = append_generator_type(Type, RA, Bin@),
    Bin@ = append_stemmer(Stem, RA, Bin@),
    Bin@ = append_stopper(Stopper, RA, Bin@),
    Bin@ = append_stemming_strategy(StemStrategy, Bin@),
    Bin@ = append_stop(Bin@),
    Bin@.

%% -----------------------------------------------------------
%% Term Generator Commands
%% -----------------------------------------------------------

%% DEFAULT_GENERATOR_CHECK_MARK
append_generator_type(default, _RA, Bin) ->
    Bin; %% It is default by default :)

%% `#x_query_generator{name = ResRef}'
append_generator_type(ResRef, RA, Bin@) 
        when is_reference(ResRef) ->
    Bin@ = append_generator_command(from_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, ResRef, Bin@),
    Bin@;

append_generator_type(Type, _RA, Bin@) ->
    Bin@ = append_generator_command(generator_type, Bin@),
    Bin@ = append_generator_type_id(Type, Bin@),
    Bin@.


append_stemmer(undefined, _RA, Bin) ->
    Bin;

append_stemmer(#x_stemmer{}=Stemmer, _RA, Bin@) ->
    Bin@ = append_generator_command(stemmer, Bin@),
    Bin@ = xapian_encode:append_stemmer(Stemmer, Bin@),
    Bin@;

append_stemmer(Stemmer, RA, Bin@) ->
    Bin@ = append_generator_command(stemmer_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, Stemmer, Bin@),
    Bin@.


append_stopper(undefined, _RA, Bin) ->
    Bin;

append_stopper(Stopper, RA, Bin@) ->
    Bin@ = append_generator_command(stopper_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, Stopper, Bin@),
    Bin@.


%% See `XapianErlangDriver::readStemmingStrategy'
append_stemming_strategy(Strategy, Bin@) ->
    case stem_strategy_id(Strategy) of
    %% Default
    0 -> 
        Bin@;
    StrategyId ->
        Bin@ = append_generator_command(stemming_strategy, Bin@),
        Bin@ = append_uint8(StrategyId, Bin@),
        Bin@
    end.

append_generator_type_id(Id, Bin) ->
    append_param(generator_type_id(Id), Bin).


append_generator_command(Command, Bin) ->
    append_param(generator_command_id(Command), Bin).


%% @doc Set a short version of prefixes.
%% Left as is if there is no this name.
replace_prefix_name(Name, N2P) ->
    case orddict:find(Name, N2P) of
        {ok, Prefix} -> Prefix;
        error -> Name %% It is a prefix already.
    end.

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
           ], [], [], undefined, undefined).

-endif.
