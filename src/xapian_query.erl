-module(xapian_query).
-export([encode/4]).

-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_common, [ 
    append_uint/2,
    append_uint8/2,
    append_double/2,
    append_iolist/2,
    slot_id/2]).

-import(xapian_const, [
    query_id/1,
    operator_id/1,
    parser_type_id/1,
    parser_command_id/1,
    parser_feature_id/1,
    stem_strategy_id/1]).

%% S2T stands for Slot2TypeArray.
encode(#x_query{op=Op, value=Value, parameter=Param}, N2S, S2T, Bin@) ->
    Bin@ = append_type(query_group, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(Param, Bin@),
    Bin@ = append_query(Value, N2S, S2T, Bin@),
    Bin@;

encode(#x_query_value{op=Op, slot=Slot, value=Value}, N2S, S2T, Bin@) ->
    SlotId = slot_id(Slot, N2S),
    Bin@ = append_type(query_value, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(SlotId, Bin@),
    Bin@ = append_value(SlotId, Value, S2T, Bin@),
    Bin@;

encode(#x_query_value_range{op=Op, slot=Slot, from=From, to=To}, N2S, S2T, Bin@) ->
    SlotId = slot_id(Slot, N2S),
    Bin@ = append_type(query_value_range, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(SlotId, Bin@),
    Bin@ = append_value(SlotId, From, S2T, Bin@),
    Bin@ = append_value(SlotId, To, S2T, Bin@),
    Bin@;

encode(#x_query_term{name=Name, wqf=WQF, position=Pos}, _N2S, _S2T, Bin@) ->
    Bin@ = append_type(query_term, Bin@),
    Bin@ = append_iolist(Name, Bin@),
    Bin@ = append_uint(WQF, Bin@),
    Bin@ = append_uint(Pos, Bin@),
    Bin@;

encode(#x_query_string{parser=Parser, value=String, 
    default_prefix=Prefix, features=Features}, _N2S, _S2T, Bin@) ->
    Bin@ = append_type(query_string, Bin@),
    Bin@ = append_parser(Parser, Bin@),
    Bin@ = append_iolist(String, Bin@),
    Bin@ = append_iolist(Prefix, Bin@),
    Bin@ = append_parser_feature_ids(Features, Bin@),
    Bin@;

encode(#x_query_scale_weight{value=SubQuery, op=Op, factor=Fac}, N2S, S2T, Bin@) ->
    Bin@ = append_type(query_scale_weight, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_double(Fac, Bin@),
    Bin@ = encode(SubQuery, N2S, S2T, Bin@),
    Bin@;

encode(Term, N2S, S2T, Bin) ->
    encode(#x_query_term{name=Term}, N2S, S2T, Bin).


append_operator(Op, Bin) ->
    append_uint8(operator_id(Op), Bin).


append_type(Type, Bin) ->
    append_uint8(query_id(Type), Bin).


append_query(Query, N2S, S2T, Bin@) ->
    %% Defines when to stop.
    SubQueryCount = erlang:length(Query),
    Bin@ = append_uint(SubQueryCount, Bin@),
    lists:foldl(fun(Rec, Acc) -> encode(Rec, N2S, S2T, Acc) end, Bin@, Query).

append_parser_type_id(Id, Bin) ->
    append_uint8(parser_type_id(Id), Bin).


append_parser_command(Command, Bin) ->
    append_uint8(parser_command_id(Command), Bin).


append_parser_feature_id(Flag, Bin) ->
    append_uint8(parser_feature_id(Flag), Bin).


%% ------------------------------------------------------------
%% Query Parser
%% ------------------------------------------------------------

%% Encode QueryParser
append_parser(default, Bin@) ->
    %% DEFAULT_PARSER_CHECK_MARK in the C++ code
    %% No commands, the default parser
    append_uint8(0, Bin@);

append_parser(standard, Bin@) ->
    Bin@ = append_parser_type(standard, Bin@),
    Bin@ = append_parser_command(stop, Bin@),
    Bin@;

append_parser(#x_query_parser{}=Rec, Bin@) ->
    #x_query_parser
    {
        name = Type,
        stemmer = Stem,
        stemming_strategy = StemStrategy,
        max_wildcard_expansion = MaxWildCardExp,
        default_op = Operator,
        prefixes = Prefixes
    } = Rec,
    Bin@ = append_parser_type(Type, Bin@),
    Bin@ = append_stemmer(Stem, Bin@),
    Bin@ = append_stemming_strategy(StemStrategy, Bin@),
    Bin@ = append_max_wildcard_expansion(MaxWildCardExp, Bin@),
    Bin@ = append_default_op(Operator, Bin@),
    Bin@ = append_prefixes(Prefixes, Bin@),
    Bin@ = append_parser_command(stop, Bin@),
    Bin@.


%% -----------------------------------------------------------
%% Query Parser Commands
%% -----------------------------------------------------------

%% `QP_PARSER_TYPE' command, `XapianErlangDriver::selectParser'
append_parser_type(default, Bin) ->
    Bin; %% It is default by default :)

append_parser_type(Type, Bin@) ->
    Bin@ = append_parser_command(parser_type, Bin@),
    Bin@ = append_parser_type_id(Type, Bin@),
    Bin@.


append_stemmer(undefined, Bin) ->
    Bin;

append_stemmer(#x_stemmer{}=Stemmer, Bin@) ->
    Bin@ = append_parser_command(stemmer, Bin@),
    Bin@ = xapian_encode:append_stemmer(Stemmer, Bin@),
    Bin@.


%% See `XapianErlangDriver::readStemmingStrategy'
append_stemming_strategy(Strategy, Bin@) ->
    case stem_strategy_id(Strategy) of
    %% Default
    0 -> 
        Bin@;
    StrategyId ->
        Bin@ = append_parser_command(stemming_strategy, Bin@),
        Bin@ = append_uint8(StrategyId, Bin@),
        Bin@
    end.


append_max_wildcard_expansion(Count, Bin@) when Count > 0, is_integer(Count) ->
    Bin@ = append_parser_command(max_wildcard_expansion, Bin@),
    Bin@ = append_uint(Count, Bin@),
    Bin@;

%% Skip default
append_max_wildcard_expansion(Default, Bin) 
    when Default =:= 0; Default =:= unlimited; Default =:= undefined ->
    Bin.


%% OR is by default.
append_default_op('OR', Bin@) ->
    Bin@;

append_default_op(Op, Bin@) ->
    Bin@ = append_parser_command(default_op, Bin@),
    Bin@ = append_uint8(operator_id(Op), Bin@),
    Bin@.


append_prefixes([H|T], Bin@) ->
    Prefix = xapian_check:check_prefix(H),
    Bin@ = append_parser_command(prefix, Bin@),
    Bin@ = xapian_encode:append_prefix(Prefix, Bin@),
    append_prefixes(T, Bin@);

append_prefixes([], Bin) -> 
    Bin.



%% Checks `undefined' value
%% See `XapianErlangDriver::decodeParserFeatureFlags'
append_parser_feature_ids(undefined, Bin) ->
    append_features([default], Bin);

append_parser_feature_ids(Features, Bin) ->
    append_features(Features, Bin).


%% See `XapianErlangDriver::decodeParserFeatureFlags'
append_features([], Bin) ->
    append_parser_feature_id(stop, Bin);

append_features([H|T], Bin) ->
    append_features(T, append_parser_feature_id(H, Bin)).



append_value(SlotId, Value, Slot2TypeArray, Bin) when is_integer(SlotId) -> 
    CheckedValue = xapian_common:fix_value(SlotId, Value, Slot2TypeArray),
    xapian_common:append_value(CheckedValue, Bin).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Result = encode(#x_query{value=["test1", <<"test2">>]}, [], [], <<>>),
    io:write(user, Result).

-endif.
