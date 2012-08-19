-module(xapian_query).
-export([encode/5]).
-export([append_parser/3, append_query_string/3]).

-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_common, [ 
    append_uint/2,
    append_uint8/2,
    append_param/2,
    append_stop/1,
    append_flags/2,
    append_double/2,
    append_string/2,
    append_value/4,
    slot_id/2]).

-import(xapian_const, [
    query_id/1,
    operator_id/1,
    parser_type_id/1,
    parser_command_id/1,
    parser_feature_id/1,
    stem_strategy_id/1]).

%% S2T stands for Slot2TypeArray.
%% RA/2 is a resource appender.
encode(#x_query{op=Op, value=Value, parameter=Param}, N2S, S2T, RA, Bin@) ->
    Bin@ = append_type(query_group, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(Param, Bin@),
    Bin@ = append_query(Value, N2S, S2T, RA, Bin@),
    Bin@;

encode(#x_query_value{op=equal, slot=Slot, value=Value}, N2S, S2T, RA, Bin) ->
    Q = #x_query_value_range{op=equal, slot=Slot, from=Value, to=Value},
    encode(Q, N2S, S2T, RA, Bin);

encode(#x_query_value{op=Op, slot=Slot, value=Value}, N2S, S2T, _RA, Bin@) ->
    SlotId = slot_id(Slot, N2S),
    Bin@ = append_type(query_value, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(SlotId, Bin@),
    Bin@ = append_value(SlotId, Value, S2T, Bin@),
    Bin@;

encode(#x_query_value_range{op=Op, slot=Slot, from=From, to=To}, 
       N2S, S2T, _RA, Bin@) ->
    SlotId = slot_id(Slot, N2S),
    Bin@ = append_type(query_value_range, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(SlotId, Bin@),
    Bin@ = append_value(SlotId, From, S2T, Bin@),
    Bin@ = append_value(SlotId, To, S2T, Bin@),
    Bin@;

encode(#x_query_term{name=Name, wqf=WQF, position=Pos}, _N2S, _S2T, _RA, Bin@) ->
    Bin@ = append_type(query_term, Bin@),
    Bin@ = append_string(Name, Bin@),
    Bin@ = append_uint(WQF, Bin@),
    Bin@ = append_uint(Pos, Bin@),
    Bin@;

encode(#x_query_string{} = Rec, _N2S, _S2T, RA, Bin@) ->
    Bin@ = append_type(query_string, Bin@),
    Bin@ = append_query_string(Rec, RA, Bin@),
    Bin@;

encode(#x_query_scale_weight{value=SubQuery, op=Op, factor=Fac}, 
       N2S, S2T, RA, Bin@) ->
    Bin@ = append_type(query_scale_weight, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_double(Fac, Bin@),
    Bin@ = encode(SubQuery, N2S, S2T, RA, Bin@),
    Bin@;

encode(ResRef, _N2S, _S2T, RA, Bin@) when is_reference(ResRef) ->
    Bin@ = append_type(query_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, ResRef, Bin@),
    Bin@;

%% Otherwise, term
encode(Term, N2S, S2T, RA, Bin) ->
    encode(#x_query_term{name=Term}, N2S, S2T, RA, Bin).


append_operator(Op, Bin) ->
    append_uint8(operator_id(Op), Bin).


append_type(Type, Bin) ->
    append_param(query_id(Type), Bin).


append_query(Query, N2S, S2T, RA, Bin@) ->
    %% Defines when to stop.
    SubQueryCount = erlang:length(Query),
    Bin@ = append_uint(SubQueryCount, Bin@),
    lists:foldl(fun(Rec, Acc) -> encode(Rec, N2S, S2T, RA, Acc) end, Bin@, Query).

append_parser_type_id(Id, Bin) ->
    append_param(parser_type_id(Id), Bin).


append_parser_command(Command, Bin) ->
    append_param(parser_command_id(Command), Bin).



append_query_string(#x_query_string{parser=Parser, value=String, 
    default_prefix=Prefix, features=Features}, RA, Bin@) ->
    Bin@ = append_parser(RA, Parser, Bin@),
    Bin@ = append_string(String, Bin@),
    Bin@ = append_string(Prefix, Bin@),
    Bin@ = append_parser_feature_ids(Features, Bin@),
    Bin@.


%% ------------------------------------------------------------
%% Query Parser
%% ------------------------------------------------------------

%% Encode QueryParser
append_parser(_RA, default, Bin@) ->
    %% DEFAULT_PARSER_CHECK_MARK in the C++ code
    %% No commands, the default parser
    append_stop(Bin@);

append_parser(RA, standard, Bin@) ->
    Bin@ = append_parser_type(standard, RA, Bin@),
    Bin@ = append_stop(Bin@),
    Bin@;

append_parser(RA, #x_query_parser{}=Rec, Bin@) ->
    #x_query_parser
    {
        name = Type,
        stemmer = Stem,
        stemming_strategy = StemStrategy,
        stopper = Stopper,
        max_wildcard_expansion = MaxWildCardExp,
        default_op = Operator,
        prefixes = Prefixes,
        value_range_processors = ValueRangeProcs
    } = Rec,
    Bin@ = append_parser_type(Type, RA, Bin@),
    Bin@ = append_stemmer(Stem, RA, Bin@),
    Bin@ = append_stopper(Stopper, RA, Bin@),
    Bin@ = append_stemming_strategy(StemStrategy, Bin@),
    Bin@ = append_max_wildcard_expansion(MaxWildCardExp, Bin@),
    Bin@ = append_default_op(Operator, Bin@),
    Bin@ = append_prefixes(Prefixes, Bin@),
    Bin@ = value_range_processors(RA, ValueRangeProcs, Bin@),
    Bin@ = append_stop(Bin@),
    Bin@;

append_parser(RA, ResRef, Bin@) when is_reference(ResRef) ->
    Bin@ = append_parser_command(from_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, ResRef, Bin@),
    Bin@ = append_stop(Bin@),
    Bin@.

%% -----------------------------------------------------------
%% Query Parser Commands
%% -----------------------------------------------------------

%% `QP_PARSER_TYPE' command, `XapianErlangDriver::selectParser'
append_parser_type(default, _RA, Bin) ->
    Bin; %% It is default by default :)

%% `#x_query_parser{name = ResRef}'
append_parser_type(ResRef, RA, Bin@) 
        when is_reference(ResRef) ->
    Bin@ = append_parser_command(from_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, ResRef, Bin@),
    Bin@;

append_parser_type(Type, _RA, Bin@) ->
    Bin@ = append_parser_command(parser_type, Bin@),
    Bin@ = append_parser_type_id(Type, Bin@),
    Bin@.


append_stemmer(undefined, _RA, Bin) ->
    Bin;

append_stemmer(#x_stemmer{}=Stemmer, _RA, Bin@) ->
    Bin@ = append_parser_command(stemmer, Bin@),
    Bin@ = xapian_encode:append_stemmer(Stemmer, Bin@),
    Bin@;

append_stemmer(Stemmer, RA, Bin@) ->
    Bin@ = append_parser_command(stemmer_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, Stemmer, Bin@),
    Bin@.


append_stopper(undefined, _RA, Bin) ->
    Bin;

append_stopper(Stopper, RA, Bin@) ->
    Bin@ = append_parser_command(stopper_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, Stopper, Bin@),
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


value_range_processors(RA, Procs, Bin) ->
    F = fun(R, B@) -> 
            B@ = append_parser_command(value_range_processor, B@),
            xapian_common:append_resource(RA, R, B@)
        end,
    lists:foldl(F, Bin, Procs).


%% Checks `undefined' value
%% See `XapianErlangDriver::decodeParserFeatureFlags'
append_parser_feature_ids(undefined, Bin) ->
    append_features([default], Bin);

append_parser_feature_ids(Features, Bin) ->
    append_features(Features, Bin).


%% See `XapianErlangDriver::decodeParserFeatureFlags'
append_features(Features, Bin) ->
    Nums = encode_features(Features),
    append_flags(Nums, Bin).

%% Unset group
encode_features([{except, [_|_] = H}|T]) ->
    toggle_group(encode_features(H)) ++ encode_features(T);
%% Unset single 
encode_features([{except, H}|T]) ->
    [- parser_feature_id(H) | encode_features(T)];
%% Set single
encode_features([H|T]) ->
    [parser_feature_id(H) | encode_features(T)];
encode_features([]) ->
    [].


toggle_group(Nums) ->
    [-N || N <- Nums].



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Result = encode(#x_query{value=["test1", <<"test2">>]}, [], [], undefined, <<>>),
    io:write(user, Result).

-endif.
