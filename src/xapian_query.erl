-module(xapian_query).
-export([encode/3]).

-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_common, [ 
    append_uint/2,
    append_uint8/2,
    append_slot/3,
    append_iolist/2]).

%% From `include/xapian/query.h'
operator_id('AND')          -> 0;
operator_id('OR')           -> 1;
operator_id('AND NOT')      -> 2;
operator_id('XOR')          -> 3;
operator_id('AND MAYBE')    -> 4;
operator_id('FILTER')       -> 5;
operator_id('NEAR')         -> 6;
operator_id('PHRASE')       -> 7;
operator_id('VALUE RANGE')  -> 8;
operator_id('SCALE WEIGHT') -> 9;
operator_id('ELITE SET')    -> 10;
operator_id('VALUE GE')     -> 11;
operator_id('VALUE LE')     -> 12;
operator_id('SYNONYM')      -> 13.


query_id(query_group)       -> 1;
query_id(query_value)       -> 2;
query_id(query_value_range) -> 3;
query_id(query_term)        -> 4;
query_id(query_string)      -> 5.


encode(#x_query{op=Op, value=Value, parameter=Param}, N2S, Bin@) ->
    Bin@ = append_type(query_group, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_uint(Param, Bin@),
    %% Defines when to stop.
    SubQueryCount = erlang:length(Value),
    Bin@ = append_uint(SubQueryCount, Bin@),
    lists:foldl(fun(Rec, Acc) -> encode(Rec, N2S, Acc) end, Bin@, Value);

encode(#x_query_value{op=Op, slot=Slot, value=Value}, N2S, Bin@) ->
    Bin@ = append_type(query_value, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_slot(Slot, N2S, Bin@),
    Bin@ = append_iolist(Value, Bin@),
    Bin@;

encode(#x_query_value_range{op=Op, slot=Slot, from=From, to=To}, N2S, Bin@) ->
    Bin@ = append_type(query_value_range, Bin@),
    Bin@ = append_operator(Op, Bin@),
    Bin@ = append_slot(Slot, N2S, Bin@),
    Bin@ = append_iolist(From, Bin@),
    Bin@ = append_iolist(To, Bin@),
    Bin@;

encode(#x_query_term{name=Name, wqf=WQF, position=Pos}, _N2S, Bin@) ->
    Bin@ = append_type(query_term, Bin@),
    Bin@ = append_iolist(Name, Bin@),
    Bin@ = append_uint(WQF, Bin@),
    Bin@ = append_uint(Pos, Bin@),
    Bin@;

encode(#x_query_string{parser=Parser, string=String, 
    default_prefix=Prefix, features=Features}, _N2S, Bin@) ->
    Bin@ = append_type(query_string, Bin@),
    Bin@ = append_parser(Parser, Bin@),
    Bin@ = append_iolist(String, Bin@),
    Bin@ = append_iolist(Prefix, Bin@),
    Bin@ = append_parser_feature_ids(Features, Bin@),
    Bin@;

encode(Term, N2S, Bin) ->
    encode(#x_query_term{name=Term}, N2S, Bin).


append_operator(Op, Bin) ->
    append_uint8(operator_id(Op), Bin).


append_type(Type, Bin) ->
    append_uint8(query_id(Type), Bin).




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Result = encode(#x_query{value=["test1", <<"test2">>]}, [], <<>>),
    io:write(user, Result).

-endif.


%% ------------------------------------------------------------
%% Query Parser Header
%% ------------------------------------------------------------

%% @see XapianErlangDriver::queryParserCommand
parser_command_id(stemmer)                  -> 1;
parser_command_id(stemming_strategy)        -> 2;
parser_command_id(max_wildcard_expansion)   -> 3;
parser_command_id(default_op)               -> 4;
parser_command_id(parser_type)              -> 5; 
parser_command_id(prefix)                   -> 6.


%% @see XapianErlangDriver::queryParserType
parser_type_id(default) -> 0;
parser_type_id(empty)   -> 1.


%% @see XapianErlangDriver::STEM_STRATEGIES
stem_strategy_id(none)    -> 0;
stem_strategy_id(default) -> 0;
stem_strategy_id(some)    -> 1;
stem_strategy_id(all)     -> 2.



parser_feature_id(stop)                    -> 0;
parser_feature_id('BOOLEAN')               -> 1;
parser_feature_id('PHRASE')                -> 2;
parser_feature_id('LOVEHATE')              -> 3;
parser_feature_id('BOOLEAN ANY CASE')      -> 4;
parser_feature_id('WILDCARD')              -> 5;
parser_feature_id('PURE NOT')              -> 6;
parser_feature_id('PARTIAL')               -> 7;
parser_feature_id('SPELLING CORRECTION')   -> 8;
parser_feature_id('SYNONYM')               -> 9;
parser_feature_id('AUTO SYNONYMS')         -> 10;
parser_feature_id('AUTO MULTIWORD SYNONYMS') -> 11;
parser_feature_id('DEFAULT')               -> 12;

parser_feature_id('SYNONYMS') -> 
    parser_feature_id('SYNONYM');

parser_feature_id(boolean) -> 
    parser_feature_id('BOOLEAN');

parser_feature_id(phrase) -> 
    parser_feature_id('PHRASE');

parser_feature_id(lovehate) -> 
    parser_feature_id('LOVEHATE');

parser_feature_id(boolean_any_case) -> 
    parser_feature_id('BOOLEAN ANY CASE');

parser_feature_id(wildcard) -> 
    parser_feature_id('WILDCARD');

parser_feature_id(pure_not) -> 
    parser_feature_id('PURE NOT');

parser_feature_id(partial) -> 
    parser_feature_id('PARTIAL');

parser_feature_id(spelling_correction) -> 
    parser_feature_id('SPELLING CORRECTION');

parser_feature_id(synonym) -> 
    parser_feature_id('SYNONYM');

parser_feature_id(synonyms) -> 
    parser_feature_id('SYNONYM');

parser_feature_id(auto_synonyms) -> 
    parser_feature_id('AUTO SYNONYMS');

parser_feature_id(default) -> 
    parser_feature_id('DEFAULT').


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

append_parser(empty, Bin@) ->
    append_parser_type(empty, Bin@);

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
    Bin@ = append_stemming_strategy(Stem, Bin@),
    Bin@ = append_max_wildcard_expansion(Stem, Bin@),
    Bin@ = append_default_op(Stem, Bin@),
    Bin@ = append_prefixes(Prefixes, Bin@),
    Bin@.


%% -----------------------------------------------------------
%% Query Parser Commands
%% -----------------------------------------------------------

%% `QP_PARSER_TYPE' command, `XapianErlangDriver::selectParser'
append_parser_type(default, Bin) ->
    Bin; %% It is default by default :)

append_parser_type(Type, Bin@) ->
    Bin@ = append_parser_command(parser_type, Bin@),
    Bin@ = append_parser_type(Type, Bin@),
    Bin@.


append_stemmer(undefined, Bin) ->
    Bin;

append_stemmer(#x_stemmer{}=Stemmer, Bin@) ->
    Bin@ = append_parser_command(stemmer, Bin@),
    Bin@ = xapian_encode:append_stemmer(Stemmer, Bin@),
    Bin@.


%% @see `XapianErlangDriver::readStemmingStrategy'
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
    when Default =:= 0; Default =:= unlimit, Default =:= undefined ->
    Bin.



append_default_op(Op, Bin@) ->
    Bin@ = append_parser_command(default_op, Bin@),
    Bin@ = append_uint8(operator_id(Op), Bin@),
    Bin@.


append_prefixes([], Bin) -> 
    Bin;

append_prefixes([H|T], Bin@) ->
    Prefix = xapian_check:check_prefix(H),
    Bin@ = append_parser_command(prefix, Bin@),
    Bin@ = xapian_encode:append_prefix(Prefix, Bin@),
    append_prefixes(T, Bin@).


%% Checks `undefined' value
%% @see `XapianErlangDriver::decodeParserFeatureFlags'
append_parser_feature_ids(undefined, Bin) ->
    append_features([default], Bin);

append_parser_feature_ids(Features, Bin) ->
    append_features(Features, Bin).


%% @see `XapianErlangDriver::decodeParserFeatureFlags'
append_features([], Bin) ->
    append_parser_feature_id(stop, Bin);

append_features([H|T], Bin) ->
    append_features(T, append_parser_feature_id(H, Bin)).
