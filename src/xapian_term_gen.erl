-module(xapian_term_gen).
-export([append_generator/3]).

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
    slot_id/2]).

-import(xapian_const, [
    term_id/1,
    operator_id/1,
    generator_type_id/1,
    generator_command_id/1,
    generator_feature_id/1,
    stem_strategy_id/1]).


%% ------------------------------------------------------------
%% Term Generator
%% ------------------------------------------------------------

%% Encode TermGenerator
append_generator(_RA, default, Bin@) ->
    %% DEFAULT_PARSER_CHECK_MARK in the C++ code
    %% No commands, the default generator
    append_stop(Bin@);

append_generator(RA, standard, Bin@) ->
    Bin@ = append_generator_type(standard, RA, Bin@),
    Bin@ = append_stop(Bin@),
    Bin@;

append_generator(RA, #x_term_generator{}=Rec, Bin@) ->
    #x_term_generator
    {
        name = Type,
        stemmer = Stem,
        stemming_strategy = StemStrategy,
        stopper = Stopper
    } = Rec,
    Bin@ = append_generator_type(Type, RA, Bin@),
    Bin@ = append_stemmer(Stem, RA, Bin@),
    Bin@ = append_stopper(Stopper, RA, Bin@),
    Bin@ = append_stemming_strategy(StemStrategy, Bin@),
    Bin@ = append_stop(Bin@),
    Bin@;

append_generator(RA, ResRef, Bin@) when is_reference(ResRef) ->
    Bin@ = append_generator_command(from_resource, Bin@),
    Bin@ = xapian_common:append_resource(RA, ResRef, Bin@),
    Bin@ = append_stop(Bin@),
    Bin@.

%% -----------------------------------------------------------
%% Term Generator Commands
%% -----------------------------------------------------------

%% `TG_TYPE' command, `XapianErlangDriver::selectGenerator'
append_generator_type(default, _RA, Bin) ->
    Bin; %% It is default by default :)

%% `#x_term_generator{name = ResRef}'
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

append_generator_command(Command, Bin) ->            
   append_param(generator_command_id(Command), Bin).


append_generator_type_id(Id, Bin) ->         
    append_param(generator_type_id(Id), Bin).

