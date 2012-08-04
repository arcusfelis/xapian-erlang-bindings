-module(xapian_proper).
-include_lib("xapian/include/xapian.hrl").
-include_lib("xapian/src/xapian.hrl").
-include_lib("proper/include/proper.hrl").

-export([ x_query_parser/0
        , x_query_string/0
        , x_query/0]).

-type xp_generator() :: term().

-spec unicode_char() -> xp_generator().
unicode_char() ->
    ?SUCHTHAT(C, char(), C < 16#D800 orelse C > 16#DFFF).

-spec unicode_string() -> xp_generator().
unicode_string() ->
    list(unicode_char()).

-spec unicode_binary() -> xp_generator().
unicode_binary() ->
    ?LET(S, unicode_string(), unicode:characters_to_binary(S)).

-spec unicode_characters() -> xp_generator().
%% UTF-8 binary or a list of codepoints.
unicode_characters() ->
    oneof([unicode_string(), unicode_binary()]).

-spec x_string() -> xp_generator().
x_string() -> 
    unicode_characters().

-spec x_stemmer() -> xp_generator().
x_stemmer() ->
    #x_stemmer{language = x_language_code()}.

-spec x_prefix_name() -> xp_generator().
x_prefix_name() ->
    #x_prefix_name{
        name = oneof([x_string(), atom()]),
        prefix = oneof([x_string(), unicode_char()]),
        is_boolean = boolean(),
        is_exclusive = boolean(),
        is_default = boolean()
    }.

-spec x_query_parser() -> xp_generator().
x_query_parser() ->
    #x_query_parser{
        name = oneof([default, standard]),
        stemmer = maybe(x_stemmer()),
        stemming_strategy = oneof([none, some, all, default]),
        max_wildcard_expansion = oneof([non_neg_integer(), unlimited]), 
        default_op = x_operator(),
        prefixes = list(x_prefix_name())
    }.

-spec x_query_string() -> xp_generator().
x_query_string() ->
    #x_query_string{
        parser = oneof([default, standard, x_query_parser()]),
        value = x_string(),                    
        default_prefix = x_string(),               
        features = maybe(list(x_parser_feature()))
    }.

-spec x_query() -> xp_generator().
x_query() ->
    #x_query{
        op = x_operator(),
        value = ?LAZY(list(union([x_sub_query(), x_string()]))),
        parameter = non_neg_integer()
    }.

-spec x_query_value() -> xp_generator().
x_query_value() ->
    #x_query_value{
        op = oneof(['VALUE GE', 'VALUE LE', greater, less, lower, equal]),
        slot = non_neg_integer(),
        value = x_string()
    }.

-spec x_query_value_range() -> xp_generator().
x_query_value_range() ->
    #x_query_value_range{
        slot = non_neg_integer(),
        from = x_string(),
        to = x_string()
    }.

-spec x_query_term() -> xp_generator().
x_query_term() ->
    #x_query_term{
        name = x_string(),
        wqf  = non_neg_integer(),
        position = non_neg_integer()
    }.

-spec x_query_scale_weight() -> xp_generator().
x_query_scale_weight() ->
    #x_query_scale_weight{
        op = 'SCALE WEIGHT',
        value = x_sub_query(),
        factor = float()
    }.

x_sub_query() ->
    oneof([ x_query(), x_query_string(), x_query_value()
          , x_query_value_range(), x_query_term(), x_query_scale_weight()
          ]).


-spec maybe(xp_generator()) -> xp_generator().
maybe(Gen) ->
    oneof([undefined, Gen]).

%% It can be an atom, a binary or a list of characters.
-spec x_language_code() -> xp_generator().
x_language_code() -> 
    ?LET({F, X}, 
         {oneof([nope() | atom_to_characters()]), oneof(languages())},
         F(X)).

-spec atom_to_characters() -> [fun((atom()) -> string() | binary())].
atom_to_characters() ->
    [ fun atom_to_list/1
    , fun(A) -> list_to_binary(atom_to_list(A)) end].

-spec nope() -> [fun((term()) -> term())].
nope() -> fun(X) -> X end.

-spec x_operator() -> xp_generator().
x_operator() ->
    oneof(operators()).

-spec x_parser_feature() -> xp_generator().
x_parser_feature() ->
    oneof(features()).

operators() ->
  [ 'AND'
  , 'OR'
  , 'AND NOT'
  , 'XOR'
  , 'AND MAYBE'
  , 'FILTER'
  , 'NEAR'
  , 'PHRASE'
  , 'VALUE RANGE' 
  , 'SCALE WEIGHT'
  , 'ELITE SET'
  , 'VALUE GE'
  , 'VALUE LE'
  , 'SYNONYM'
  , greater
  , lower
  , less
  ].

languages() ->
    [ none % - don't stem terms                       
    , ''                                          
    , en , english  %% Martin Porter's 2002 revision
    , da , danish                                   
    , nl , dutch                                    
    , fi , finnish                                  
    , fr , french                                   
    , de , german, german2       
    , hu , hungarian                                
    , it , italian                                  
    , nb , nn , no , norwegian                      
    , pt , portuguese                               
    , ro , romanian                                 
    , ru , russian                                  
    , es , spanish                                  
    , sv , swedish                                  
    , tr , turkish                                  
    , lovins , porter
    , kraaij_pohlmann].

features() ->
    [ 'BOOLEAN'
    , 'PHRASE'
    , 'LOVEHATE'
    , 'BOOLEAN ANY CASE'
    , 'WILDCARD'
    , 'PURE NOT'
    , 'PARTIAL'
    , 'SPELLING CORRECTION'
    , 'SYNONYM'
    , 'AUTO SYNONYMS'
    , 'AUTO MULTIWORD SYNONYMS'
    , 'DEFAULT'
    , 'SYNONYMS'
    , boolean
    , phrase 
    , lovehate
    , boolean_any_case
    , wildcard
    , pure_not
    , partial
    , spelling_correction
    , synonym
    , synonyms
    , auto_synonyms
    , default].

