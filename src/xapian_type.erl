-module(xapian_type).

-export_type([
    x_string/0, 
    x_non_empty_string/0,
    x_slot_name/0, 
    x_slot/0, 
    x_position/0,
    x_slot_value/0,
    x_term_count/0,
    x_document_index_part/0,
    x_document_id/0,
    x_unique_document_id/0,
    x_order_type/0,
    x_timeout/0,
    x_inet_port/0,
    x_inet_address/0,
    x_parser_feature/0,
    x_query_parser/0,
    x_operator/0,
    x_server/0,
    x_transaction/0,
    x_resource/0,
    x_meta/0,
    x_record/0,
    x_prefix_name/0,
    x_stemmer/0,
    x_language_code/0]).

-type noempty_binary() :: <<_:8, _:_ * 8>>.

-type x_string()        :: iolist() | binary().
-type x_non_empty_string() :: nonempty_string() | noempty_binary() 
    | [x_non_empty_string() | nonempty_string() | noempty_binary(), ...].
-type x_slot_name()     :: atom().
-type x_slot()          :: non_neg_integer().
-type x_position()      :: non_neg_integer().
-type x_term_count()    :: non_neg_integer().
-type x_document_id()   :: non_neg_integer().
-type x_slot_value()    :: x_slot() | x_slot_name().
-type x_timeout()       :: non_neg_integer().
-type x_order_type() ::
    key 
    | relevance 
    | value 
    | key_relevance 
    | relevance_key 
    | relevance_value
    | value_relevance.

-include_lib("xapian/include/xapian.hrl").

-type x_term()          :: #x_term{}.
-type x_value()         :: #x_value{}.
-type x_data()          :: #x_data{}.
-type x_text()          :: #x_text{}.
-type x_delta()         :: #x_delta{}.
-type x_query_parser()  :: #x_query_parser{}.
-type x_stemmer()       :: #x_stemmer{}.
-type x_prefix_name()   :: #x_prefix_name{}.

-type x_document_index_part() :: x_term() 
    | x_value() 
    | x_data() 
    | x_delta() 
    | x_text().

-type x_server() :: pid().
-type x_transaction() :: fun(([x_server()]) -> term()).

-type x_query() :: #x_query{} 
    | #x_query_value{} 
    | #x_query_value_range{} 
    | #x_query_term{}
    | #x_query_string{}
    | #x_query_scale_weight{}.

-type x_resource() :: reference().
-type x_record() :: tuple().
-type x_meta() :: [atom()].

%% An document id or an unique term
-type x_unique_document_id() :: x_document_id() | x_string().

-type x_inet_port() :: 0 .. 65535.
-type x_inet_address() :: x_string().


-type x_parser_feature() ::
      'BOOLEAN'
    | 'PHRASE'
    | 'LOVEHATE'
    | 'BOOLEAN ANY CASE'
    | 'WILDCARD'
    | 'PURE NOT'
    | 'PARTIAL'
    | 'SPELLING CORRECTION'
    | 'SYNONYM'
    | 'AUTO SYNONYMS'
    | 'AUTO MULTIWORD SYNONYMS'
    | 'DEFAULT'
    | 'SYNONYMS'
    | boolean
    | phrase 
    | lovehate
    | boolean_any_case
    | wildcard
    | pure_not
    | partial
    | spelling_correction
    | synonym
    | synonyms
    | auto_synonyms
    | default.


-type x_operator() ::
      'AND'
    | 'OR'
    | 'AND NOT'
    | 'XOR'
    | 'AND MAYBE'
    | 'FILTER'
    | 'NEAR'
    | 'PHRASE'
    | 'VALUE RANGE'
    | 'SCALE WEIGHT'
    | 'ELITE SET'
    | 'VALUE GE'
    | 'VALUE LE'
    | 'SYNONYM'
    | greater
    | lower
    | less.


%% @doc Either the English name for the language or the two letter ISO639 code.
%% http://xapian.org/docs/apidoc/html/classXapian_1_1Stem.html
-type x_language_code() :: 
    none % - don't stem terms
    | da | danish 
    | nl | dutch 
    | en | english  % Martin Porter's 2002 revision of his stemmer
    | fi | finnish
    | fr | french 
    | de | german 
         | german2 % Normalises umlauts and ß
    | hu | hungarian 
    | it | italian 
    | nb | nn | no | norwegian
    | pt | portuguese 
    | ro | romanian 
    | ru | russian
    | es | spanish 
    | sv | swedish 
    | tr | turkish
    | english_lovins | lovins %  Lovin's stemmer
    | english_porter | porter % Porter's stemmer as described in his 1980 paper
    | kraaij_pohlmann. % A different Dutch stemmer
