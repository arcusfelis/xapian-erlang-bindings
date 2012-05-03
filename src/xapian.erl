-module(xapian).
-export([start/0]).

-export_type([
    x_string/0, 
    x_slot_name/0, 
    x_slot/0, 
    x_slot_value/0,
    x_document_index_part/0,
    x_document_id/0,
    x_order_type/0]).

-export_type([
    x_server/0,
    x_transaction/0,
    x_resource/0,
    x_meta/0,
    x_record/0]).

-include_lib("xapian/include/xapian.hrl").


start() ->
    application:start(xapian).


-type x_string()        :: binary().
-type x_slot_name()     :: atom().
-type x_slot()          :: non_neg_integer().
-type x_position()      :: non_neg_integer().
-type x_wdf()           :: non_neg_integer().
-type x_document_id()   :: non_neg_integer().
-type x_slot_value()    :: x_slot() | x_slot_name().
-type x_order_type() ::
    key 
    | relevance 
    | value 
    | key_relevance 
    | relevance_key 
    | relevance_value
    | value_relevance.

-type x_term()          :: #x_term{}.
-type x_value()         :: #x_value{}.
-type x_data()          :: #x_data{}.
-type x_text()          :: #x_text{}.
-type x_delta()         :: #x_delta{}.

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
    | #x_query_string{}.

-type x_resource() :: reference().
-type x_record() :: tuple().
-type x_meta() :: [atom()].

