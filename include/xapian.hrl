%%% Xapian Binding Header File
-define(REQUIRED, erlang:throw(required_field)).

% ----------------------------------------------------------
%% These records are used with `open'
% ----------------------------------------------------------

-record(x_database, {
    name :: atom(),
    path = ?REQUIRED :: xapian_type:x_string()
}).

-record(x_prog_database, {
    name :: atom(),
    program = ?REQUIRED :: xapian_type:x_string(),
    arguments = ?REQUIRED :: xapian_type:x_string(),
    timeout = 10000 :: xapian_type:x_timeout()
}).

-record(x_tcp_database, {
    name :: atom(),
    host = ?REQUIRED :: xapian_type:x_inet_address(),
    port = ?REQUIRED :: xapian_type:x_inet_port(),
    timeout = 10000 :: xapian_type:x_timeout(),
    connect_timeout = 10000 :: xapian_type:x_timeout()
}).


% ----------------------------------------------------------
%% These records are used by indexer
% ----------------------------------------------------------


%% A stemmer is used for deleting duplicate terms. 
%% Terms with the same meaning but in the different forms will be equal 
%% after stemming. 
%%
%% For example, terms `"day"` and `"days"` will be equal after stemming.
-record(x_stemmer, {
    language = ?REQUIRED :: xapian_type:x_language_code()
}).


%% x_data.value will not indexed.
-record(x_data, {
    value = ?REQUIRED :: xapian_type:x_string()
}).


%% @type x_term() = #x_term{
%%      value = xapian_type:x_string(),
%%      position = xapian_type:x_position() | [xapian_type:x_position()] | undefined,
%%      frequency = xapian_type:x_wdf_difference(),
%%      action = add | set | update | remove,
%%      ignore = boolean()}.
%%
%% [https://github.com/freeakk/xapian/blob/master/doc/markdown/records.md#x_term]
-record(x_term, {
    value = ?REQUIRED :: xapian_type:x_string(),
    position :: xapian_type:x_position() | [xapian_type:x_position()] | undefined,
    frequency = 1 :: xapian_type:x_wdf_difference(),
    action = set :: add | set | update | remove,
    ignore = true :: boolean()
}).

%% Values can be used for sorting or for filtering.
%% Values are not stemmed.
%% Values are not indexed.
-record(x_value, {
    %% The number or the name of the slot.
    %% 
    %% This field is required.
    slot = ?REQUIRED  :: xapian_type:x_slot() | xapian_type:x_slot_name(),
    %% This field is required.
    value = ?REQUIRED :: xapian_type:x_string(),
    %% If action = remove and value = "", then remove slot with any value.
    %% If action = remove and value != "", then remove slot with passed value.
    %% If action = add, then value will be seted only if the slot is free.
    action = set :: add | set | update | remove,

    %% Ignore errors.
    ignore = true :: boolean()
}).


%% Calls Xapian::TermGenerator::increase_termpos.
%% Can be used as a delimeter for `'NEAR'` operator:
%%
%% Index the document:
%%
%% ```erlang
%% [#x_term{value="term1"}, #x_delta{}, #x_term{value="term2"}]
%% ```
%%
%% After this, the query "term1 NEAR term2" will skip this document.
-record(x_delta, {
    %% Amount to increase the term position by (default: 100). 
    position = 100 :: xapian_type:x_position()
}).


%% The text will be indexed as a set of terms.
-record(x_text, {
    %% The text to index.
    value = ?REQUIRED :: xapian_type:x_string(),

    %% The wdf increment (or decriment, if it is below 0).
    frequency = 1 :: xapian_type:x_wdf_difference(),

    %% The term prefix to use (default is no prefix). 
    prefix = <<>> :: xapian_type:x_string()
}).







% ----------------------------------------------------------
%% These records contain metainfo
% ----------------------------------------------------------

%% [https://github.com/freeakk/xapian/blob/master/doc/markdown/records.md#x_prefix_name]
-record(x_prefix_name, {
    name = ?REQUIRED  :: xapian_type:x_non_empty_string() | atom(),
    prefix = ?REQUIRED :: xapian_type:x_non_empty_string() | char(),
    is_boolean = false :: boolean(),
    is_exclusive = true :: boolean(),
    is_default = true :: boolean()
}).


%% [https://github.com/freeakk/xapian/blob/master/doc/markdown/records.md#x_value_name]
-record(x_value_name, {
    name :: atom(),
    slot :: non_neg_integer(),
    type = string :: float | string
}).



% ----------------------------------------------------------
%% These records used for transactions
% ----------------------------------------------------------

-record(x_transaction_result, {
    is_committed,
    is_consistent,

    %% If the transaction was completed, then contains result of transaction.
    result,

    %% Contains proplist `[{Server, StatusOfTransaction}]'.
    statuses,

    %% Contains the code of the error, that cancelled the current transaction.
    reason
}).




% ----------------------------------------------------------
%% These records used for error handling 
% ----------------------------------------------------------

-record(x_error, {
    %% Short error code
    type,

    %% Long description
    reason
}).



% ----------------------------------------------------------
%% These records used for queries
% ----------------------------------------------------------

-record(x_query, {
    op='AND' :: xapian_type:x_operator(),
    %% List of other queries or terms (term is a string).
    value :: [xapian_type:x_query() | xapian_type:x_string()],
    %% For `'NEAR'` and `'PHRASE'`, a window size can be specified in parameter.
    %% For `'ELITE_SET'`, the elite set size can be specified in parameter. 
    parameter=0 :: non_neg_integer()
}).


%% The operator is required.
%%
%% To match prices lower than 500$ use:
%%
%% ```erlang
%% #x_query_value{slot=price, op=less, value=500}
%% ```
%%
%%
%% If you want to match  by an exact value, then use:
%%
%% ```erlang
%% #x_query_value_range{slot=Slot, from=Value, to=Value}
%% ```
-record(x_query_value, {
    %% `'VALUE GE'` (`greater`) or `'VALUE LE'` (`less`, `lower`)
    op = ?REQUIRED :: 'VALUE GE' | 'VALUE LE' | greater | less | lower,
    slot :: xapian_type:x_slot() | xapian_type:x_slot_name(),
    value :: xapian_type:x_string()
}).

-record(x_query_value_range, {
    op='VALUE RANGE' :: xapian_type:x_operator(),
    slot :: xapian_type:x_slot() | xapian_type:x_slot_name(),
    from :: xapian_type:x_string(),
    to :: xapian_type:x_string()
}).

-record(x_query_term, {
    name :: xapian_type:x_string(),
    wqf = 1,
    position = 0 :: xapian_type:x_position()
}).


%% [https://github.com/freeakk/xapian/blob/master/doc/markdown/records.md#x_query_parser]
-record(x_query_parser, {
    name = default :: default | standard,
    stemmer :: xapian_type:x_stemmer() | undefined,
    stemming_strategy = default :: none | some | all | default,
    max_wildcard_expansion = unlimited :: non_neg_integer(),
    default_op = 'OR' :: xapian_type:x_operator(),
    prefixes = [] :: [xapian_type:x_prefix_name()]
}).


%% `#x_query_string' will be decoded using QueryParser.
-record(x_query_string, {
    %% * `default` - The default parser;
    %% * `standard` - The basic Xapian parser will be used.
    parser = default :: #x_query_parser{} | default | standard,

    %% Encoded query string
    value = ?REQUIRED :: xapian_type:x_string(),
    default_prefix = <<>>  :: xapian_type:x_string(),

    %% @see XapianErlangDriver::PARSER_FEATURES
    %% If undefined, then features=[default].
    %% `[default]' and `[]' are _different_.
    %%
    %% `[]' means no features.
    %%
    %% `[default]' means to use default options for `QueryParser'. 
    %% Default options are hardcoded inside Xapian.
    features :: [xapian_type:x_parser_feature()] | undefined
}).


%% [http://trac.xapian.org/wiki/FAQ/ExtraWeight](ExtraWeight on Wiki)
-record(x_query_scale_weight, {
    op = 'SCALE WEIGHT' :: xapian_type:x_operator(),
    %% Sub-query
    value = ?REQUIRED :: xapian_type:x_query(), 
    factor = ?REQUIRED :: float()
}). 


-record(x_sort_order, {
    type = ?REQUIRED :: xapian_type:x_order_type(),
    value :: xapian_type:x_slot_value() | xapian_type:x_resource(), %% KeyMaker resource
    is_reversed = false :: boolean()
}).


%% [https://github.com/freeakk/xapian/blob/master/doc/markdown/records.md#x_enquire]
-record(x_enquire, {
    value = xapian_type:x_query(),
    query_len = 0 :: non_neg_integer(),
    order = relevance :: #x_sort_order{} | relevance,
    docid_order = asc :: asc | desc | undefined | default | dont_care,
    weighting_scheme :: undefined | xapian_type:x_resource(),
    percent_cutoff = 0,
    weight_cutoff = 0,
    collapse_key :: undefined | xapian_type:x_slot_value(),
    collapse_max = 1 :: non_neg_integer()
}).


-record(x_bm25_weight, {
    k1 = 1 :: float(), 
    k2 = 0 :: float(), 
    l3 = 1 :: float(), 
    b  = 0.5 :: float(), 
    min_normlen = 0.5 :: float()
}).


%% [https://github.com/freeakk/xapian/blob/master/doc/markdown/records.md#x_match_set]
-record(x_match_set, {
    enquire = ?REQUIRED :: xapian_type:x_resource(),
    from = 0 :: non_neg_integer(), 
    max_items = undefined :: non_neg_integer() | undefined, 
    check_at_least = 0 :: non_neg_integer(), 
    spies = [] :: [xapian_type:x_resource()]
}).



