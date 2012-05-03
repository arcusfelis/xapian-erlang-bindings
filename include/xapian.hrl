-define(REQUIRED, erlang:throw(required_field)).

% ----------------------------------------------------------
%% These records are used by indexer
% ----------------------------------------------------------

-record(x_stemmer, {
    language = ?REQUIRED :: xapian:x_string()
}).


-record(x_data, {
    value = ?REQUIRED :: xapian:x_string()
}).


-record(x_term, {
    %% The name of the term. 
    value = ?REQUIRED :: xapian:x_string(),

    %% The position of the term. 
    %% If position = undefined, then we will use Xapian::Document::add_term,
    %% otherwise Xapian::Document::add_posting.
    position :: xapian:x_position(),

    %% The within-document frequency, or wdf, of a term t in D is the 
    %% number of times it is pulled out of D in the indexing process. 
    %% Usually this is the size of the wdp vector, but in Xapian it can 
    %% exceed it, since we can apply extra wdf to some parts of the 
    %% document text. For example, often this is done for the document 
    %% title and abstract to attach extra importance to their contents 
    %% compared to the rest of the document text.

    %% WDF of the current term will be increase by this value when indexing.
    %% If WDF = 0, then Xapian::Document::add_boolean_term will be called.
    wdf = 0 :: xapian:x_wdf()
}).


-record(x_value, {
    slot = ?REQUIRED  :: xapian:x_slot() | xapian:x_slot_name(),
    value = ?REQUIRED :: xapian:x_string()
}).


%% Calls Xapian::TermGenerator::increase_termpos.
-record(x_delta, {
    %% Amount to increase the term position by (default: 100). 
    position = 100 :: xapian:x_position()
}).


-record(x_text, {
    %% The text to index.
    value = ?REQUIRED :: xapian:x_string(),

    %% The wdf increment.
    position = 1 :: xapian:x_position(),

    %% The term prefix to use (default is no prefix). 
    prefix = <<>> :: xapian:x_string()
}).







% ----------------------------------------------------------
%% These records contain metainfo
% ----------------------------------------------------------

%% Example of using for queryparser.add_prefix("title", "S");
%% #x_prefix_name{name=title, prefix=<<"S">>}
%% It contains alternative name for keys.
%% Xapian is schemaless, so the schema is storing in the client code.
%% Prefixes are a part of the schema.
%%
%% `is_boolean', `is_exclusive' and `is_default' are only 
%% important for QueryParser.
-record(x_prefix_name, {
    %% full field name
    %%
    %% A prefix name `#x_prefix_name.name' is a pseudonym for prefix 
    %% `#x_prefix_name.prefix'.
    %%
    %% This name is for a client and the prefix is for Xapian.
    %%
    %% A prefix name is usually an atom, because of speed.
    %% But it can be any iolist, it will be converted into a binary with 
    %% `iolist_to_binary' before to pass to Xapian (for example, it will be
    %% passed for QueryParser).
    %%
    %% In Erlang code, the name can be a term, but it will be matched with an 
    %% operator `=:='. 
    %% So, `#x_prefix_name{name="author"}' and `#x_prefix_name{name=author}' 
    %% are different prefixes. That is why just use atoms for names everywhere.
    name :: iolist(),

    %% short field name
    %%
    %% This value is used internally by Xapian.
    %% It often is an uppercase letter, so it can be a char, for example,
    %% `#x_prefix_name{prefix=$A, name=author}'. 
    %%
    %% There is no difference in the format for this field.
    prefix :: binary() | char() | iolist(),


    %% If true, then QueryParser will combine this prefix 
    %% with Xapian::Query::OP_FILTER.
    %%
    %% This allows to use `field:value' for retrieve only documents, that have 
    %% the field with the name `field' and the value `value'.
    %%
    %% For example, we want to search for books about Erlang, then we can pass 
    %% `#x_prefix_name{name = language, prefix = $L}' as a config parameter and
    %% call a parser with a query string `language:erlang process linux OTP'.
    %%
    %% This field is only used for Xapian::Parser.
    %% @see Xapian::QueryParser::add_boolean_prefix
    is_boolean = false :: boolean(),

    %% If `true', each document can have at most one term with this prefix, 
    %% so multiple filters with this prefix should be combined with OP_OR. 
    %% If `false', each document can have multiple terms with this prefix, 
    %% so multiple filters should be combined with OP_AND, like happens with 
    %% filters with different prefixes.
    %%
    %% Ignored, if `is_boolean' is not `true'.
    is_exclusive = true :: boolean(),

    %% This rule only used, when this record is passed as a parameter of
    %% the `xapian_drv:open' method.
    %%
    %% If `true', this rule will be applied for a default QueryParser.
    %% If `false', this rule will be ignored for a default QueryParser.
    is_default = true :: boolean()
}).


-record(x_value_name, {
    %% full field name
    name :: atom(),

    slot :: non_neg_integer()
}).



% ----------------------------------------------------------
%% These records used for transactions
% ----------------------------------------------------------

-record(x_transaction_result, {
    is_committed,
    is_consistent,
    result,
    statuses,
    reason
}).


-record(x_error, {
    type,
    reason
}).



% ----------------------------------------------------------
%% These records used for queries
% ----------------------------------------------------------

-record(x_query, {
    op='AND',
    %% List of other queries or terms (term is a string).
    value :: [xapian:x_query() | xapian:x_string()],
    %% For `NEAR' and `PHRASE', a window size can be specified in parameter.
    %% For `ELITE_SET', the elite set size can be specified in parameter. 
    parameter=0
}).

-record(x_query_value, {
    %% `VALUE GE' or `VALUE LE'
    op = ?REQUIRED,
    slot :: non_neg_integer(),
    value :: xapian:x_string()
}).

-record(x_query_value_range, {
    op='VALUE RANGE',
    slot :: non_neg_integer(),
    from :: xapian:x_string(),
    to :: xapian:x_string()
}).

-record(x_query_term, {
    name :: xapian:x_string(),
    wqf = 1,
    position = 0 :: xapian:x_position()
}).

-record(x_query_parser, {
    name = default :: default | empty,
    stemmer :: #x_stemmer{} | undefined,
    stemming_strategy,
    %% 0 or `unlimited' for no limit
    max_wildcard_expansion = unlimited :: non_neg_integer(),
    default_op = 'OP_OR',
    prefixes :: [#x_prefix_name{}]
}).


%% `#x_query_string' will be decoded using QueryParser.
-record(x_query_string, {
    parser = default :: #x_query_parser{} | default | empty,

    %% Encoded query string
    string = ?REQUIRED :: xapian:x_string(),
    default_prefix = <<>>  :: xapian:x_string(),

    %% @see XapianErlangDriver::PARSER_FEATURES
    %% If undefined, then features=[default].
    %% `[default]' and `[]' are _different_.
    features :: [atom()]
}).


-record(x_sort_order, {
    type = relevance :: xapian:x_order_type(),
    value :: xapian:x_slot_value() | xapian:x_resource(), %% KeyMaker resource
    is_reversed = false :: boolean()
}).


-record(x_enquire, {
    x_query = xapian:x_query(),
    %% the query length to use in weight calculations 
    %% by default the sum of the wqf of all terms is used. 
    query_len = 0 :: non_neg_integer(),
    %% Primary document order
    order = relevance :: #x_sort_order{} | relevance,
    %% Secondary order
    %% asc = default
    %% dont_care = undefined
    docid_order = asc :: asc | desc | undefined | default | dont_care,
    %% Weighting scheme
    weighting_scheme :: xapian:x_resource(),
    percent_cutoff = 0,
    weight_cutoff = 0,
    collapse_key :: xapian:x_slot_value(),
    %% Max number of items with the same key to leave after collapsing
    collapse_max = 1 :: non_neg_integer()
}).
