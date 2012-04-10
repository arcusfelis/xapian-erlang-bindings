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
-record(x_prefix_name, {
    %% full field name
    name :: atom(),

    %% short field name
    prefix :: binary()
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
