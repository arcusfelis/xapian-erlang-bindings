%%% @doc This module is a `gen_server' that handles a single port connection.
-module(xapian_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/2,
         start_link/2,
         last_document_id/1,
         read_document/3,
         document_info/3,
         is_document_exist/2,
         close/1]).

%% For writable DB
-export([add_document/2,
         delete_document/2,
         replace_document/3,
         replace_or_create_document/3,
         update_document/3,
         update_or_create_document/3,
         transaction/3,
         transaction/2,
         set_metadata/3,
         add_spelling/2,
         add_synonym/3,
         remove_synonym/3,
         clear_synonyms/2]).

%% Queries
-export([query_page/5]). 

%% Resources
-export([enquire/2,
         document/2,
         match_set/2,
         query_parser/2]).


-export([parse_string/3]).

%% Resources
-export([create_resource/2,
         release_resource/2,
         release_table/2]).


%% Information
-export([mset_info/2,
         mset_info/3,
         match_spy_info/3,
         database_info/1,
         database_info/2]).


%% More information
-export([name_to_slot/1, 
         name_to_slot/2,
         slot_to_type/1,
         slot_to_type/2,
         subdb_names/1,
         multi_docid/3,
         qlc_table_to_reference/2
        ]).


%% ------------------------------------------------------------------
%% Other flags
%% ------------------------------------------------------------------

%% Intermodule export (non for a client!)
-export([internal_qlc_init/4,
         internal_register_qlc_table/3,
         internal_qlc_get_next_portion/4,
         internal_qlc_lookup/3,

         internal_transaction_lock_server/2,
         internal_transaction_cancel/2,

         internal_test_run/3,
         internal_execute_generator/4,
         internal_compile_resource/3,

         internal_create_resource/2,
         internal_create_resource/3
         ]).


%% with_state internals
-export([internal_name_to_slot/2,
         internal_name_to_type/2,
         internal_name_to_slot_dict/1,
         internal_slot_to_type_array/1,
         internal_subdb_names/1,
         internal_multi_docid/2,
         internal_qlc_table_hash_to_reference/2
        ]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).


%% ------------------------------------------------------------------
%% Import 
%% ------------------------------------------------------------------

-import(xapian_common, [ 
    append_binary/2,
    append_int8/2,
    append_uint8/2,
    append_uint16/2,
    append_uint/2,
    append_string/2,
    append_document_id/2,
    append_unique_document_id/2,
    resource_appender/2,
    resource_reader/2,
    resource_register/1,
    read_uint/1,
    read_string/1]).

-import(xapian_const, [
    command_id/1,
    open_mode_id/1,
    qlc_type_id/1,
    test_id/1]).

resource_reference_to_number(Register, {ClientPid, _ClientRef}, ResRef) ->
    xapian_register:get(Register, ClientPid, ResRef);
resource_reference_to_number(Register, ClientPid, ResRef) ->
    xapian_register:get(Register, ClientPid, ResRef).


%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

%% Used in handlers
-define(SERVER, ?MODULE).

%% Used for testing, then can be moved to an another file
-define(SRV, ?MODULE).
-define(APP, xapian).


%% ------------------------------------------------------------------
%% Records' Definitions
%% ------------------------------------------------------------------

-record(state, {
    %% The record is defined inside `xapian_port' as `port_rec'.
    %% It contains matadata for controlling a linked-in port driver or a port.
    port :: x_port(),

    %% Information was retrieved from `#x_prefix_name{}'.
    name_to_prefix :: orddict:orddict(),

    %% A value slot is an unsigned integer in C++.
    %% They are mapped into atoms in Erlang (it is optional).
    %% Information was retrieved from `#x_value_name{}'.
    name_to_slot :: orddict:orddict(),

    %% It is used for float values. Usually, type is `string', it is the 
    %% same as `undefined'.
    %% An index of the array is a slot number.
    slot_to_type :: array() | undefined,

    %% Used for creating resources.
    %% It contains mapping from an atom to information, about how to create 
    %% new resource on C++ side of the application.
    con_name_to_number :: orddict:orddict(),

    %% Each sub-database can have a name for identification.
    %% `undefined' names are not stored by this dict.
    subdb_name_to_id :: orddict:orddict(),

    %% If `base1', `base2' and `base3' is open, then contains  
    %% `{base1, base2, base3}'.
    %% If sub-database has no name, then `undefined' is used as an element of 
    %% the tuple.
    %% For example, it is often contains `{undefined}'. That means, that the
    %% only one database is open by the process. This database is without a 
    %% name.
    subdb_names :: tuple(),

    qlc_reference_and_table_hash = xapian_qlc_table_hash:new(),

    %% Pid of the real server (used by a transaction).
    %% If the process will be terminated, then new owner of the port will 
    %% be master.
    master :: pid() | undefined,
    %% Stores information about active resources of this server.
    register = xapian_register:new()
}).


%% Describes information about objects (resources) 
%% that can be created with special C++ code.
%%
%% When the port will be open, this server (gen_server) will load 
%% information about each resource type which can be created.
%%
%% Values `type = match_spy' and `name = value_count_match_spy' are passed 
%% using the call:
%%
%% ```
%% generator.add(new Constructor( 
%%     std::string("value_count_match_spy"), &createValueCountMatchSpy));
%% '''
%%
-record(resource_constructor_info, {
    %% `number' is `name' for C++. It is auto-generated.
    number :: non_neg_integer(),
    %% Name is `trad_weight' or `value_count_match_spy'.
    name :: atom()
}).


-record('DOWN',
{
    ref,   %% monitor reference
    type,  %% type of object 'process'
    id,    %% object id (pid)
    reason %% reason for termination
}).


%% ------------------------------------------------------------------
%% Import code
%% ------------------------------------------------------------------

-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").


%% ------------------------------------------------------------------
%% Declare parse transforms
%% ------------------------------------------------------------------

-compile({parse_transform, do}).
-compile({parse_transform, seqbind}).


%% ------------------------------------------------------------------
%% Import external types
%% ------------------------------------------------------------------

-type x_server() :: xapian_type:x_server().
-type x_table() :: xapian_type:x_table().
-type x_port() :: xapian_type:x_port().
-type x_transaction() :: xapian_type:x_transaction().
-type x_sub_query() :: xapian_type:x_sub_query().
-type x_resource() :: xapian_type:x_resource().
-type x_record() :: xapian_type:x_record().
-type x_meta() :: xapian_type:x_meta().
-type x_document_id() :: xapian_type:x_document_id().
-type x_string() :: xapian_type:x_string().
-type x_unique_document_id() :: xapian_type:x_unique_document_id().
-type x_document_constructor() :: xapian_type:x_document_constructor().
-type x_database_name() :: xapian_type:x_document_constructor().
-type x_slot_value() :: xapian_type:x_slot_value().
-type x_slot_type() :: xapian_type:x_slot_type().


%% ------------------------------------------------------------------
%% Internal types
%% ------------------------------------------------------------------

-type multi_db_path() :: [#x_database{}|#x_prog_database{}|#x_tcp_database{}].
-type db_path() :: x_string() | multi_db_path().


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Open the database with params.
%% `Path' is a directory name of the database.
%% For opening multiple databases you can pass a list of:
%%
%% <ul> <li> 
%% `#x_database{}';
%% </li><li>
%% `#x_prog_database{}';
%% </li><li>
%% `#x_tcp_database{}'.
%% </li></ul>
%%
%% See the description of these records for more information.
%%
%% `Params' is a list of:
%%
%% <ul> <li> 
%% Modes: read, write, overwrite, create, open:
%%  <ul> <li> 
%% The `read' mode is only for reading. 
%%  </li><li>
%% The `write' mode is for reading and for writing.
%%  </li><li>
%% Write mode can be combined with:
%% `open' (default), `create', `overwrite'.
%%  </li></ul>
%% </li><li>
%% Names for values and for prefixes:
%%  <ul> <li> 
%% `#x_value_name{slot = 1, name = slotname}'
%%  </li><li>
%% `#x_prefix_name{name = author, prefix = <<$A>>}';
%%  </li></ul>
%% </li><li>
%% The default stemmer. It will be used in `TermGenerator' and in the 
%% default query parser:
%% `#x_stemmer{language="english"}';
%% </li><li>
%% An interface to work: `port' (or `driver' by default).
%% </li><li>
%% `{name, Atom}' allows to register the server under the local name `Atom';
%% </li><li>
%% `{name, {local, Atom}}' does the same;
%% </li><li>
%% `{name, {global, Atom}}' registers the process under the global name.
%% </li></ul>
%% @see close/2
%% @see xapian_pool:open/3
-spec open(db_path(), [term()]) -> {ok, x_server()}.

open(Path, Params) ->
    xapian_server_sup:start_server(Path, Params).


%% @doc Start a linked server without supervision.
-spec start_link(db_path(), [term()]) -> {ok, x_server()}.

start_link(Path, Params) ->
    Args = [Path, append_default_params(Params)],
    case proplists:get_value(name, Params) of
        undefined ->
            gen_server:start_link(?MODULE, Args, []);
        A when is_atom(A) ->
            gen_server:start_link({local, A}, ?MODULE, Args, []);
        Name ->
            gen_server:start_link(Name, ?MODULE, Args, [])
    end.


%% @doc Return an identifier of the last added document.
%%      If the database is empty, returns `undefined'.
-spec last_document_id(x_server()) -> x_document_id() | undefined.
last_document_id(Server) ->
    call(Server, last_document_id).


%% @doc Close the database and kill a control process (aka Server).
%%      Database will be automaticly close, when a supervised server 
%%      process is dead.
%%
%%      This function is used for flushing changes of the writable database.
%%      The caller will be blocked while all changes will not flushed.
close(Server) ->
    gen_server:call(Server, close).


%% @doc Read the document with `DocId' from `Server' and put it into the record,
%%      defined by `RecordMetaDefinition'.
%%
%%     ```
%%      RecordMetaDefinition = 
%%          xapian_record:record(record_name, record_info(fields, record_fields)).
%%     '''
%%
read_document(Server, DocId, RecordMetaDefinition) ->
    call(Server, {read_document_by_id, DocId, RecordMetaDefinition}).


%% @doc Read document info, without putting it into database.
-spec document_info(x_server(), 
                    x_document_constructor(), x_meta()) -> x_record().

document_info(Server, DocumentConstructor, RecordMetaDefinition) ->
    call(Server, {document_info, DocumentConstructor, RecordMetaDefinition}).


%% @doc Return a list of records.
-spec query_page(x_server(), non_neg_integer(), non_neg_integer(), 
        x_sub_query(), x_meta()) -> [x_record()].
query_page(Server, Offset, PageSize, Query, RecordMetaDefinition) ->
    call(Server, {query_page, Offset, PageSize, Query, RecordMetaDefinition}).



%% -------------------------------------------------------------------
%% Resource manipulation
%% -------------------------------------------------------------------

%% @doc Return an enquire.
-spec enquire(x_server(), x_sub_query()) -> x_resource().
enquire(Server, Query) ->
    call(Server, {enquire, Query}).


%% @doc Return a document as a resource.
%% The second document can be:
%% <ul> <li>
%% A document id or an unique term;
%% </li><li> 
%% A document constructor.
%% </li></ul>
%%
%% If the second argument is the empty list, then the `badarg' error will
%% occure.
%%
%% It is an undefined behaviour.
%% Two cases can be here:
%%
%% <ol> <li>
%% You cannot use an empty term as an id (all documents will be selected).
%%
%% Solution: Use other ways to traverse the documents.
%% </li><li> 
%% Why do you need a document resource, defined with an empty constructor?
%%
%% Solution: pass `[#x_text{value=""}]'.
%% </li></ol> 
-spec document(x_server(), x_unique_document_id() 
               | x_document_constructor()) -> x_resource().
document(_Server, []) ->
    erlang:error(badarg);

document(Server, [H|_] = UniqueTerm) when is_list(UniqueTerm), is_integer(H) ->
    call(Server, {document, UniqueTerm});

document(Server, DocumentConstructor) when is_list(DocumentConstructor) ->
    call(Server, {document_info_resource, DocumentConstructor});

document(Server, DocId) -> %% DocId can be an unique term.
    call(Server, {document, DocId}).


%% @doc Return a match set (M-Set).
%% A match set can be created from:
%% <ul> <li>
%% an enquire (`x_resource()' type);
%% </li><li>
%% from record `#x_match_set{}', which contains an enquire and 
%% addition parameters.
%% </li></ul>
%%
%% Match set record is:
%%
%% ```
%% #x_match_set{
%%     enquire = EnquireResource, 
%%     offset = Offset, 
%%     max_items = MaxItems, 
%%     check_at_least = CheckAtLeast, 
%%     spies = Spies
%% }
%% '''
%%
%%  where 
%% <ul> <li>
%% `EnquireResource' contains the result of the search. 
%%
%% This parameter is required {@link enquire/2};
%% </li><li>
%% `Offset' means how many elements to skip. It is 0 by default;
%% </li><li>
%% `MaxItems' means how many elements to return. 
%%
%% Not more than `MaxItems' elements will be return. 
%% It is `undefined' by default, 
%% that means all items will be selected;
%% </li><li>
%% `Spies' is a list of MatchSpy resources {@link xapian_match_spy}.
%% </li></ul>
%%
%% @see enquire/2
%% @see xapian_mset_qlc:table/3
%% @see mset_info/3
%% @see xapian_match_spy
-spec match_set(x_server(), #x_match_set{} | Enquire) -> x_resource() when 
    Enquire :: x_resource().

match_set(Server, #x_match_set{} = Rec) ->
    call(Server, Rec);

match_set(Server, EnquireResource) ->
    Rec = #x_match_set{enquire = EnquireResource},
    match_set(Server, Rec).



%% @doc Create QueryParser as a resource.
-spec query_parser(x_server(), #x_query_parser{}) -> x_resource().

query_parser(Server, #x_query_parser{} = Rec) ->
    call(Server, Rec).


%% @doc Run a query parser for getting a Query object or a corrected 
%%      query string.
%%
%%      Corrected query string is used for syntax checking.
-spec parse_string(x_server(), #x_query_string{}, Fields) -> Result when
    Fields :: [Key],
    Key :: query_resource | corrected_query_string,
    Result :: [Pair] | Value,
    Pair :: {Key, Value},
    Value :: x_resource() | x_string().

parse_string(Server, #x_query_string{} = Rec, Fields) ->
    call(Server, {parse_string, Rec, Fields}).

%% @doc Release a resource.
%% It will be called automaticly, if the client process is died.
%% If a release constructor will be passed, then the error occurs.
%% @see create_resource/2
-spec release_resource(x_server(), x_resource()) -> ok.
release_resource(Server, ResourceRef) when is_reference(ResourceRef) ->
    call(Server, {release_resource, ResourceRef}).

%% @doc Create a resource from the resource constructror `Con'.
%% `Con' is created with the help of a function from a `xapian_resource' module.
%% `ResRef' must be released with {@link release_resource/2}.
%% `ResRef' will be released automaticly, when `Server' is dead.
%% @see release_resource/2
-spec create_resource(Server, Con) -> ResRef when
    Server :: x_server(),
    Con :: xapian_type:x_resource_constructor(),
    ResRef :: x_resource().
create_resource(Server, Con) ->
    xapian_resource:create(Server, Con).

%% @doc Clean resources allocated by the QLC table.
-spec release_table(x_server(), x_table()) -> ok.
release_table(Server, Table) ->
    TableHash = xapian_qlc_table_hash:hash(Table),
    call(Server, {qlc_release_table, TableHash}).


%% @see release_table/2
%% @private
internal_register_qlc_table(Server, ResRef, Table) ->
    TableHash = xapian_qlc_table_hash:hash(Table),
    gen_server:cast(Server, {qlc_register_table, ResRef, TableHash}).


%% ------------------------------------------------------------------
%% API Function Definitions for writable DB
%% ------------------------------------------------------------------

%% @doc Write a new document, return its id.
%% @see replace_document/3
%% @see replace_or_create_document/3
%% @see delete_document/2
-spec add_document(x_server(), x_document_constructor()) -> 
    x_document_id().

add_document(Server, Document) ->
    call(Server, {add_document, Document}).


%% @doc Mass manipulations with spelling information.
%%
%% `Term' is:
%%
%% ```
%% #xterm{ value = x_string()}
%% '''
%%
%% The position field is meaningless.
%% `action', `ignore', `frequency' can be used in the same manner,
%% as with the `add_document/2' function.
-spec add_spelling(Server, Spelling) -> no_return() when
    Server :: x_server(),
    Spelling :: xapian_type:x_spelling_constructor().

add_spelling(Server, Spelling) ->
    call(Server, {add_spelling, Spelling}).


%% @doc Add the synonym `Synonym' for the term `Term'.
%% @see remove_synonym/3
-spec add_synonym(Server, Term, Synonym) -> no_return() when
    Server :: x_server(),
    Term :: x_string(), 
    Synonym :: x_string().

add_synonym(Server, Term, Synonym) ->
    call(Server, {add_synonym, Term, Synonym}).


%% @doc Remove the synonym `Synonym' for the term `Term'.
%% @see clear_synonyms/2
%% @see add_synonym/3
-spec remove_synonym(Server, Term, Synonym) -> no_return() when
    Server :: x_server(),
    Term :: x_string(), 
    Synonym :: x_string().

remove_synonym(Server, Term, Synonym) ->
    call(Server, {remove_synonym, Term, Synonym}).


%% @doc Remove all synonyms for the term `Term'.
%% @see remove_synonym/3
-spec clear_synonyms(Server, Term) -> no_return() when
    Server :: x_server(),
    Term :: x_string().

clear_synonyms(Server, Term) ->
    call(Server, {clear_synonyms, Term}).


%% @doc Replace all matched documents with the new version.
%%
%% If nothing matches, then nothing will be changed.
%% If more then one documents matches, only one will left, the id of
%% this document will be returned.
%%
%% <note>This function and `Xapian::WritableDatabase::replace_document' 
%% have the different behaviour.</note>
%%
%% REP_DOC_MARK
-spec replace_document(x_server(), x_unique_document_id(), 
    x_document_constructor()) -> x_document_id().

replace_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {replace_document, DocIdOrUniqueTerm, NewDocument}).


%% @doc Replace all matched documents with the new version.
%%
%% This method replaces the document with the specified document ID. 
%% If the passed document ID isn't currently used, the document will be added 
%% with the passed document ID.
%%
%% If more then one documents matches, only one will left, the id of
%% this document will be returned.
%%
%% <note>This function and `Xapian::WritableDatabase::replace_document' 
%% have the same behaviour.</note>
%%
%% REP_CRT_DOC_MARK
%% @see add_document/2
%% @see replace_document/3
-spec replace_or_create_document(x_server(), x_unique_document_id(), 
    x_document_constructor()) -> x_document_id().

replace_or_create_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {replace_or_create_document, DocIdOrUniqueTerm, NewDocument}).


%% @doc Extend (edit) the document with data.
%%
%%      If the document is not exist, the error 
%%      `#x_error{type = <<"BadArgumentDriverError">>}' will occure. 
%%      UPD_DOC_BAD_ID_MARK
-spec update_document(x_server(), x_unique_document_id(), 
    x_document_constructor()) -> x_document_id().

update_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {update_document, DocIdOrUniqueTerm, NewDocument, false}).


%% @doc Update documents or create the new document.
%%
%% If `DocIdUniqueTerm' is a term and a document is not exist, new document will.
%% A unique term WILL NOT added automaticly.
-spec update_or_create_document(x_server(), x_unique_document_id(), 
                                x_document_constructor()) -> x_document_id().

update_or_create_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {update_document, DocIdOrUniqueTerm, NewDocument, true}).



%% @doc Delete documents.
%%
%% If a document id was passed, then this function deletes the document.
%%
%% If a term was passed, then any documents indexed by the specified term 
%% from the database will be deleted.
%%
%% A major use is for convenience when UIDs from another system are mapped to 
%% terms in Xapian, although this method has other uses 
%% (for example, you could add a "deletion date" term to documents at index 
%%  time and use this method to delete all documents due for deletion on a 
%%  particular date).
%%
%% Returns `true', if at least one document was deleted.
%% Returns `false', if nothing was deleted.
%%
%% <note>This function catches the 
%% `#x_error{type = <<"DocNotFoundError">>}' error and returns `false', if 
%% the document was not found.</note>
%% @see add_document/2
-spec delete_document(x_server(), x_unique_document_id()) -> IsDocumentExist 
        when IsDocumentExist :: boolean().

delete_document(Server, DocIdOrUniqueTerm) ->
    call(Server, {delete_document, DocIdOrUniqueTerm}).


%% @doc Return `true', if the document with a specified id exists.
-spec is_document_exist(x_server(), x_unique_document_id()) -> boolean().

is_document_exist(Server, DocIdOrUniqueTerm) ->
    call(Server, {is_document_exist, DocIdOrUniqueTerm}).


%% @doc Save a key-value pair into the database dictionary.
%% @see database_info/2
-spec set_metadata(x_server(), x_string(), x_string()) -> ok.

set_metadata(Server, Key, Value) ->
    call(Server, {set_metadata, Key, Value}).



%% ------------------------------------------------------------------
%% Tests (internal)
%% ------------------------------------------------------------------

%% @private
internal_test_run(Server, TestName, Params) ->
    call(Server, {test, TestName, Params}).


%% ------------------------------------------------------------------
%% Transactions
%% ------------------------------------------------------------------

%% @doc Run a transaction with 5-second timeout.
transaction(Servers, F) ->
    transaction(Servers, F, 5000).


%% @doc Runs function `F' for writable `Servers' as a transaction.
%%      Transaction will stop other operations with selected databases.
%%
%% This function runs a transaction on few servers.
%% `F' will be called as:
%%
%% ```
%% Servers = [Server1, Server2, Server3].
%%  F([TransServer1, TransServer2, TransServer3]).
%% '''
%%
%% Results (`#x_transaction_result.statuses') from a server:
%%
%% <ul> <li>
%% `committed' - A transaction was pass on this server. Data is consistent;
%% </li><li>
%% `aborted' - A transaction was canceled on this server. Data is consistent;
%% </li><li>
%% `failed' - An exeption was occured. Data is inconsistent.
%% </li></ul>
%% 
%% If one of the servers crashed during transaction, the transaction process 
%% will be killed using `cancel_transaction' with reason `crashed_server'.
-spec transaction([x_server()], x_transaction(), timeout()) -> 
    #x_transaction_result{}.

transaction(Servers, F, Timeout) ->
    xapian_transaction:run_transaction(Servers, F, Timeout).


%% @private
internal_transaction_lock_server(Server, Ref) ->
    gen_server:call(Server, {transaction, Ref}).


%% @private
internal_transaction_cancel(Server, Ref) ->
    Server ! {cancel_transaction, Ref}.

%% ------------------------------------------------------------------
%% Information about database objects
%% ------------------------------------------------------------------

-type string_term() :: x_string().
-type weight() :: float().
-type doc_count() :: non_neg_integer().
-type term_count() :: non_neg_integer().
-type doc_length() :: float().

-type mset_info_parameter() ::
        matches_lower_bound
      | matches_estimated
      | matches_upper_bound
      | uncollapsed_matches_lower_bound
      | uncollapsed_matches_estimated
      | uncollapsed_matches_upper_bound
      | size
      | max_possible
      | max_attained
      | {term_weight, string_term()}
      | {term_freq, string_term()}.


-type mset_info_result_pair() :: mset_info_result_pair1() | 
                                 mset_info_result_pair2().

-type mset_info_result_pair1() ::
    mset_info_result_pair_1(doc_count(), weight()).


-type mset_info_result_pair_1(C, W) ::
        {matches_lower_bound, C}
      | {matches_estimated, C}
      | {matches_upper_bound, C}
      | {uncollapsed_matches_lower_bound, C}
      | {uncollapsed_matches_estimated, C}
      | {uncollapsed_matches_upper_bound, C}
      | {size, C}
      | {max_possible, W}
      | {max_attained, W}.


-type mset_info_result_pair2() ::
    mset_info_result_pair_2(doc_count(), weight(), string_term()).


-type mset_info_result_pair_2(C, W, T) ::
       {{term_weight, T}, W | undefined} | 
       {{term_freq, T},   C | undefined}.


%% @doc Returns the list of all properties.
%% @equiv mset_info(Server, MSetResource, xapian_mset_info:properties())
-spec mset_info(x_server(), x_resource()) -> mset_info_result_pair1().

mset_info(Server, MSetResource) ->
    Params = xapian_mset_info:properties(),
    call(Server, {mset_info, MSetResource, Params}).

%% @doc Returns the list of selected properties and wanted values.
%% Properties:
%% <ul> <li> `matches_lower_bound';
%% </li><li> `matches_estimated';
%% </li><li> `matches_upper_bound';
%% </li><li> `uncollapsed_matches_lower_bound';
%% </li><li> `uncollapsed_matches_estimated';
%% </li><li> `uncollapsed_matches_upper_bound';
%% </li><li> `size'; 
%% </li><li> `max_possible'; 
%% </li><li> `max_attained';
%% </li><li> `{term_weight, Term}';
%% </li><li> `{term_freq, Term}'.
%% </li></ul>
-spec mset_info(x_server(), x_resource(), Params) -> Result
    when
        Params :: [Param],
        Param :: mset_info_parameter(),
        Result :: [ResultPair],
        ResultPair :: mset_info_result_pair();

               (x_server(), x_resource(), Param) -> ResultValue
    when
        Param :: mset_info_parameter(),
        ResultValue :: doc_count() | weight() | undefined. 

mset_info(Server, MSetResource, Params) ->
    call(Server, {mset_info, MSetResource, Params}).


%% @doc Return info about MatchSpy.
%% @see xapian_match_spy
-spec match_spy_info(x_server(), x_resource(), Params | Param) -> term() when
    Params :: [Param],
    Param :: ValueCountMatchSpyParam,
    ValueCountMatchSpyParam :: value_slot | document_count.

match_spy_info(Server, MatchSpyRes, Params) ->
    call(Server, {match_spy_info, MatchSpyRes, Params}).



-type database_info_result_pair() ::
    database_info_result_pair1() | database_info_result_pair2().


-type database_info_result_pair1() ::
      database_info_result_pair_1(doc_count(), doc_length()).

-type database_info_result_pair_1(C, L) ::
        {has_positions, boolean()}
      | {document_count, C}
      | {last_document_id, C}
      | {average_length, L}
      | {document_length_lower_bound, L}
      | {document_length_upper_bound, L}.


-type database_info_result_pair2() ::
      database_info_result_pair_2(x_string(), 
                                  x_document_id(), 
                                  x_slot_value(),
                                  string_term(), 
                                  term_count(), 
                                  doc_count(), 
                                  doc_length()).

-type database_info_result_pair_2(S, I, V, T, TC, DC, L) ::
        {uuid, S}
      | {{term_exists, T}, boolean()}
      | {{term_freq, T}, DC}
      | {{collection_freq, T}, TC}
      | {{value_freq, V}, DC}
      | {{value_lower_bound, V}, S}
      | {{value_upper_bound, V}, S}
      | {{wdf_upper_bound, T}, TC}
      | {{document_length, I}, L}
      | {{metadata, S}, S | undefined}.


-type database_info_param() ::
    database_info_param1() | database_info_param2().

-type database_info_param1() ::
        has_positions 
      | document_count
      | last_document_id
      | average_length
      | document_length_lower_bound
      | document_length_upper_bound
      | uuid.

-type database_info_param2() ::
      database_info_param_2(x_string(), x_slot_value(), 
                            x_document_id(), x_string()).

-type database_info_param_2(Term, Value, DocId, Key) ::
        {term_exists, Term}
      | {term_freq, Term}
      | {collection_freq, Term}
      | {value_freq, Value}
      | {value_lower_bound, Value}
      | {value_upper_bound, Value}
      | {wdf_upper_bound, Term}
      | {document_length, DocId}
      | {metadata, Key}.


%% @doc Returns the orddict of all properties.
-spec database_info(x_server()) -> [database_info_result_pair1()].

database_info(Server) ->
    Params = xapian_db_info:properties(),
    call(Server, {database_info, Params}).


%% @doc Returns the list of selected properties and wanted values.
%% Properties:
%% <ul> <li>
%% `has_positions' - Does this database have any positional information?;
%% </li><li>
%% `document_count' - Get the number of documents in the database;
%% </li><li>
%% `last_document_id' - Get the highest document id which has been 
%% used in the database. {@link last_document_id/1}; 
%% </li></ul>
%% `average_length'
%%      Get the average length of the documents in the database;
%% * `document_length_lower_bound'
%%      Get a lower bound on the length of a document in this DB;
%% * `document_length_upper_bound'
%%      Get an upper bound on the length of a document in this DB;
%% * `uuid'
%%      Get a UUID for the database;
%% * `{term_exists, Term}'
%%      Check if a given term exists in the database;
%% * `{term_freq, Term}'
%%      Get the number of documents in the database indexed by a given term;
%% * `{collection_freq, Term}'
%%      Return the total number of occurrences of the given term;
%% * `{value_freq, Value}'
%%      Return the frequency of a given value slot;
%% * `{value_lower_bound, Value}'
%%      Get a lower bound on the values stored in the given value slot;
%% * `{value_upper_bound, Value}'
%%      Get an upper bound on the values stored in the given value slot;
%% * `{wdf_upper_bound, Term}'
%%      Get an upper bound on the wdf of the term;
%% * `{document_length, DocId}'
%%      Get the length of a document;
%% * `{metadata, Key}'
%%      Get the user-specified metadata associated with a given key.
%%
%% If `DocId' or `Term' does not exist, `undefined' value is returned.
%%
%% For example,
%%
%% ```
%% database_info(Server, [{term_exists, "erlang"}, {term_freq, "erlang"}]).
%% [{{term_exists, "erlang"}, false}, {{term_freq, "erlang"}, undefined}]
%% '''
%%
-spec database_info(x_server(), Params) -> Result when
    Params :: [Param],
    Param :: database_info_param(),
    Result :: [ResultPair],
    ResultPair :: database_info_result_pair();

                   (x_server(), Param) -> ResultValue when
    Param :: database_info_param(),
    ResultValue :: boolean() | doc_count() | doc_length() | x_document_id() |
                   term_count() | x_string() | undefined.

database_info(Server, Params) ->
    call(Server, {database_info, Params}).


%% ------------------------------------------------------------------
%% Information about the state of the process
%% ------------------------------------------------------------------

%% @doc Returns orddict.
-spec name_to_slot(#state{} | x_server()) -> 
    [{xapian_type:x_slot_name(), xapian_type:x_slot()}].

name_to_slot(#state{name_to_slot = N2S}) ->
    N2S;

name_to_slot(Server) ->
    call(Server, {with_state, fun ?SRV:internal_name_to_slot_dict/1}).


%% @doc Convert a value slot name to its slot number.
-spec name_to_slot(#state{} | x_server(), x_slot_value()) -> xapian_type:x_slot().

name_to_slot(_ServerOrState, Slot) when is_integer(Slot) ->
    Slot;

name_to_slot(#state{name_to_slot = N2S}, Slot) when is_atom(Slot) ->
    orddict:fetch(Slot, N2S);

name_to_slot(Server, Slot) when is_atom(Slot) ->
    call(Server, {with_state, fun ?SRV:internal_name_to_slot/2, Slot}).


%% @doc Convert a QLC handler, allocated with help of 
%%      any of `xapian_*_qlc:table/?' functions, to its resource.
%% @end
%% @see release_table/2
-spec qlc_table_to_reference(#state{} | x_server(), x_table()) -> x_resource().

qlc_table_to_reference(ServerOrState, Table) ->
    Hash = xapian_qlc_table_hash:hash(Table),
    with(ServerOrState, fun ?SRV:internal_qlc_table_hash_to_reference/2, Hash).


with(#state{} = State, Fun, Params) ->
    case Fun(State, Params) of
        {ok, Result} -> Result;
        {error, Reason} -> erlang:error(Reason)
    end;

with(Server, Fun, Params) ->
    call(Server, {with_state, Fun, Params}).


%% @doc Returns an array for conversation from `x_slot()' to its type.
%%      If an element of the array is `undefined', then its type is `string'.
slot_to_type(#state{slot_to_type = V2T}) ->
    V2T;

slot_to_type(Server) ->
    call(Server, {with_state, fun ?SRV:internal_slot_to_type_array/1}).


%% @doc Convert a slot number or a slot name to its type (`float' or `string').
-spec slot_to_type(#state{} | x_server(), x_slot_value()) -> x_slot_type().

slot_to_type(State=#state{}, Slot) ->
    {ok, Type} = internal_name_to_type(State, Slot),
    Type;

slot_to_type(Server, Slot) ->
    call(Server, {with_state, fun ?SRV:internal_name_to_type/2, Slot}).


%% @doc Return an tuple of the database names.
%% For example, 2 sub-databases were opened under names `db1_name' and 
%% `db2z_name', then `{db1_name, db2_name}' will be returned.
-spec subdb_names(#state{} | x_server()) -> tuple().

subdb_names(#state{subdb_names = I2N}) ->
    I2N;

subdb_names(Server) ->
    call(Server, {with_state, fun ?SRV:internal_subdb_names/1}).


%% @doc Calculate a syntatic document ID from the real document ID and its DB name.
-spec multi_docid(#state{} | x_server(), RealDocId, SubDbName) -> MultiDocId when
        RealDocId  :: x_document_id(),
        MultiDocId :: x_document_id(),
        SubDbName  :: x_database_name().

multi_docid(State=#state{subdb_name_to_id = N2I}, DocId, SubDbName) 
    when is_atom(SubDbName) ->
    SubDbId = orddict:fetch(SubDbName, N2I),
    multi_docid(State, DocId, SubDbId);

multi_docid(#state{subdb_names = Names}, DocId, SubDbNum) 
    when is_integer(SubDbNum) ->
    DbCount = size(Names),
    (DbCount * (DocId-1)) + SubDbNum;

multi_docid(Server, DocId, SubDb) when not is_tuple(Server) ->
    call(Server, {with_state, fun ?SRV:internal_multi_docid/2, {DocId, SubDb}}).


%% ------------------------------------------------------------------
%% Information for internal use
%% ------------------------------------------------------------------

%% @private
internal_name_to_slot_dict(#state{name_to_slot = N2S}) -> 
    {ok, N2S}.

%% @private
internal_slot_to_type_array(#state{slot_to_type = V2T}) -> 
    {ok, V2T}.

%% @private
internal_name_to_slot(#state{name_to_slot = N2S}, Slot) -> 
    orddict_find(Slot, N2S).

%% @private
internal_subdb_names(#state{subdb_names = I2N}) -> 
    {ok, I2N}.

%% @private
internal_multi_docid(State, {DocId, SubDbName}) when is_atom(SubDbName) ->
    #state{subdb_name_to_id = N2I} = State,
    do([error_m ||
        SubDbNum <- orddict_find(SubDbName, N2I),
        {ok, multi_docid(State, DocId, SubDbNum)}]);

internal_multi_docid(State, {DocId, SubDbNum}) ->
    {ok, multi_docid(State, DocId, SubDbNum)}.


%% @private
internal_name_to_type(#state{slot_to_type = S2T}, Slot) 
    when is_integer(Slot) ->
    try
        {ok, xapian_common:slot_type(Slot, S2T)}
    catch error:Reason ->
        {error, Reason}
    end;

internal_name_to_type(#state{slot_to_type = S2T, 
                             name_to_slot = N2S}, Name) 
    when is_atom(Name) ->
    try
        Slot = xapian_common:slot_id(Name, N2S),
        {ok, xapian_common:slot_type(Slot, S2T)}
    catch error:Reason ->
        {error, Reason}
    end.



%% ------------------------------------------------------------------
%% API for other modules
%% ------------------------------------------------------------------

%% @doc Create a qlc resource, collect basic information about a set.
%% @private
-spec internal_qlc_init(x_server(), atom(), reference(), fun()) ->
    #internal_qlc_info{}.

internal_qlc_init(Server, Type, ResourceRef, EncoderFun) ->
    call(Server, {qlc_init, Type, ResourceRef, EncoderFun}).


%% @doc Read next `Count' elements starting from `From' from QlcResNum.
%% @private
-spec internal_qlc_get_next_portion(x_server(), 
    non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    binary().

internal_qlc_get_next_portion(Server, QlcResNum, From, Count) ->
    call(Server, {qlc_next_portion, QlcResNum, From, Count}).


%% @private
-spec internal_qlc_lookup(x_server(), fun(),
    non_neg_integer()) -> binary().

internal_qlc_lookup(Server, EncoderFun, ResNum) ->
    call(Server, {qlc_lookup, EncoderFun, ResNum}).


%% @doc Create a resource object of the specified type.
%%
%% ParamCreatorFun will be called as ParamCreatorFun(Register).
%% ParamCreatorFun returns `{ok, Bin}', where `Bin' is encoded binary, 
%% this binary will be passed as `ParamEncoder' into a resource 
%% creator function on C++ side.
%% @private
-spec internal_create_resource(x_server(), atom(), fun()) -> x_resource().

internal_create_resource(Server, ResourceConName, ParamCreatorFun) ->
    call(Server, {create_resource, ResourceConName, ParamCreatorFun}).


%% @private
-spec internal_create_resource(x_server(), atom()) -> x_resource().

internal_create_resource(Server, ResourceConName) ->
    call(Server, {create_resource, ResourceConName, undefined}).

 
%% ------------------------------------------------------------------
%% gen_server Client Helpers
%% ------------------------------------------------------------------

-spec call(x_server(), term()) -> term().

call(Server, Params) ->
    client_error_handler(gen_server:call(Server, Params)).


client_error_handler({ok, Result}) -> 
    Result;

client_error_handler({error, Reason}) -> 
    erlang:error(Reason);

client_error_handler({exception_migration, Type, Reason, Trace}) -> 
    ClientTrace = erlang:get_stacktrace(),
    erlang:raise(Type, #x_server_error{reason=Reason, trace=Trace}, ClientTrace).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private
init([Path, Params]) ->
    Prefixes = 
    [xapian_check:check_prefix(X) || X=#x_prefix_name{} <- Params],

    DefaultPrefixes = 
    [X || X <- Prefixes, X#x_prefix_name.is_default],

    Name2Prefix = 
    [{Name, Prefix} || #x_prefix_name{name = Name, prefix = Prefix} <- Prefixes],

    Name2Slot = 
    [{Name, Slot} 
        || #x_value_name{name = Name, slot = Slot} <- Params],

    Slot2Type =
    [{Slot, Type} 
        || #x_value_name{type = Type, slot = Slot} <- Params, Type =/= string],

    Slot2TypeArray = 
        if 
            Slot2Type =:= []   -> undefined; 
            true                -> array:from_orddict(Slot2Type, string) 
        end,

    Name2PrefixDict = orddict:from_list(Name2Prefix),
    Name2SlotDict   = orddict:from_list(Name2Slot),

    %% This stemmer will be used by default.
    %% Find the `#x_stemmer{}' record as a parameter.
    DefaultStemmer = lists:keyfind(x_stemmer, 1, Params),

    %% Select an interface for communicate with the C-part.
    PortType = 
        case lists:member(port, Params) of
            true -> port;
            false -> driver
        end,
    Port = xapian_port:open(PortType),

    stop_if_error(do([error_m ||
        SubDbNames <- 
            open_databases(Port, Path, Params),
        <<>> <- 
            set_default_stemmer(Port, DefaultStemmer),
        <<>> <- 
            set_default_prefixes(Port, DefaultPrefixes),
        ResourceCons <-
            port_get_resource_constructors(Port),
        begin
        %% number is a object id in __Resource:Generator__,
        %% not a number in the register.
        Name2NumCon = 
        [{InfoName, InfoNum} 
            || #resource_constructor_info{number=InfoNum, name=InfoName} 
                <- ResourceCons],
        Name2NumConDict = orddict:from_list(Name2NumCon),
        Name2Subdb = subdb_numbers(SubDbNames),
        Name2SubdbDict = orddict:from_list(Name2Subdb),
        {ok, #state{
            port = Port,
            name_to_prefix = Name2PrefixDict,
            name_to_slot = Name2SlotDict,
            slot_to_type = Slot2TypeArray,
            con_name_to_number = Name2NumConDict,
            subdb_name_to_id = Name2SubdbDict,
            subdb_names = list_to_tuple(SubDbNames)
        }}
        end]));

%% Add a copy of the server for the transaction
init([{from_state, State}]) ->
    {ok, State}.

 
%% @private
handle_call(Mess, From, State) ->
    %% Handle errors.
    %% Don't let the server die, bacause restart maybe be expensive.
    %% TODO: maybe it want to be restarted, because of the bad code upgrade?
    try
        hc(Mess, From, State)
    catch Type:Reason ->
        %% Retranslate this error on the client.
        Trace = erlang:get_stacktrace(),
        Reply = {exception_migration, Type, Reason, Trace},
        {reply, Reply, State}
    end.


hc({with_state, Fun}, _From, State) ->
    {reply, Fun(State), State};

hc({with_state, Fun, Params}, _From, State) ->
    {reply, Fun(State, Params), State};

hc(last_document_id, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_last_document_id(Port),
    {reply, Reply, State};

hc(close, _From, State=#state{master=undefined}) ->
    #state{ port = Port } = State,
    %% Flush on disk, close DB.
    close_db_port(Port),
    %% Destroy connection (send EOF to a prog port).
    xapian_port:close(Port),
    Reply = ok,
    Reason = normal,
    {stop, Reason, Reply, State};

hc(close, _From, State) ->
    {stop, normal, ok, State};

hc({add_document, Document}, From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, From, State),
    Reply = port_add_document(Port, EncodedDocument),
    {reply, Reply, State};

hc({add_spelling, Spelling}, From, State) ->
    #state{ port = Port } = State,
    EncodedSpelling = document_encode(Spelling, From, State),
    Reply = port_add_spelling(Port, EncodedSpelling),
    {reply, Reply, State};

hc({clear_synonyms, Term}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_clear_synonyms(Port, Term),
    {reply, Reply, State};

hc({remove_synonym, Term, Synonym}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_remove_synonym(Port, Term, Synonym),
    {reply, Reply, State};

hc({add_synonym, Term, Synonym}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_add_synonym(Port, Term, Synonym),
    {reply, Reply, State};

hc({replace_or_create_document, Id, Document}, From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, From, State),
    Reply = port_replace_or_create_document(Port, Id, EncodedDocument),
    {reply, Reply, State};

hc({replace_document, Id, Document}, From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, From, State),
    Reply = port_replace_document(Port, Id, EncodedDocument),
    {reply, Reply, State};

hc({update_document, Id, Document, Create}, From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, From, State),
    Reply = port_update_document(Port, Id, EncodedDocument, Create),
    {reply, Reply, State};

hc({is_document_exist, Id}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_is_document_exist(Port, Id),
    {reply, Reply, State};

hc({delete_document, Id}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_delete_document(Port, Id),
    {reply, Reply, State};

hc({set_metadata, Key, Value}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_set_metadata(Port, Key, Value),
    {reply, Reply, State};

hc({test, TestName, Params}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_test(Port, TestName, Params),
    {reply, Reply, State};

hc({document_info_resource, Document}, {FromPid, _FromRef}, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, FromPid, State),
    Result = port_document_info_resource(Port, EncodedDocument),
    m_do_register_resource(State, FromPid, Result);

hc({document_info, Document, Meta}, From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
          subdb_names = Id2Name, slot_to_type = Slot2Type } = State,
    EncodedDocument = document_encode(Document, From, State),
    Reply = port_document_info(Port, EncodedDocument, 
                               Meta, Name2Slot, Id2Name, Slot2Type),
    {reply, Reply, State};

hc({read_document_by_id, Id, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
          subdb_names = Id2Name, slot_to_type = Slot2Type } = State,
    Reply = port_read_document_by_id(Port, Id, 
                                     Meta, Name2Slot, Id2Name, Slot2Type),
    {reply, Reply, State};

hc({query_page, Offset, PageSize, Query, Meta}, From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
        subdb_names = Id2Name, slot_to_type = Slot2Type } = State,
    RA = resource_appender(State, From),
    Reply = port_query_page(Port, Offset, PageSize, Query, 
                            Meta, Name2Slot, Id2Name, Slot2Type, RA),
    {reply, Reply, State};

hc({enquire, Query}, {FromPid, _FromRef}, State) ->
    #state{ 
        port = Port, 
        name_to_slot = Name2Slot, 
        slot_to_type = Slot2TypeArray } = State,
    RA = resource_appender(State, FromPid),
    PortAnswer = port_enquire(Port, Query, Name2Slot, Slot2TypeArray, RA),
    %% Special handling of errors
    m_do_register_resource(State, FromPid, PortAnswer);

hc({document, DocId}, {FromPid, _FromRef}, State) ->
    #state{ port = Port } = State,
    PortAnswer = port_document(Port, DocId),
    m_do_register_resource(State, FromPid, PortAnswer);


hc(#x_query_parser{} = QP, {FromPid, _FromRef}, State) ->
    #state{port = Port } = State,
    RA = resource_appender(State, FromPid),
    PortAnswer = port_create_query_parser(Port, RA, QP),
    m_do_register_resource(State, FromPid, PortAnswer);


hc(#x_match_set{} = Mess, {FromPid, _FromRef}, State) ->
    #x_match_set{
        enquire = EnquireRef, 
        offset = Offset, 
        max_items = MaxItems, 
        check_at_least = CheckAtLeast, 
        spies = SpyRefs
    } = Mess, 
    #state{port = Port } = State,
    do_reply(State, do([error_m ||
        SpyRFs
            <- check_all([internal_compile_resource(State, SpyRef, FromPid) 
                    || SpyRef <- SpyRefs]),
        %% Resource function (RF): fun(Bin) -> Bin.
        EnquireRF
            <- internal_compile_resource(State, EnquireRef, FromPid),

        MSetNum <-
            port_match_set(Port, EnquireRF, Offset, 
                MaxItems, CheckAtLeast, SpyRFs),

        register_resource(State, FromPid, MSetNum)]));

hc({parse_string, QS, Fields}, {FromPid, _FromRef}, State) ->
    #state{register = Register, port = Port } = State,
    RA = resource_appender(State, FromPid),
    RR = resource_reader(Register, FromPid),
    do_reply(State, do([error_m ||
        {Res, RR2}
            <- port_parse_string(Port, RA, RR, QS, Fields),
        NewState 
            = State#state{register = resource_register(RR2)},
        {reply, {ok, Res}, NewState}
    ]));

%% It is synchronous, because it we put this code into `handle_cast', we cannot
%% find the place where the client sends a message.
%% If the error occures, we can throw an exception inside the client code.
%%
%% The return value is useless.
hc({release_resource, Ref}, {FromPid, _FromRef}, State) ->
    case run_release_resource(Ref, FromPid, State) of
        {ok, NewState} ->
            {reply, {ok, ok}, NewState};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

hc({qlc_release_table, Hash}, From, State) ->
    case internal_qlc_table_hash_to_reference(State, Hash) of
        {ok, Ref} ->
            hc({release_resource, Ref}, From, State);
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

%% Convert Res into QlcRes.
%% ResRef is an iterable object.
%% QlcRef, QlcResNum is for access for a QLC table.
hc({qlc_init, QlcType, ResRef, EncFun}, {FromPid, _FromRef}, State) ->
    #state{register = Register } = State,
    do_reply(State, do([error_m ||
        %% Get an iterable resource by the reference.
        %% DB term iterators use an undefined resource, 
        %% that is why maybe_* is used.
        ResNum 
            <- xapian_register:maybe_get(Register, FromPid, ResRef),

        %% Create QLC table (iterator-like object in Erlang)
        #internal_qlc_info{resource_number = QlcResNum} = Reply
            <- port_qlc_init(State, QlcType, ResNum, EncFun),

        {NewRegister, QlcRef}  
            <- xapian_register:put(Register, FromPid, QlcResNum),
        begin
            NewState = State#state{register = NewRegister},

            %% Add a reference
            Reply2 = Reply#internal_qlc_info{resource_ref = QlcRef},
            {reply, {ok, Reply2}, NewState}
        end]));
    

hc({qlc_lookup, EncoderFun, QlcResNum}, _From, State) ->
    Reply = port_qlc_lookup(State, QlcResNum, EncoderFun),
    {reply, Reply, State};
    

hc({qlc_next_portion, QlcResNum, From, Count}, _From, State) ->
    #state{port = Port } = State,
    Reply = port_qlc_next_portion(Port, QlcResNum, From, Count),
    {reply, Reply, State};



%% Create new resource object using the `ResourceConName' constructor
%% with a number `ResourceConNumber'.
%% Return an Erlang reference of new object.
hc({create_resource, ResourceConName, Gen}, 
    {FromPid, _FromRef}, State) ->
    #state{ port = Port } = State,
    do_reply(State, do([error_m ||
        ParamBin <-
                internal_execute_generator(State, ResourceConName, Gen, <<>>),

        ResourceConNumber
            <- port_create_resource(Port, ParamBin),

        register_resource(State, FromPid, ResourceConNumber)]));


hc({mset_info, MSetRef, Params}, From, State) ->
    #state{port = Port, register = Register } = State,
    Reply = 
    do([error_m ||
        MSetNum 
            <- resource_reference_to_number(Register, From, MSetRef),

        port_mset_info(Port, MSetNum, Params)
    ]),
    {reply, Reply, State};

hc({match_spy_info, MatchSpyRef, Params}, From, State) ->
    #state{port = Port, register = Register } = State,
    Reply = 
    do([error_m ||
        MatchSpyNum
            <- resource_reference_to_number(Register, From, MatchSpyRef),

        port_match_spy_info(Port, MatchSpyNum, Params)
    ]),
    {reply, Reply, State};

hc({database_info, Params}, _From, State) ->
    #state{port = Port} = State,
    Reply = port_database_info(Port, Params),
    {reply, Reply, State};

hc({transaction, Ref}, From, State) ->
    #state{ port = Port } = State,
    {FromPid, _FromRef} = From,
    case port_start_transaction(Port) of
        started ->
            %% Clone this server
            Args = [{from_state, State#state{master = self()}}],
            {ok, NewServer} = gen_server:start_link(?MODULE, Args, []),

            %% Change the owner of the port
            xapian_port:connect(Port, NewServer),

            %% Reply back
            Reply = NewServer,
            gen_server:reply(From, Reply),

            %% Wait, while NewServer exit
            MonRefServer = erlang:monitor(process, NewServer),
            Status = receive
                %% Clone server is down (with `close()' command), 
                %% self() process is an owner of the port now.
                {'DOWN', MonRefServer, process, NewServer, normal} -> 
                    port_commit_transaction(Port);

                %% This message is from transaction process.
                {cancel_transaction, Ref} -> 
                    close(NewServer),
                    port_cancel_transaction(Port)
            end,
            %% Send a message to the transaction process.
            xapian_transaction:report_transaction_status(
                FromPid, Ref, self(), Status),
            {noreply, State};

        Error ->
            {reply, Error, State}
    end.


%% @private
%%
%% Assosiate ResRef with TableHash.
handle_cast({qlc_register_table, ResRef, TableHash}, State) ->
    #state{qlc_reference_and_table_hash = TableRegister} = State,
    NewTableRegister = 
    xapian_qlc_table_hash:put(TableRegister, ResRef, TableHash),
    NewState = State#state{qlc_reference_and_table_hash = NewTableRegister},
    {noreply, NewState}.



%% @doc It is a helper, which is used with the monad error_m.
do_reply(OldState, {error, _Reason} = Error) ->
    {reply, Error, OldState};

do_reply(_State, Other) ->
    Other.


%% @doc It is a helper, which is used with the monad error_m.
stop_if_error({error, _Reason} = Error) ->
    {stop, Error};

stop_if_error(Other) ->
    Other.


%% @private
%% Ref is created for each process that uses resources.
handle_info(#'DOWN'{ref=Ref, type=process, id=ClientPid}, State) ->
    case run_erase_context(Ref, ClientPid, State) of
        {error, _Reason} ->
            {noreply, State}; %% ignore an error
        {ok, NewState} ->
            {noreply, NewState}
    end.


%% @private
terminate(_Reason, #state{master = undefined, port = Port}) ->
    close_db_port(Port),
    ok;

terminate(_Reason, #state{master = Master, port = Port}) ->
    %% Change the owner of the port
    xapian_port:connect(Port, Master),
    ok.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
append_resource_number(ResNum, Bin@) ->
    Schema = xapian_const:resource_encoding_schema_id(reference),
    Bin@ = append_uint8(Schema, Bin@),
    append_uint(ResNum, Bin@).


maybe_append_resource_number(undefined, Bin) ->
    Schema = xapian_const:resource_encoding_schema_id(undefined),
    append_uint8(Schema, Bin);

maybe_append_resource_number(ResNum, Bin) ->
    append_resource_number(ResNum, Bin).


%% @doc Create the `fun(Bin)' function.
internal_compile_resource(State, ResRef, ClientPid)
    when is_reference(ResRef), is_pid(ClientPid) ->
    #state{ register = Register } = State,
    Schema = xapian_const:resource_encoding_schema_id(reference),
    do([error_m ||
        ResNum 
            <- resource_reference_to_number(Register, ClientPid, ResRef),
        {ok, fun(Bin@) ->
                %% append_resource_number
                Bin@ = append_uint8(Schema, Bin@),
                append_uint(ResNum, Bin@) 
                end}
       ]);

%% @doc Create a HOF for appending the resource as a binary.
%% Res is a constructor record (returns by a function from xapian_resource).
%% It will be passed to `xapian_resource:compile' (1), 
%% that will pass it to `xapian_server' (2).
%% 
%% 1. extracts info from the `Res' record fields;
%% 2. extracts info from `State'.
%% @private
internal_compile_resource(State, Res, _ClientPid) when is_tuple(Res) ->
    Schema = xapian_const:resource_encoding_schema_id(constructor),
    do([error_m ||
        CompiledConBin 
            <- xapian_resource:compile(State, Res, <<>>),
               %% `internal_execute_generator' is called inside 
               %% `xapian_resource:compile/3'.
        {ok, fun(Bin) -> append_binary(CompiledConBin, 
                                       append_uint8(Schema, Bin)) end}
       ]).


%% @doc Form the binary string: 
%% `<<Bin, ResTypeConstructorNum, GenParam>>'.
%% This data allows to create a resource using a user object constructor 
%% (see user_object).
%% @private
internal_execute_generator(State = #state{}, ResourceTypeName, Gen, Bin) ->
    #state{ con_name_to_number = N2R } = State,
    do([error_m ||
        %% Info about a resource constructor
        UserResourceNumber
            <- orddict_find(ResourceTypeName, N2R),
        
        %% Write a constructor's parameters.
        ParamBin 
            <- run_resource_generator(State, Gen),

        %% Concat all together.
        {ok, append_resource_access(UserResourceNumber, ParamBin, Bin)}
       ]).


%% @doc Append a resource number.
%% <ul> <li>
%% `ResouceType' - `match_spy' or `value_range_processor',
%% defines the big group of resources (used to select the QlcTable).
%% </li><li>
%% `UserResourceNumber' - the number of the user's object constructor.
%% </li><li>
%% `ParamBin' - data, used by the user's object consctructor.
%% </li></ul>
append_resource_access(UserResourceNumber, ParamBin, Bin@) ->
    Bin@ = append_uint(UserResourceNumber, Bin@),
    <<Bin@/binary, ParamBin/binary>>.


document_encode(Document, From, #state{
        name_to_prefix = Name2Prefix,
        name_to_slot = Name2Slot,
        slot_to_type = Slot2TypeArray
    } = State) ->
    RA = resource_appender(State, From),
    xapian_document:encode(Document, Name2Prefix, Name2Slot, Slot2TypeArray, RA).


open_mode(Params) ->
    case lists:member(write, Params) of
    true -> open_write_mode(Params);
    false -> read_open
    end.

%% Default mode is open.
open_write_mode(Params) -> 
    Open      = lists:member(open, Params),
    Create    = lists:member(create, Params),
    Overwrite = lists:member(overwrite, Params),
    if
        Open, Create, not Overwrite -> write_create_or_open;
        not Open, Create, Overwrite -> write_create_or_overwrite;
        Create -> write_create;
        true -> write_open
    end.


control(Port, Operation) ->
    control(Port, Operation, <<>>).


control(Port, Operation, Data) ->
    <<Status:8/native-unsigned-integer, Result/binary>> = 
        xapian_port:control(Port, command_id(Operation), Data),
    case Status of
        0 -> {ok, Result};
        1 -> 
            {Type, Bin1} = read_string(Result),
            {Mess, <<>>} = read_string(Bin1),
            error_logger:error_msg("[~w:~w] ~s: \"~s\" ~nData: ~p~n", 
                                   [Operation, command_id(Operation), 
                                    Type, Mess, Data]),
            {error, #x_error{type=Type, reason=Mess, command=Operation}}
    end.


open_database(Port, Mode, Path) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(open_mode_id(Mode), Bin@),
    Bin@ = append_string(Path, Bin@),
    control(Port, open, Bin@).


open_prog_database(Port, Mode, Prog, Args, Timeout) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(open_mode_id(Mode), Bin@),
    Bin@ = append_string(Prog, Bin@),
    Bin@ = append_string(Args, Bin@),
    Bin@ = append_uint(Timeout, Bin@),
    control(Port, open_prog, Bin@).


open_tcp_database(Port, Mode, TcpHost, TcpPort, Timeout, ConnectTimeout) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(open_mode_id(Mode), Bin@),
    Bin@ = append_string(TcpHost, Bin@),
    Bin@ = append_uint16(TcpPort, Bin@),
    Bin@ = append_uint(Timeout, Bin@),
    Bin@ = append_uint(ConnectTimeout, Bin@),
    control(Port, open_tcp, Bin@).


close_db_port(Port) ->
    [control(Port, close, <<>>) || xapian_port:is_port_alive(Port)],
    ok.


%%open_databases(_Port, [], _Params) ->
%%    {error, empty_path_list};

open_databases(Port, PathList, Params) ->
    Mode = open_mode(Params),
    open_databases(Port, PathList, Mode, []).


%% Open few DBs together
open_databases(_Port, [H,_|_], Mode, []) 
    when is_tuple(H), Mode =/= read_open ->
    %% Only one database may be specified when --writable is used.
    {error, multiple_writable_dbs};

open_databases(Port, [#x_database{name=Name, path=Path}|T], Mode, Names) ->
    Res = open_database(Port, Mode, Path),
    open_next_database(Port, Res, T, Mode, [Name|Names]);

open_databases(Port, [#x_prog_database{}=H|T], Mode, Names) ->
    #x_prog_database{name=Name, program=Prog, 
                     arguments=Args, timeout=Timeout} = H,
    Res = open_prog_database(Port, Mode, Prog, Args, Timeout),
    open_next_database(Port, Res, T, Mode, [Name|Names]);

open_databases(Port, [#x_tcp_database{}=H|T], Mode, Names) ->
    #x_tcp_database{name=Name, host=TcpHost, port=TcpPort, 
                    timeout=Timeout, connect_timeout=ConnectTimeout} = H,
    Res = open_tcp_database(Port, Mode, TcpHost, TcpPort, 
                            Timeout, ConnectTimeout),
    open_next_database(Port, Res, T, Mode, [Name|Names]);

open_databases(_Port, [], _Mode, Names) ->
    {ok, lists:reverse(Names)};

open_databases(Port, Path, Mode, []) when is_tuple(Path) ->
    open_databases(Port, [Path], Mode, []);

%% Path is a iolist
open_databases(Port, Path, Mode, []) ->
    case open_database(Port, Mode, Path) of
        {ok, <<>>} ->
            {ok, [undefined]};
        {error, _Mess} = Error ->
            Error
    end.        


open_next_database(Port, {ok, <<>>}, PathList, Mode, Names) ->
    open_databases(Port, PathList, Mode, Names);

open_next_database(_Port, {error, _Mess} = Error, _PathList, _Mode, _Names) ->
    Error.


set_default_stemmer(_Port, false) ->
    {ok, <<>>};

set_default_stemmer(Port, DefaultStemmer) ->
    Data = xapian_encode:append_stemmer(DefaultStemmer, <<>>),
    control(Port, set_default_stemmer, Data).


set_default_prefixes(_Port, []) ->
    {ok, <<>>};

set_default_prefixes(Port, DefaultPrefixes) ->
    Data = 
    lists:foldl(fun xapian_encode:append_prefix/2, 
        append_uint(erlang:length(DefaultPrefixes), <<>>), 
        DefaultPrefixes),
    control(Port, set_default_prefixes, Data).


port_add_document(Port, EncodedDocument) ->
    decode_docid_result(control(Port, add_document, EncodedDocument)).


port_add_spelling(Port, EncodedSpelling) ->
    control(Port, add_spelling, EncodedSpelling).

port_remove_synonym(Port, Term, Synonym) ->
    Bin@ = append_string(Term, <<>>),
    Bin@ = append_string(Synonym, Bin@), 
    control(Port, remove_synonym, Bin@).

port_add_synonym(Port, Term, Synonym) ->
    Bin@ = append_string(Term, <<>>),
    Bin@ = append_string(Synonym, Bin@), 
    control(Port, add_synonym, Bin@).

port_clear_synonyms(Port, Term) ->
    Bin@ = append_string(Term, <<>>),
    control(Port, clear_synonyms, Bin@).

port_update_document(Port, Id, EncodedDocument, Create) ->
    Action = 
    if  Create -> update_or_create_document;
        true -> update_document
        end,
    decode_docid_result(
        control(Port, Action, 
            append_unique_document_id(Id, EncodedDocument))).


port_replace_or_create_document(Port, Id, EncodedDocument) ->
    decode_docid_result(
        control(Port, replace_or_create_document, 
            append_unique_document_id(Id, EncodedDocument))).


port_replace_document(Port, Id, EncodedDocument) ->
    decode_docid_result(
        control(Port, replace_document, 
            append_unique_document_id(Id, EncodedDocument))).


port_delete_document(Port, Id) ->
    decode_boolean_result(
        control(Port, delete_document, append_unique_document_id(Id, <<>>))).


port_is_document_exist(Port, Id) ->
    decode_boolean_result(
        control(Port, is_document_exist, append_unique_document_id(Id, <<>>))).


port_set_metadata(Port, Key, Value) ->
    Bin@ = <<>>,
    Bin@ = append_string(Key, Bin@),
    Bin@ = append_string(Value, Bin@),
    control(Port, set_metadata, Bin@).


port_last_document_id(Port) ->
    decode_docid_result(control(Port, last_document_id)).

port_test(Port, echo, ValueBin) ->
    Num = test_id(echo),
    Bin@ = <<>>,
    Bin@ = append_int8(Num, Bin@),
    Bin@ = append_uint(byte_size(ValueBin), Bin@),
    Bin@ = <<Bin@/binary, ValueBin/binary>>,
    control(Port, test, Bin@);

port_test(Port, result_encoder, [From, To]) ->
    Num = test_id(result_encoder),
    Bin@ = <<>>,
    Bin@ = append_int8(Num, Bin@),
    Bin@ = append_document_id(From, Bin@),
    Bin@ = append_document_id(To, Bin@),
    control(Port, test, Bin@);

port_test(Port, Type, _) when Type =:= exception; Type =:= memory ->
    Num = test_id(Type),
    Bin = append_int8(Num, <<>>),
    control(Port, test, Bin).



%% @doc Helper for port_*_transaction.
replace_result(Value, {ok, <<>>}) -> 
    Value;

replace_result(_Value, {error, Reason}) -> 
    Reason.


port_start_transaction(Port) -> 
    replace_result(started, control(Port, start_transaction)).


%% Returns `committed' if success
port_commit_transaction(Port) -> 
    replace_result(committed, control(Port, commit_transaction)).


%% Returns `aborted' if success
port_cancel_transaction(Port) -> 
    replace_result(aborted, control(Port, cancel_transaction)).


%% @doc Read and decode one document from the port.
port_read_document_by_id(Port, Id, Meta, Name2Slot, Id2Name, 
                         Slot2Type) ->
    Bin@ = <<>>,
    Bin@ = append_document_id(Id, Bin@),
    Bin@ = xapian_record:encode(Meta, Name2Slot, Slot2Type, Bin@),
    decode_record_result(control(Port, read_document_by_id, Bin@), Meta, Id2Name).


port_document_info(Port, EncodedDocument, 
                   Meta, Name2Slot, Id2Name, Slot2Type) ->
    Bin@ = EncodedDocument,
    Bin@ = xapian_record:encode(Meta, Name2Slot, Slot2Type, Bin@),
    decode_record_result(control(Port, document_info, Bin@), Meta, Id2Name).

port_document_info_resource(Port, EncodedDocument) ->
    decode_resource_result(control(Port, document_info_resource, EncodedDocument)).


port_query_page(Port, Offset, PageSize, Query, Meta, Name2Slot, Id2Name, 
                Slot2Type, RA) ->
    Bin@ = <<>>,
    Bin@ = append_uint(Offset, Bin@),
    Bin@ = append_uint(PageSize, Bin@),
    Bin@ = xapian_query:encode(Query, Name2Slot, Slot2Type, RA, Bin@),
    Bin@ = xapian_record:encode(Meta, Name2Slot, Slot2Type, Bin@),
    decode_records_result(control(Port, query_page, Bin@), Meta, Id2Name).


port_enquire(Port, Enquire, Name2Slot, Slot2TypeArray, RA) ->
    Bin@ = <<>>,
    Bin@ = xapian_enquire:encode(Enquire, Name2Slot, Slot2TypeArray, RA, Bin@),
    decode_resource_result(control(Port, enquire, Bin@)).


port_document(Port, DocId) ->
    Bin@ = <<>>,
    Bin@ = append_unique_document_id(DocId, Bin@),
    decode_resource_result(control(Port, document, Bin@)).


port_match_set(Port, EnqRF, From, MaxItems, CheckAtLeast, SpyRFs) ->
    Bin@ = <<>>,
    Bin@ = append_compiled_resource(EnqRF, Bin@),
    Bin@ = append_uint(From, Bin@),
    Bin@ = append_max_items(MaxItems, Bin@),
    Bin@ = append_uint(CheckAtLeast, Bin@),
    Bin@ = append_uint(length(SpyRFs), Bin@),
    Bin@ = lists:foldl(fun append_compiled_resource/2, Bin@, SpyRFs),
    decode_resource_result(control(Port, match_set, Bin@)).


port_parse_string(Port, RA, RR, QS, Fields) ->
    Bin = xapian_parse_string:encode(QS, RA, Fields, <<>>),
    Data = control(Port, parse_string, Bin),
    decode_parse_string_result(Data, RR, Fields).


port_create_query_parser(Port, RA, QP) ->
    EncodedQP = xapian_query:append_parser(RA, QP, <<>>),
    decode_resource_result(control(Port, create_query_parser, EncodedQP)).

append_compiled_resource(Res, Bin) -> Res(Bin).

append_max_items(MaxItems, Bin@) 
    when is_integer(MaxItems), MaxItems >= 0 ->
    append_uint(MaxItems, append_uint8(0, Bin@));

append_max_items(undefined, Bin@) ->
    append_uint8(1, Bin@).


port_release_resource(Port, ResourceNum) ->
    Bin@ = <<>>,
    %% append_resource_number
    %%
    %% Append a number of the real resource without a schema type.
    Bin@ = append_uint(ResourceNum, Bin@),
    control(Port, release_resource, Bin@).

%% This is a multi-version of port_release_resource. 
%% If the resource is not exist, then the error will be ignored.
port_release_resources(Port, ResourceNumbers) ->
    Bin@ = <<>>,
    %% append_resource_number
    %%
    %% Append a number of the real resource without a schema type.
    Bin@ = lists:foldl(fun xapian_common:append_uint/2,  Bin@, ResourceNumbers),
    Bin@ = append_uint(0, Bin@),
    control(Port, release_resources, Bin@).


port_qlc_next_portion(Port, QlcResNum, From, Count) ->
    Bin@ = <<>>,
    Bin@ = append_resource_number(QlcResNum, Bin@),
    Bin@ = append_uint(From, Bin@),
    Bin@ = append_uint(Count, Bin@),
    control(Port, qlc_next_portion, Bin@).


port_create_resource(Port, ParamBin) ->
    decode_resource_result(control(Port, create_resource, ParamBin)).
    


port_get_resource_constructors(Port) ->
    decode_resource_constructors(control(Port, get_resource_constructors, <<>>)).


port_qlc_init(State, QlcType, ResourceNum, EncoderFun) ->
    #state{ port = Port } = State,
    Bin@ = <<>>,
    Bin@ = append_uint8(qlc_type_id(QlcType), Bin@),
    Bin@ = maybe_append_resource_number(ResourceNum, Bin@),
    Bin@ = append_with_encoder(State, EncoderFun, Bin@), 
    decode_qlc_info_result(control(Port, qlc_init, Bin@)).


port_qlc_lookup(State, QlcResNum, EncoderFun) ->
    #state{ port = Port } = State,
    Bin@ = <<>>,
    Bin@ = append_resource_number(QlcResNum, Bin@),
    Bin@ = append_with_encoder(State, EncoderFun, Bin@), 
    control(Port, qlc_lookup, Bin@).


append_with_encoder(State, EncoderFun, Bin) 
    when is_function(EncoderFun) ->
    {arity, Arity} = erlang:fun_info(EncoderFun, arity),
    case Arity of
        1 -> EncoderFun(Bin);
        2 -> EncoderFun(State, Bin)
    end.


port_mset_info(Port, MSetNum, Params) ->
    Bin@ = <<>>,
    Bin@ = append_resource_number(MSetNum, Bin@),
    Bin@ = xapian_mset_info:encode(Params, Bin@),
    decode_mset_info_result(control(Port, mset_info, Bin@), Params).

port_match_spy_info(Port, MatchSpyNum, Params) ->
    Bin@ = <<>>,
    Bin@ = append_resource_number(MatchSpyNum, Bin@),
    Bin@ = xapian_spy_info:encode(Params, Bin@),
    decode_spy_info_result(control(Port, match_spy_info, Bin@), Params).

port_database_info(Port, Params) ->
    Bin@ = <<>>,
    Bin@ = xapian_db_info:encode(Params, Bin@),
    decode_database_info_result(control(Port, database_info, Bin@), Params).



%% -----------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------
            
%% @doc Delete information about allocated resource, deallocate resource,
%%      clean information about the qlc table.
run_release_resource(ResRef, ClientPid, State) ->
    #state{port = Port, register = Register,
           qlc_reference_and_table_hash = TableRegister} = State,
    do([error_m ||
    {NewRegister, ResNum} <- 
        xapian_register:delete(Register, ClientPid, ResRef),
    %% Delete qlc.
    NewTableRegister <- 
        maybe_erase_qlc_table(ResRef, TableRegister),
    <<>> <- 
        port_release_resource(Port, ResNum),
    {ok, State#state{register = NewRegister,
                     qlc_reference_and_table_hash = NewTableRegister}}
    ]).

run_erase_context(MonRef, ClientPid, State) ->
    #state{port = Port, register = Register,
           qlc_reference_and_table_hash = TableRegister} = State,
    do([error_m ||
    {NewRegister, ResRef2Num} 
        <- xapian_register:erase(Register, ClientPid, MonRef),
    {ResRefs, ResNums}
        = lists:unzip(ResRef2Num),
    %% Delete qlc.
    NewTableRegister 
        <- error_foldl(fun maybe_erase_qlc_table/2, TableRegister, ResRefs),
    <<>> 
        <- port_release_resources(Port, ResNums),
    {ok, State#state{register = NewRegister,
                     qlc_reference_and_table_hash = NewTableRegister}}
    ]).

%% @doc If the type of the resource is qlc, then delete information about
%%      the qlc table from the state.
maybe_erase_qlc_table(ResRef, TableRegister) ->
    case xapian_qlc_table_hash:erase(TableRegister, ResRef) of
        {ok, {NewTableRegister, ResRef, _Hash}} ->
            %% Shrink the result.
            {ok, NewTableRegister};
        {error, _Reason} ->
            {ok, TableRegister}
    end.


%% @private
internal_qlc_table_hash_to_reference(State, Hash) ->
    #state{qlc_reference_and_table_hash = TableRegister} = State,
    case xapian_qlc_table_hash:get(TableRegister, Hash) of
        {ok, {Ref, Hash}} ->
            {ok, Ref};
        {error, _Reason} = Error ->
            Error
    end.
        

%% Result is from a port.
decode_result_with_hof({ok, Bin}, Meta, Fn) ->
    case Fn(Meta, Bin) of
        {Recs, <<>>} -> {ok, Recs}
    end;

decode_result_with_hof(Other, _Meta, _Fn) -> 
    Other.


decode_result_with_hof({ok, Bin}, Meta, I2N, Fn) ->
    case Fn(Meta, I2N, Bin) of
        {Recs, <<>>} -> {ok, Recs}
    end;

decode_result_with_hof(Other, _Meta, _I2N, _Fn) -> 
    Other.


decode_result_with_hof({ok, Bin}, Fn) ->
    case Fn(Bin) of
        {Res, <<>>} -> {ok, Res}
    end;

decode_result_with_hof(Other, _Fn) -> 
    Other.


decode_record_result(Data, Meta, I2N) ->
    decode_result_with_hof(Data, Meta, I2N, fun xapian_record:decode/3).

decode_records_result(Data, Meta, I2N) ->
    decode_result_with_hof(Data, Meta, I2N, fun xapian_record:decode_list/3).

decode_mset_info_result(Data, Params) ->
    decode_result_with_hof(Data, Params, fun xapian_mset_info:decode/2).

decode_spy_info_result(Data, Params) ->
    decode_result_with_hof(Data, Params, fun xapian_spy_info:decode/2).

decode_database_info_result(Data, Params) ->
    decode_result_with_hof(Data, Params, fun xapian_db_info:decode/2).

decode_docid_result(Data) -> 
    decode_result_with_hof(Data, fun xapian_common:read_document_id/1).

decode_resource_result(Data) -> 
    decode_result_with_hof(Data, fun xapian_common:read_uint/1).

decode_boolean_result(Data) -> 
    decode_result_with_hof(Data, fun xapian_common:read_boolean/1).

decode_parse_string_result(Data, RR, Fields) -> 
    decode_result_with_hof(Data, Fields, RR, fun xapian_parse_string:decode/3).



decode_qlc_info_result({ok, Bin@}) -> 
    {ResNum, Bin@} = read_uint(Bin@),
    {Size,   <<>>} = read_uint(Bin@),
    Result = encode_qlc_info(Size, ResNum),
    {ok, Result};

decode_qlc_info_result(Other) -> 
    Other.


%% @doc Forms a state, will be used in the `table' function of different modules. 
%% `Size' is a count of elements in the table.
%% `ResNum' is a QLC resource number.
encode_qlc_info(Size, ResNum) ->
    #internal_qlc_info{
        num_of_objects = Size,
        resource_number = ResNum
    }.



%% @doc Read a list of `#resource_constructor_info{}' from binary, 
%%      while it is not empty.
decode_resource_constructors({ok, Bin}) ->
    {ok, decode_resource_constructor_info_cycle(Bin, [])};

decode_resource_constructors(Other) -> 
    Other.


-spec decode_resource_constructor_info_cycle(binary(), [Con]) -> [Con] when
    Con :: #resource_constructor_info{}.

decode_resource_constructor_info_cycle(<<>>, Acc) ->
    lists:reverse(Acc);

decode_resource_constructor_info_cycle(Bin@, Acc) ->
    {Num, Bin@} = read_uint(Bin@),
    {Name, Bin@} = read_string(Bin@),
    Rec = #resource_constructor_info{
        number = Num,
        name = list_to_atom(binary_to_list(Name))
    },
    decode_resource_constructor_info_cycle(Bin@, [Rec|Acc]).


%% orddict:find for error_m
orddict_find(Id, Dict) ->
    case orddict:find(Id, Dict) of
        error ->
            {error, {bad_id, Id, Dict}};
        {ok, Value} ->
            {ok, Value}
    end.



%% -----------------------------------------------------------------
%% Internal helpers.
%% -----------------------------------------------------------------

register_resource(State, FromPid, ResourceNum) ->
    #state{ register = Register } = State,
    %% Reply is a reference
    {ok, {NewRegister, Ref}} = 
    xapian_register:put(Register, FromPid, ResourceNum),
    NewState = State#state{register = NewRegister},
    {reply, {ok, Ref}, NewState}.

m_do_register_resource(State, FromPid, Result) ->
    %% Special handling of errors
    case Result of
        {error, _Reason} = Error ->
            {reply, Error, State};
        {ok, ResourceNum} ->
            register_resource(State, FromPid, ResourceNum)
    end.

         
%% @doc Helper for monades and list comprehensions.
check_all(List) ->
    check_all(List, []).

check_all([{ok, X}|T], Acc) ->
    check_all(T, [X|Acc]);

check_all([{error, _} = H|_T], _Acc) ->
    H;

check_all([], Acc) ->
    {ok, lists:reverse(Acc)}.


%% @doc Foldl, while the result is not error.
error_foldl(F, Acc, [H|T]) ->
    case F(H, Acc) of
        {ok, Acc2} ->
            error_foldl(F, Acc2, T);
        {error, _Reason} = Error ->
            Error
    end;
error_foldl(_F, Acc, []) ->
    {ok, Acc}.


subdb_numbers(Names) ->
    %% Encounting from 1.
    subdb_numbers(Names, 1, []).


subdb_numbers([undefined|T], Num, Acc) ->
    subdb_numbers(T, Num+1, Acc);

subdb_numbers([H|T], Num, Acc) ->
    subdb_numbers(T, Num+1, [{H, Num}|Acc]);

subdb_numbers([], _Num, Acc) ->
    lists:reverse(Acc).
    

append_default_params(Params) ->
    case application:get_env(?APP, default_open_parameters) of
        undefined ->
            Params;
        {ok, [_|_] = DefParams} ->
            DefParams ++ Params 
    end.


%% @doc Run a function from the `xapian_resource' module.
run_resource_generator(State, Gen) when is_function(Gen) ->
    {arity, Arity} = erlang:fun_info(Gen, arity),
    case Arity of
        0 -> Gen();
        1 -> Gen(State)
    end;
run_resource_generator(_State, undefined) ->
    {ok, <<>>}.

