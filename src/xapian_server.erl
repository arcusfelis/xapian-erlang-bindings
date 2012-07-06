% This module is a `gen_server' that handles a single port connection.
-module(xapian_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/2,
         last_document_id/1,
         read_document/3,
         document_info/3,
         is_document_exist/2,
         close/1]).

%% For writable DB
-export([add_document/2,
         delete_document/2,
         replace_document/3,
         update_or_create_document/3,
         update_document/3,
         transaction/3,
         transaction/2,
         set_metadata/3]).

%% Queries
-export([query_page/5]). 

%% Resources
-export([enquire/2,
         document/2,
         match_set/2]).

%% Resources
-export([release_resource/2,
         release_table/2]).

%% Information
-export([mset_info/2,
         mset_info/3,
         database_info/1,
         database_info/2]).


%% More information
-export([name_to_slot/1, 
         name_to_slot/2,
         slot_to_type/1,
         slot_to_type/2,
         subdb_names/1,
         multi_docid/3]).



%% ------------------------------------------------------------------
%% Other flags
%% ------------------------------------------------------------------

%% Intermodule export (non for a client!)
-export([internal_qlc_init/4,
         internal_register_qlc_table/3,
         internal_qlc_get_next_portion/4,
         internal_qlc_lookup/3,

         internal_create_resource/2,
         internal_create_resource/3,
         internal_test_run/3
         ]).


%% with_state internals
-export([internal_name_to_slot/2,
         internal_name_to_type/2,
         internal_name_to_slot_dict/1,
         internal_slot_to_type_array/1,
         internal_subdb_names/1,
         internal_multi_docid/2]).


-import(xapian_common, [ 
    append_int8/2,
    append_uint8/2,
    append_uint16/2,
    append_uint/2,
    append_iolist/2,
    append_document_id/2,
    append_unique_document_id/2,
    read_uint/1,
    read_uint8/1,
    read_string/1]).

-import(xapian_const, [
    command_id/1,
    open_mode_id/1,
    resource_type_id/1,
    qlc_type_id/1,
    resource_type_name/1,
    test_id/1]).

%% Used in handlers
-define(SERVER, ?MODULE).

%% Used for testing, then can be moved to an another file
-define(SRV, ?MODULE).
-define(APP, xapian).


-record(state, {
    %% The record is defined inside `xapian_port' as `port_rec'.
    %% It contains matadata for controlling a linked-in port driver or a port.
    port :: x_port(),

    %% Information was retrieved from #x_prefix_name{}.
    name_to_prefix :: orddict:orddict(),

    %% A value slot is an unsigned integer in C++.
    %% They are mapped into atoms in Erlang (it is optional).
    %% Information was retrieved from #x_value_name{}.
    name_to_slot :: orddict:orddict(),

    %% It is used for float values. Usually, type is `string', it is the 
    %% same as `undefined'.
    %% An index of the array is a slot number.
    slot_to_type :: array() | undefined,

    %% Used for creating resources.
    %% It contains mapping from an atom to information, about how to create 
    %% new resource on C++ side of the application.
    name_to_resource :: orddict:orddict(),

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
%% generator.add(new userResource(ResourceType::MATCH_SPY, 
%%     std::string("value_count_match_spy"), &createValueCountMatchSpy));
%% '''
-record(resource_info, {
    %% `type' is `enquery' or `mset'. 
    %% See `xapian_const:resource_type_id/1' for all variants.
    %% Each type is stored inside special table (see ObjectRegister).
    type :: atom(),
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


-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").

-compile({parse_transform, do}).
-compile({parse_transform, seqbind}).




%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type x_server() :: xapian_type:x_server().
-type x_port() :: xapian_type:x_port().
-type x_transaction() :: xapian_type:x_transaction().
-type x_query() :: xapian_type:x_query().
-type x_resource() :: xapian_type:x_resource().
-type x_record() :: xapian_type:x_record().
-type x_meta() :: xapian_type:x_meta().
-type void() :: 'VoiD'.
-type x_document_id() :: xapian_type:x_document_id().
-type x_string() :: xapian_type:x_string().
-type x_unique_document_id() :: xapian_type:x_unique_document_id().
-type x_document_constructor() :: xapian_type:x_document_constructor().

-type multi_db_path() :: [#x_database{}|#x_prog_database{}|#x_tcp_database{}].
-type db_path() :: x_string() | multi_db_path().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Open the database with params.
%% `Path' is a directory name of the database.
%% For opening multiple databases you can pass a list of:
%%
%% * `#x_database{}';
%% * `#x_prog_database{}';
%% * `#x_tcp_database{}'.
%%
%% See the description of these records for more information.
%%
%% `Params' is a list of:
%%
%% * Modes: read, write, overwrite, create, open:
%%      The `read' mode is only for reading. 
%%      The `write' mode is for reading and for writing.
%%      Write mode can be combined with:
%%          `open' (default), `create', `overwrite'.
%% * Names for values and for prefixes:
%%      `#x_value_name{slot = 1, name = slotname}'
%%      `#x_prefix_name{name = author, prefix = <<$A>>}';
%% * The default stemmer. It will be used in `TermGenerator' and in the 
%%      default query parser:
%%      `#x_stemmer{language="english"}';
%% * An interface to work: `port' (or `driver' by default).
%% * `{name, Atom}' allows to register the server under the local name `Atom';
%% * `{name, {local, Atom}}' does the same;
%% * `{name, {global, Atom}}' registers the process under the global name.
-spec open(db_path(), [term()]) -> {ok, x_server()}.

open(Path, Params) ->
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
%%      Database will be automaticly close, if a supervised server 
%%      process will dead.
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
read_document(Server, DocId, RecordMetaDefinition) ->
    call(Server, {read_document_by_id, DocId, RecordMetaDefinition}).


%% @doc Read document info, without putting it into database.
-spec document_info(x_server(), 
                    x_document_constructor(), x_meta()) -> x_record().

document_info(Server, DocumentConstructor, RecordMetaDefinition) ->
    call(Server, {document_info, DocumentConstructor, RecordMetaDefinition}).


%% @doc Return a list of records.
-spec query_page(x_server(), non_neg_integer(), non_neg_integer(), 
        x_query(), x_meta()) -> [x_record()].
query_page(Server, Offset, PageSize, Query, RecordMetaDefinition) ->
    call(Server, {query_page, Offset, PageSize, Query, RecordMetaDefinition}).



%% -------------------------------------------------------------------
%% Resource manipulation
%% -------------------------------------------------------------------

%% @doc Return an enquire.
-spec enquire(x_server(), x_query()) -> x_resource().
enquire(Server, Query) ->
    call(Server, {enquire, Query}).


%% @doc Return a document as a resource.
-spec document(x_server(), x_unique_document_id() 
               | x_document_constructor()) -> x_resource().
document(Server, DocumentConstructor) when is_list(DocumentConstructor) ->
    call(Server, {document_info_resource, DocumentConstructor});

document(Server, DocId) ->
    call(Server, {document, DocId}).


%% @doc Return a match set (M-Set).
%% A match set can be created from:
%% * an enquire (`x_resource()' type);
%% * from record `#x_match_set{}', which contains an enquire and 
%%   addition parameters.
%%
%% Match set record is:
%%
%%  ```
%%  #x_match_set{
%%      enquire = EnquireResource, 
%%      from = From, 
%%      max_items = MaxItems, 
%%      check_at_least = CheckAtLeast, 
%%      spies = Spies
%%  }
%%  '''
%%
%%  where 
%%      * `EnquireResource' contains the result of the search. 
%%              @see enquire/2. It is required;
%%      * `From' means how many elements to skip. It is 0 by default;
%%      * `MaxItems' means how many elements to return. 
%%              Not more than `MaxItems' elements will be return. 
%%              It is `undefined' by default, 
%%              that means all items will be selected;
%%      * `Spies' is a list of MatchSpy resources (@see xapian_match_spy).
%%
%% @see xapian_mset_qlc
%% @see mset_info/3
-spec match_set(x_server(), #x_match_set{} | x_resource()) -> x_resource().
match_set(Server, #x_match_set{} = Rec) ->
    call(Server, Rec);

match_set(Server, EnquireResource) ->
    Rec = #x_match_set{enquire = EnquireResource},
    match_set(Server, Rec).


%% @doc Release a resource.
-spec release_resource(x_server(), x_resource()) -> void().
release_resource(Server, ResourceRef) ->
    call(Server, {release_resource, ResourceRef}).


%% @doc Clean resources allocated by the QLC table.
release_table(Server, Table) ->
    TableHash = erlang:phash2(Table),
    gen_server:cast(Server, {qlc_release_table, TableHash}).


internal_register_qlc_table(Server, Table, ResRef) ->
    TableHash = erlang:phash2(Table),
    gen_server:cast(Server, {qlc_register_table, TableHash, ResRef}).


%% ------------------------------------------------------------------
%% API Function Definitions for writable DB
%% ------------------------------------------------------------------

-spec add_document(x_server(), x_document_constructor()) -> 
    x_document_id().

add_document(Server, Document) ->
    call(Server, {add_document, Document}).


-spec replace_document(x_server(), x_unique_document_id(), 
    x_document_constructor()) -> x_document_id().

replace_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {replace_document, DocIdOrUniqueTerm, NewDocument}).


%% @doc Extend (edit) the document with data.
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


-spec delete_document(x_server(), x_unique_document_id()) -> ok.

delete_document(Server, DocIdOrUniqueTerm) ->
    call(Server, {delete_document, DocIdOrUniqueTerm}).


-spec is_document_exist(x_server(), x_unique_document_id()) -> x_document_id().

is_document_exist(Server, DocIdOrUniqueTerm) ->
    call(Server, {is_document_exist, DocIdOrUniqueTerm}).



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
%% * `committed' - A transaction was pass on this server. Data is consistent;
%% * `aborted' - A transaction was canceled on this server. Data is consistent;
%% * `failed' - An exeption was occured. Data is inconsistent.
%% 
%% If one of the servers crashed during transaction, the transaction process 
%% will be killed using `cancel_transaction' with reason `crashed_server'.
-spec transaction([x_server()], x_transaction(), timeout()) -> 
    #x_transaction_result{}.

transaction(Servers, F, Timeout) ->
    xapian_transaction:transaction(Servers, F, Timeout).



%% ------------------------------------------------------------------
%% Information about database objects
%% ------------------------------------------------------------------

%% @doc Returns the list of all properties.
%% @equiv mset_info(Server, MSetResource, xapian_mset_info:properties())
mset_info(Server, MSetResource) ->
    Params = xapian_mset_info:properties(),
    call(Server, {mset_info, MSetResource, Params}).


%% @doc Returns the list of selected properties and wanted values.
%% Properties:
%% * `matches_lower_bound';
%% * `matches_estimated';
%% * `matches_upper_bound';
%% * `uncollapsed_matches_lower_bound';
%% * `uncollapsed_matches_estimated';
%% * `uncollapsed_matches_upper_bound';
%% * `size'; 
%% * `max_possible'; 
%% * `max_attained';
%% * `{term_weight, Term}';
%% * `{term_freq, Term}'.
mset_info(Server, MSetResource, Params) ->
    call(Server, {mset_info, MSetResource, Params}).


%% @doc Returns the list of all properties.
database_info(Server) ->
    Params = xapian_db_info:properties(),
    call(Server, {database_info, Params}).


%% @doc Returns the list of selected properties and wanted values.
%% Properties:
%% * `has_positions'; 
%% * `document_count'; 
%% * `last_document_id'
%%      {@link last_document_id/1}; 
%% * `average_length';
%% * `document_length_lower_bound';
%% * `document_length_upper_bound'; 
%% * `uuid'
%%      Get a UUID for the database;
%% * `{term_exists, Term}';
%% * `{term_freq, Term}';
%% * `{collection_freq, WTF}';
%% * `{value_freq, Value}';
%% * `{value_lower_bound, Value}';
%% * `{value_upper_bound, Value}';
%% * `{wdf_upper_bound, Term}'; 
%% * `{document_length, DocId}';
%% * `{metadata, Key}'
%%      Get the user-specified metadata associated with a given key.
%%
%% If `DocId' or `Term' does not exist, `undefined' value will be return.
%%
%% For example,
%%
%% ```
%% database_info(Server, [{term_exists, "erlang"}, {term_freq, "erlang"}]).
%% [{{term_exists, "erlang"}, false}, {{term_freq, "erlang"}, undefined}]
%% '''
database_info(Server, Params) ->
    call(Server, {database_info, Params}).


%% ------------------------------------------------------------------
%% Information about the state of the process
%% ------------------------------------------------------------------

name_to_slot(_ServerOrState, Slot) when is_integer(Slot) ->
    Slot;

name_to_slot(#state{name_to_slot = N2S}, Slot) when is_atom(Slot) ->
    orddict:fetch(Slot, N2S);

name_to_slot(Server, Slot) when is_atom(Slot) ->
    call(Server, {with_state, fun ?SRV:internal_name_to_slot/2, Slot}).


name_to_slot(#state{name_to_slot = N2S}) ->
    N2S;

name_to_slot(Server) ->
    call(Server, {with_state, fun ?SRV:internal_name_to_slot_dict/1}).


slot_to_type(#state{slot_to_type = V2T}) ->
    V2T;

slot_to_type(Server) ->
    call(Server, {with_state, fun ?SRV:internal_slot_to_type_array/1}).


slot_to_type(State=#state{}, Slot) ->
    {ok, Type} = internal_name_to_type(State, Slot),
    Type;

slot_to_type(Server, Slot) ->
    call(Server, {with_state, fun ?SRV:internal_name_to_type/2, Slot}).


subdb_names(#state{subdb_names = I2N}) ->
    I2N;

subdb_names(Server) ->
    call(Server, {with_state, fun ?SRV:internal_subdb_names/1}).


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

internal_create_resource(Server, ResourceTypeName, ParamCreatorFun) ->
    call(Server, {create_resource, ResourceTypeName, ParamCreatorFun}).


%% @private
-spec internal_create_resource(x_server(), atom()) -> x_resource().

internal_create_resource(Server, ResourceTypeName) ->
    call(Server, {create_resource, ResourceTypeName, undefined}).


%% ------------------------------------------------------------------
%% gen_server Client Helpers
%% ------------------------------------------------------------------

call(Server, Params) ->
    client_error_handler(gen_server:call(Server, Params)).


client_error_handler({ok, Result}) -> 
    Result;

client_error_handler({error, Reason}) -> 
    erlang:error(Reason).


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
            true                -> array:from_orddict(Slot2Type) 
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
        ResourceInfo <-
            port_get_resource_info(Port),
        begin

        %% ATTENTION: this #resource{} is malformed!
        %% type is a resource type,
        %% number is a object id in __ObjectRegister<UserResource>__,
        %% not a number in the register of type `type'.
        Name2Resource = 
        [{InfoName, #resource{type=InfoType, number=InfoNum}} 
            || #resource_info{type=InfoType, number=InfoNum, name=InfoName} 
                <- ResourceInfo],
        Name2ResourceDict = orddict:from_list(Name2Resource),
        Name2Subdb = subdb_numbers(SubDbNames),
        Name2SubdbDict = orddict:from_list(Name2Subdb),
        {ok, #state{
            port = Port,
            name_to_prefix = Name2PrefixDict,
            name_to_slot = Name2SlotDict,
            slot_to_type = Slot2TypeArray,
            name_to_resource = Name2ResourceDict,
            subdb_name_to_id = Name2SubdbDict,
            subdb_names = list_to_tuple(SubDbNames)
        }}
        end]));

%% Add a copy of the server for the transaction
init([{from_state, State}]) ->
    {ok, State}.

 
%% @private
handle_call({with_state, Fun}, _From, State) ->
    {reply, Fun(State), State};

handle_call({with_state, Fun, Params}, _From, State) ->
    {reply, Fun(State, Params), State};

handle_call(last_document_id, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_last_document_id(Port),
    {reply, Reply, State};

handle_call(close, _From, State=#state{master=undefined}) ->
    #state{ port = Port } = State,
    %% Flush on disk, close DB.
    close_db_port(Port),
    %% Destroy connection (send EOF to a prog port).
    xapian_port:close(Port),
    Reply = ok,
    Reason = normal,
    {stop, Reason, Reply, State};

handle_call(close, _From, State) ->
    {stop, normal, ok, State};

handle_call({add_document, Document}, _From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, State),
    Reply = port_add_document(Port, EncodedDocument),
    {reply, Reply, State};

handle_call({replace_document, Id, Document}, _From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, State),
    Reply = port_replace_document(Port, Id, EncodedDocument),
    {reply, Reply, State};

handle_call({update_document, Id, Document, Create}, _From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, State),
    Reply = port_update_document(Port, Id, EncodedDocument, Create),
    {reply, Reply, State};

handle_call({is_document_exist, Id}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_is_document_exist(Port, Id),
    {reply, Reply, State};

handle_call({delete_document, Id}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_delete_document(Port, Id),
    {reply, Reply, State};

handle_call({set_metadata, Key, Value}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_set_metadata(Port, Key, Value),
    {reply, Reply, State};

handle_call({test, TestName, Params}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_test(Port, TestName, Params),
    {reply, Reply, State};

handle_call({document_info_resource, Document}, {FromPid, _FromRef}, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, State),
    Result = port_document_info_resource(Port, EncodedDocument),
    m_do_register_resource(State, document, FromPid, Result);

handle_call({document_info, Document, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
          subdb_names = Id2Name, slot_to_type = Slot2Type } = State,
    EncodedDocument = document_encode(Document, State),
    Reply = port_document_info(Port, EncodedDocument, 
                               Meta, Name2Slot, Id2Name, Slot2Type),
    {reply, Reply, State};

handle_call({read_document_by_id, Id, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
          subdb_names = Id2Name, slot_to_type = Slot2Type } = State,
    Reply = port_read_document_by_id(Port, Id, 
                                     Meta, Name2Slot, Id2Name, Slot2Type),
    {reply, Reply, State};

handle_call({query_page, Offset, PageSize, Query, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
        subdb_names = Id2Name, slot_to_type = Slot2Type } = State,
    Reply = port_query_page(Port, Offset, PageSize, Query, 
                            Meta, Name2Slot, Id2Name, Slot2Type),
    {reply, Reply, State};

handle_call({enquire, Query}, {FromPid, _FromRef}, State) ->
    #state{ 
        port = Port, 
        name_to_slot = Name2Slot, 
        slot_to_type = Slot2TypeArray,
        register = Register } = State,

    %% Special handling of errors
    case port_enquire(Port, Query, Name2Slot, Slot2TypeArray, Register) of
        {error, _Reason} = Error ->
            {reply, Error, State};
        {ok, ResourceNum} ->
            Elem = #resource{type=enquire, number=ResourceNum},
            %% Reply is a reference
            {ok, {NewRegister, Ref}} = 
            xapian_register:put(Register, FromPid, Elem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, Ref}, NewState}
    end;

handle_call({document, DocId}, {FromPid, _FromRef}, State) ->
    #state{ port = Port } = State,

    m_do_register_resource(State, document, FromPid, 
                           port_document(Port, DocId));


handle_call(#x_match_set{} = Mess, {FromPid, _FromRef}, State) ->
    #x_match_set{
        enquire = EnquireRef, 
        from = From, 
        max_items = MaxItems, 
        check_at_least = CheckAtLeast, 
        spies = SpyRefs
    } = Mess, 
    #state{port = Port, register = Register } = State,
    do_reply(State, do([error_m ||
        SpyNums  
            <- check_all([ref_to_num(Register, SpyRef, match_spy) 
                    || SpyRef <- SpyRefs]),
        EnquireNum 
            <- ref_to_num(Register, EnquireRef, enquire),

        MSetNum <-
            port_match_set(Port, EnquireNum, From, 
                MaxItems, CheckAtLeast, SpyNums),

        register_resource(State, mset, FromPid, MSetNum)]));

handle_call({release_resource, Ref}, _From, State) ->
    #state{port = Port, register = Register } = State,
    do_reply(State, do([error_m ||
    {NewRegister, Elem} <- 
        xapian_register:erase(Register, Ref),
    <<>> <- 
        begin
            #resource{type=ResourceType, number=ResourceNum} = Elem,
            port_release_resource(Port, ResourceType, ResourceNum)
        end,
        begin
            NewState = State#state{register = NewRegister},
            {reply, {ok, ok}, NewState}
        end
    ]));


%% Convert Res into QlcRes.
%% ResRef is an iterable object.
%% QlcRef, QlcResNum is for access for a QLC table.
handle_call({qlc_init, QlcType, ResRef, EncFun}, {FromPid, _FromRef}, State) ->
    #state{register = Register } = State,
    do_reply(State, do([error_m ||
        %% Get an iterable resource by the reference
        #resource{type=ResType, number=ResNum} 
            <- xapian_register:get(Register, ResRef),

        %% Create QLC table (iterator-like object in Erlang)
        #internal_qlc_info{resource_number = QlcResNum} = Reply
            <- port_qlc_init(State, QlcType, ResType, ResNum, EncFun),

        begin
            QlcElem = #resource{type=qlc, number=QlcResNum},
            {ok, {NewRegister, QlcRef}} = 
            xapian_register:put(Register, FromPid, QlcElem),
            NewState = State#state{register = NewRegister},

            %% Add a reference
            Reply2 = Reply#internal_qlc_info{resource_ref = QlcRef},
            {reply, {ok, Reply2}, NewState}
        end]));
    

handle_call({qlc_lookup, EncoderFun, QlcResNum}, _From, State) ->
    Reply = port_qlc_lookup(State, QlcResNum, EncoderFun),
    {reply, Reply, State};
    

handle_call({qlc_next_portion, QlcResNum, From, Count}, _From, State) ->
    #state{port = Port } = State,
    Reply = port_qlc_next_portion(Port, QlcResNum, From, Count),
    {reply, Reply, State};



%% Create new resource object of type `ResouceType'
%% with a number `ResourceObjectNumber',
%% using creator with a number `UserResourceNumber'.
%% Return an Erlang reference of new object.
handle_call({create_resource, ResourceTypeName, ParamCreatorFun}, 
    {FromPid, _FromRef}, State) ->
    #state{ name_to_resource = N2R, port = Port } = State,
    do_reply(State, do([error_m ||
        #resource{ type = ResouceType, number = UserResourceNumber }  
            <- orddict_find(ResourceTypeName, N2R),
        
        ParamBin 
            <-  if 
                    is_function(ParamCreatorFun) ->
                        {arity, Arity} = erlang:fun_info(ParamCreatorFun, arity),
                        case Arity of
                            0 -> ParamCreatorFun();
                            1 -> ParamCreatorFun(State)
                        end;
                    true ->
                        {ok, <<>>}
                end,

        ResourceObjectNumber
            <- port_create_resource(
                Port, ResouceType, UserResourceNumber, ParamBin),

        register_resource(State, ResouceType, FromPid, ResourceObjectNumber)]));


handle_call({mset_info, MSetRef, Params}, _From, State) ->
    #state{port = Port, register = Register } = State,
    Reply = 
    do([error_m ||
        MSetNum 
            <- ref_to_num(Register, MSetRef, mset),

        port_mset_info(Port, MSetNum, Params)
    ]),
    {reply, Reply, State};


handle_call({database_info, Params}, _From, State) ->
    #state{port = Port} = State,
    Reply = port_database_info(Port, Params),
    {reply, Reply, State};


handle_call({transaction, Ref}, From, State) ->
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
            FromPid ! {transaction_status, Ref, self(), Status},
            {noreply, State};

        Error ->
            {reply, Error, State}
    end.


%% @private
handle_cast(BadMess, State) ->
    %% This function is unused.
    {stop, {unexpected_mess, BadMess}, State}.


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
handle_info(#'DOWN'{ref=Ref, type=process}, State) ->
    #state{ 
        port = Port, 
        register = Register } = State,
    case xapian_register:erase(Register, Ref) of
        {ok, {NewRegister, Elem}} ->
            #resource{type=ResourceType, number=ResourceNum} = Elem,
            %% TODO: handle a return value
            port_release_resource(Port, ResourceType, ResourceNum),
            NewState = State#state{register = NewRegister},
            {noreply, NewState};

        {error, _Reason} = _Error ->
           {noreply, State}
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

document_encode(Document, #state{
        name_to_prefix = Name2Prefix,
        name_to_slot = Name2Slot,
        slot_to_type = Slot2TypeArray
    }) ->
    xapian_document:encode(Document, Name2Prefix, Name2Slot, Slot2TypeArray).


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
            {error, #x_error{type=Type, reason=Mess}}
    end.


open_database(Port, Mode, Path) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(open_mode_id(Mode), Bin@),
    Bin@ = append_iolist(Path, Bin@),
    control(Port, open, Bin@).


open_prog_database(Port, Mode, Prog, Args, Timeout) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(open_mode_id(Mode), Bin@),
    Bin@ = append_iolist(Prog, Bin@),
    Bin@ = append_iolist(Args, Bin@),
    Bin@ = append_uint(Timeout, Bin@),
    control(Port, open_prog, Bin@).


open_tcp_database(Port, Mode, TcpHost, TcpPort, Timeout, ConnectTimeout) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(open_mode_id(Mode), Bin@),
    Bin@ = append_iolist(TcpHost, Bin@),
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


port_update_document(Port, Id, EncodedDocument, Create) ->
    Action = 
    if  Create -> update_or_create_document;
        true -> update_document
        end,
    decode_docid_result(
        control(Port, Action, 
            append_unique_document_id(Id, EncodedDocument))).


port_replace_document(Port, Id, EncodedDocument) ->
    decode_docid_result(
        control(Port, replace_document, 
            append_unique_document_id(Id, EncodedDocument))).


port_delete_document(Port, Id) ->
    control(Port, delete_document, append_unique_document_id(Id, <<>>)).


port_is_document_exist(Port, Id) ->
    decode_boolean_result(
        control(Port, is_document_exist, append_unique_document_id(Id, <<>>))).


port_set_metadata(Port, Key, Value) ->
    Bin@ = <<>>,
    Bin@ = append_iolist(Key, Bin@),
    Bin@ = append_iolist(Value, Bin@),
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
                Slot2Type) ->
    Bin@ = <<>>,
    Bin@ = append_uint(Offset, Bin@),
    Bin@ = append_uint(PageSize, Bin@),
    Bin@ = xapian_query:encode(Query, Name2Slot, Slot2Type, Bin@),
    Bin@ = xapian_record:encode(Meta, Name2Slot, Slot2Type, Bin@),
    decode_records_result(control(Port, query_page, Bin@), Meta, Id2Name).


port_enquire(Port, Enquire, Name2Slot, Slot2TypeArray, Register) ->
    Bin@ = <<>>,
    Bin@ = xapian_enquire:encode(Enquire, Name2Slot, Slot2TypeArray, Register, Bin@),
    decode_resource_result(control(Port, enquire, Bin@)).


port_document(Port, DocId) ->
    Bin@ = <<>>,
    Bin@ = append_unique_document_id(DocId, Bin@),
    decode_resource_result(control(Port, document, Bin@)).


port_match_set(Port, MSetResourceNum, From, MaxItems, CheckAtLeast, SpyNums) ->
    Bin@ = <<>>,
    Bin@ = append_uint(MSetResourceNum, Bin@),
    Bin@ = append_uint(From, Bin@),
    Bin@ = append_max_items(MaxItems, Bin@),
    Bin@ = append_uint(CheckAtLeast, Bin@),
    Bin@ = append_match_spies(SpyNums, Bin@),
    decode_resource_result(control(Port, match_set, Bin@)).


append_match_spies(Spies, Bin) when is_binary(Bin), is_list(Spies) ->
    append_uint(0, lists:foldl(fun xapian_common:append_uint/2, Bin, Spies)).


append_max_items(MaxItems, Bin@) 
    when is_integer(MaxItems), MaxItems >= 0 ->
    append_uint(MaxItems, append_uint8(0, Bin@));

append_max_items(undefined, Bin@) ->
    append_uint8(1, Bin@).


port_release_resource(Port, ResourceType, ResourceNum) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(resource_type_id(ResourceType), Bin@),
    Bin@ = append_uint(ResourceNum, Bin@),
    control(Port, release_resource, Bin@).


port_qlc_next_portion(Port, QlcResNum, From, Count) ->
    Bin@ = <<>>,
    Bin@ = append_uint(QlcResNum, Bin@),
    Bin@ = append_uint(From, Bin@),
    Bin@ = append_uint(Count, Bin@),
    control(Port, qlc_next_portion, Bin@).


port_create_resource(Port, ResouceType, UserResourceNumber, ParamBin) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(resource_type_id(ResouceType), Bin@),
    Bin@ = append_uint(UserResourceNumber, Bin@),
    Bin@ = <<Bin@/binary, ParamBin/binary>>,
    decode_resource_result(control(Port, create_resource, Bin@)).
    


port_get_resource_info(Port) ->
    decode_resource_info(control(Port, get_resource_info, <<>>)).


port_qlc_init(State, QlcType, ResourceType, ResourceNum, EncoderFun) ->
    #state{ port = Port } = State,
    Bin@ = <<>>,
    Bin@ = append_uint8(qlc_type_id(QlcType), Bin@),
    Bin@ = append_uint8(resource_type_id(ResourceType), Bin@),
    Bin@ = append_uint(ResourceNum, Bin@),
    Bin@ = append_with_encoder2(State, EncoderFun, ResourceType, Bin@), 
    decode_qlc_info_result(control(Port, qlc_init, Bin@)).


port_qlc_lookup(State, QlcResNum, EncoderFun) ->
    #state{ port = Port } = State,
    Bin@ = <<>>,
    Bin@ = append_uint(QlcResNum, Bin@),
    Bin@ = append_with_encoder(State, EncoderFun, Bin@), 
    control(Port, qlc_lookup, Bin@).


append_with_encoder(State, EncoderFun, Bin) 
    when is_function(EncoderFun) ->
    {arity, Arity} = erlang:fun_info(EncoderFun, arity),
    case Arity of
        1 -> EncoderFun(Bin);
        2 -> EncoderFun(State, Bin)
    end.

append_with_encoder2(State, EncoderFun, Param, Bin) 
    when is_function(EncoderFun) ->
    {arity, Arity} = erlang:fun_info(EncoderFun, arity),
    case Arity of
        2 -> EncoderFun(Param, Bin);
        3 -> EncoderFun(Param, State, Bin)
    end.


port_mset_info(Port, MSetNum, Params) ->
    Bin@ = <<>>,
    Bin@ = append_uint(MSetNum, Bin@),
    Bin@ = xapian_mset_info:encode(Params, Bin@),
    decode_mset_info_result(control(Port, mset_info, Bin@), Params).


port_database_info(Port, Params) ->
    Bin@ = <<>>,
    Bin@ = xapian_db_info:encode(Params, Bin@),
    decode_database_info_result(control(Port, database_info, Bin@), Params).


%% -----------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------

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

decode_database_info_result(Data, Params) ->
    decode_result_with_hof(Data, Params, fun xapian_db_info:decode/2).

decode_docid_result(Data) -> 
    decode_result_with_hof(Data, fun xapian_common:read_document_id/1).

decode_resource_result(Data) -> 
    decode_result_with_hof(Data, fun xapian_common:read_uint/1).

decode_boolean_result(Data) -> 
    decode_result_with_hof(Data, fun xapian_common:read_boolean/1).



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



%% @doc Read a list of `#resource_info{}' from binary, while it is not empty.
decode_resource_info({ok, Bin}) ->
    {ok, decode_resource_info_cycle(Bin, [])};

decode_resource_info(Other) -> 
    Other.


-spec decode_resource_info_cycle(binary(), [#resource_info{}]) -> 
    [#resource_info{}].

decode_resource_info_cycle(<<>>, Acc) ->
    lists:reverse(Acc);

decode_resource_info_cycle(Bin@, Acc) ->
    {TypeId, Bin@} = read_uint8(Bin@),
    {Num, Bin@} = read_uint(Bin@),
    {Name, Bin@} = read_string(Bin@),
    Rec = #resource_info{
        number = Num,
        type = resource_type_name(TypeId),
        name = list_to_atom(binary_to_list(Name))
    },
    decode_resource_info_cycle(Bin@, [Rec|Acc]).


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
%%
register_resource(State, Type, FromPid, ResourceNum) ->
    #state{ register = Register } = State,
    Elem = #resource{type=Type, number=ResourceNum},
    %% Reply is a reference
    {ok, {NewRegister, Ref}} = 
    xapian_register:put(Register, FromPid, Elem),
    NewState = State#state{register = NewRegister},
    {reply, {ok, Ref}, NewState}.

m_do_register_resource(State, Type, FromPid, Result) ->
    %% Special handling of errors
    case Result of
        {error, _Reason} = Error ->
            {reply, Error, State};
        {ok, ResourceNum} ->
            register_resource(State, Type, FromPid, ResourceNum)
    end.

         
ref_to_num(Register, ResRef, Type) ->
    case xapian_register:get(Register, ResRef) of
        {ok, #resource{type=Type, number=ResNum}} ->
            {ok, ResNum};
        {error, _} = Error ->
            Error;
        {ok, _Res} ->
            {error, bad_resource_type}
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




%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% This module uses eunit lazy generators.
%% We check, that the mapping from type's id into its name and back is right.
resource_type_conversation_test_() ->
    FirstId = 1,
    LastId = resource_type_id(last),
    StopId = LastId + 1,
    ?assert(FirstId < StopId),
    {"Check resource_type_id and resource_type_name.", 
        resource_type_conversation_gen(FirstId, StopId)}.
    

resource_type_conversation_gen(CurId, CurId) ->
    [];

resource_type_conversation_gen(CurId, StopId) ->
    Case = 
    fun () ->
        More = resource_type_conversation_gen(CurId + 1, StopId),
        [?_assertEqual(CurId, 
            resource_type_id(resource_type_name(CurId))) | More]
        end,
    {generator, Case}.

-endif.

