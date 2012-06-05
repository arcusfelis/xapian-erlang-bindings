% This module is a `gen_server' that handles a single port connection.
-module(xapian_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/2,
         last_document_id/1,
         read_document/3,
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
         document/2]).

%% Resources
-export([release_resource/2]).

%% Match set (M-set)
-export([match_set/2,
         match_set/3,
         match_set/4,
         match_set/5,
         match_set/6
        ]).

%% Information
-export([mset_info/3,
         database_info/2]).


%% More information
-export([name_to_slot/1, 
         name_to_slot/2,
         value_to_type/1,
         subdb_names/1,
         multi_docid/3]).



%% ------------------------------------------------------------------
%% Other flags
%% ------------------------------------------------------------------

%% Intermodule export (non for a client!)
-export([internal_qlc_init/4,
         internal_qlc_get_next_portion/4,
         internal_qlc_lookup/3,

         internal_create_resource/2,
         internal_create_resource/3,
         internal_test_run/3
         ]).


%% with_state internals
-export([internal_name_to_slot/2,
         internal_name_to_slot_dict/1,
         internal_value_to_type_array/1,
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
    port :: record(),

    %% Information was retrieved from #x_prefix_name{}
    name_to_prefix :: orddict:orddict(),

    %% Information was retrieved from #x_value_name{}
    name_to_slot :: ordict:orddict(),
    value_to_type :: array() | undefined,

    %% Used for creating resources.
    %% It contains mapping from an atom to information, about how to create 
    %% new resource on C++ side of the application.
    name_to_resource :: ordict:orddict(),

    %% Each sub-database can have a name for identification.
    subdb_name_to_id :: ordict:orddict(),
    subdb_names :: tuple(),

    %% Pid of the real server (used by a transaction).
    %% If the process will be terminated, then new owner of the port will 
    %% be master.
    master,
    register = xapian_register:new()
}).


-record(resource_info, {
    type :: atom(),
    number :: non_neg_integer(),
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

-opaque x_server() :: xapian:x_server().
-opaque x_transaction() :: xapian:x_transaction().
-opaque x_query() :: xapian:x_query().
-opaque x_resource() :: xapian:x_resource().
-opaque x_record() :: xapian:x_record().
-opaque x_meta() :: xapian:x_meta().
-opaque void() :: 'VoiD'.
-opaque x_document_id() :: xapian:x_document_id().
-opaque x_string() :: xapian:x_string().
-opaque x_unique_document_id() :: xapian:x_unique_document_id().
-opaque x_document_index_part() :: xapian:x_document_index_part().

-type multi_db_path() :: [#x_database{}|#x_prog_database{}|#x_tcp_database{}].
-type db_path() :: x_string() | multi_db_path().


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Open the database with params.
%% `Path' is a directory name of the database.
%% For opening multiple databases you can pass a list of:
%%
%% * #x_database{};
%% * #x_prog_database{};
%% * #x_tcp_database{}.
%%
%% See the description of these records for more information.
%%
%% `Params' is a list of:
%%
%% * Modes: read, write, overwrite, create, open;
%% * Names for values and for prefixes:
%%      `#x_value_name{slot = 1, name = slotname}'
%%      `#x_prefix_name{name = author, prefix = <<$A>>}';
%% * The default stemmer. It will be used in `TermGenerator' and in the 
%%      `default_query_parser':
%%      `#x_stemmer{language="english"}';
%% * An interface to work: `port' (or `driver' by default).
%%
%% The `read' mode is only for reading. 
%% The `write' mode is for reading and for writing.
%% Write mode can be combined with:
%% `open' (default), `create', `overwrite'.
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


-spec last_document_id(x_server()) -> x_document_id().
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


%% @doc Return a list of records.
-spec query_page(x_server(), non_neg_integer(), non_neg_integer(), 
        x_query(), x_meta()) -> [x_record()].
query_page(Server, Offset, PageSize, Query, RecordMetaDefinition) ->
    call(Server, {query_page, Offset, PageSize, Query, RecordMetaDefinition}).


%% @doc Return enquire.
-spec enquire(x_server(), x_query()) -> x_resource().
enquire(Server, Query) ->
    call(Server, {enquire, Query}).


%% @doc Return a document.
-spec document(x_server(), x_unique_document_id()) -> x_resource().
document(Server, DocId) ->
    call(Server, {document, DocId}).


match_set(Server, #x_match_set{} = Rec) ->
    call(Server, Rec);

match_set(Server, EnquireResource) ->
    Rec = #x_match_set{enquire = EnquireResource},
    match_set(Server, Rec).


match_set(Server, EnquireResource, From) ->
    Rec = #x_match_set{enquire = EnquireResource, from = From},
    match_set(Server, Rec).


match_set(Server, EnquireResource, From, MaxItems) ->
    Rec = #x_match_set{
        enquire = EnquireResource, 
        from = From, 
        max_items = MaxItems
    },
    match_set(Server, Rec).


match_set(Server, EnquireResource, From, MaxItems, CheckAtLeast) ->
    Rec = #x_match_set{
        enquire = EnquireResource, 
        from = From, 
        max_items = MaxItems, 
        check_at_least = CheckAtLeast
    },
    match_set(Server, Rec).


match_set(Server, EnquireResource, From, MaxItems, CheckAtLeast, Spies) 
    when is_list(Spies) ->
    Rec = #x_match_set{
        enquire = EnquireResource, 
        from = From, 
        max_items = MaxItems, 
        check_at_least = CheckAtLeast, 
        spies = Spies
    },
    match_set(Server, Rec).


mset_info(Server, MSetResource, Params) ->
    call(Server, {mset_info, MSetResource, Params}).


database_info(Server, Params) ->
    call(Server, {database_info, Params}).


%% @doc Release a resource.
-spec release_resource(x_server(), x_resource()) -> void().
release_resource(Server, ResourceRef) ->
    call(Server, {release_resource, ResourceRef}).


%% ------------------------------------------------------------------
%% API Function Definitions for writable DB
%% ------------------------------------------------------------------

-spec add_document(x_server(), [x_document_index_part()]) -> 
    x_document_id().

add_document(Server, Document) ->
    call(Server, {add_document, Document}).


-spec replace_document(x_server(), x_unique_document_id(), 
    [x_document_index_part()]) -> x_document_id().

replace_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {replace_document, DocIdOrUniqueTerm, NewDocument}).


%% Extend (edit) the document with data.
-spec update_document(x_server(), x_unique_document_id(), 
    [x_document_index_part()]) -> x_document_id().

update_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {update_document, DocIdOrUniqueTerm, NewDocument, false}).


-spec update_or_create_document(x_server(), x_unique_document_id(), 
    [x_document_index_part()]) -> x_document_id().

update_or_create_document(Server, DocIdOrUniqueTerm, NewDocument) ->
    call(Server, {update_document, DocIdOrUniqueTerm, NewDocument, true}).


-spec delete_document(x_server(), x_unique_document_id()) -> ok.

delete_document(Server, DocIdOrUniqueTerm) ->
    call(Server, {delete_document, DocIdOrUniqueTerm}).


-spec set_metadata(x_server(), x_string(), x_string()) -> ok.

set_metadata(Server, Key, Value) ->
    call(Server, {set_metadata, Key, Value}).



%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

internal_test_run(Server, TestName, Params) ->
    call(Server, {test, TestName, Params}).


%% ------------------------------------------------------------------
%% Transactions
%% ------------------------------------------------------------------


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
%% Info  
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


value_to_type(#state{value_to_type = V2T}) ->
    V2T;

value_to_type(Server) ->
    call(Server, {with_state, fun ?SRV:internal_value_to_type_array/1}).


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
%% Info Internal
%% ------------------------------------------------------------------

internal_name_to_slot_dict(#state{name_to_slot = N2S}) -> 
    {ok, N2S}.

internal_value_to_type_array(#state{value_to_type = V2T}) -> 
    {ok, V2T}.

internal_name_to_slot(#state{name_to_slot = N2S}, Slot) -> 
    orddict_find(Slot, N2S).

internal_subdb_names(#state{subdb_names = I2N}) -> 
    {ok, I2N}.

internal_multi_docid(State, {DocId, SubDbName}) when is_atom(SubDbName) ->
    #state{subdb_name_to_id = N2I} = State,
    do([error_m ||
        SubDbNum <- orddict_find(SubDbName, N2I),
        {ok, multi_docid(State, DocId, SubDbNum)}]);

internal_multi_docid(State, {DocId, SubDbNum}) ->
    {ok, multi_docid(State, DocId, SubDbNum)}.


%% ------------------------------------------------------------------
%% API for other modules
%% ------------------------------------------------------------------

%% Create a qlc resource, collect basic information about a set.
-spec internal_qlc_init(x_server(), atom(), reference(), fun()) ->
    #internal_qlc_info{}.

internal_qlc_init(Server, Type, ResourceRef, EncoderFun) ->
    call(Server, {qlc_init, Type, ResourceRef, EncoderFun}).


%% Read next `Count' elements starting from `From' from QlcResNum.
-spec internal_qlc_get_next_portion(x_server(), 
    non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    binary().

internal_qlc_get_next_portion(Server, QlcResNum, From, Count) ->
    call(Server, {qlc_next_portion, QlcResNum, From, Count}).


-spec internal_qlc_lookup(x_server(), fun(),
    non_neg_integer()) -> binary().

internal_qlc_lookup(Server, EncoderFun, ResNum) ->
    call(Server, {qlc_lookup, EncoderFun, ResNum}).


%% ParamCreatorFun will be called as ParamCreatorFun(Register).
%% ParamCreatorFun returns `{ok, Bin}', where `Bin' is encoded binary, 
%% this binary will be passed as `ParamEncoder' into a resource 
%% creator function on C++ side.
-spec internal_create_resource(x_server(), atom(), fun()) -> x_resource().

internal_create_resource(Server, ResourceTypeName, ParamCreatorFun) ->
    call(Server, {create_resource, ResourceTypeName, ParamCreatorFun}).


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

    Value2Type =
    [{Slot, Type} 
        || #x_value_name{type = Type, slot = Slot} <- Params, Type =/= string],

    Value2TypeArray = 
        if 
            Value2Type =:= []   -> undefined; 
            true                -> array:from_orddict(Value2Type) 
        end,

    Name2PrefixDict = orddict:from_list(Name2Prefix),
    Name2SlotDict   = orddict:from_list(Name2Slot),

    %% This stemmer will be used by default
    DefaultStemmer = lists:keyfind(m_stemmer, 1, Params),

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
            value_to_type = Value2TypeArray,
            name_to_resource = Name2ResourceDict,
            subdb_name_to_id = Name2SubdbDict,
            subdb_names = list_to_tuple(SubDbNames)
        }}
        end]));

%% Add a copy of the server for the transaction
init([{from_state, State}]) ->
    {ok, State}.

 
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

handle_call({read_document_by_id, Id, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
          subdb_names = Id2Name, value_to_type = Value2Type } = State,
    Reply = port_read_document_by_id(Port, Id, 
                                     Meta, Name2Slot, Id2Name, Value2Type),
    {reply, Reply, State};

handle_call({query_page, Offset, PageSize, Query, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot,
          subdb_names = Id2Name, value_to_type = Value2Type } = State,
    Reply = port_query_page(Port, Offset, PageSize, Query, 
                            Meta, Name2Slot, Id2Name, Value2Type),
    {reply, Reply, State};

handle_call({enquire, Query}, {FromPid, _FromRef}, State) ->
    #state{ 
        port = Port, 
        name_to_slot = Name2Slot, 
        register = Register } = State,

    %% Special handling of errors
    case port_enquire(Port, Query, Name2Slot, Register) of
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
    #state{ 
        port = Port, 
        register = Register } = State,

    %% Special handling of errors
    case port_document(Port, DocId) of
        {error, _Reason} = Error ->
            {reply, Error, State};
        {ok, ResourceNum} ->
            Elem = #resource{type=document, number=ResourceNum},
            %% Reply is a reference
            {ok, {NewRegister, Ref}} = 
            xapian_register:put(Register, FromPid, Elem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, Ref}, NewState}
    end;


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

        begin
            MSetElem = #resource{type=mset, number=MSetNum},
            %% Reply is a reference
            {ok, {NewRegister, MSetRef}} = 
            xapian_register:put(Register, FromPid, MSetElem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, MSetRef}, NewState}
        end]));

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
    #state{ name_to_resource = N2R, port = Port, register = Register } = State,
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

        begin
            Elem = #resource{type=ResouceType, number=ResourceObjectNumber},
            %% Reply is a reference
            {ok, {NewRegister, Ref}} = 
            xapian_register:put(Register, FromPid, Elem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, Ref}, NewState}
        end]));


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


handle_cast(_, State) ->
    {noreply, State}.


do_reply(OldState, {error, _Reason} = Error) ->
    {reply, Error, OldState};

do_reply(_State, Other) ->
    Other.


stop_if_error({error, _Reason} = Error) ->
    io:format(user, "~p", [Error]),
    {stop, Error};

stop_if_error(Other) ->
    Other.


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



terminate(_Reason, #state{master = undefined, port = Port}) ->
    close_db_port(Port),
    ok;

terminate(_Reason, #state{master = Master, port = Port}) ->
    %% Change the owner of the port
    xapian_port:connect(Port, Master),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

document_encode(Document, #state{
        name_to_prefix = Name2Prefix,
        name_to_slot = Name2Slot,
        value_to_type = Value2TypeArray
    }) ->
    xapian_document:encode(Document, Name2Prefix, Name2Slot, Value2TypeArray).


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
    Data = xapian_encode:append_prefix(DefaultStemmer, <<>>),
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



%% helper for port_*_transaction.
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
                         Value2Type) ->
    Bin@ = <<>>,
    Bin@ = append_document_id(Id, Bin@),
    Bin@ = xapian_record:encode(Meta, Name2Slot, Value2Type, Bin@),
    decode_record_result(control(Port, read_document_by_id, Bin@), Meta, Id2Name).


port_query_page(Port, Offset, PageSize, Query, Meta, Name2Slot, Id2Name, 
                Value2Type) ->
    Bin@ = <<>>,
    Bin@ = append_uint(Offset, Bin@),
    Bin@ = append_uint(PageSize, Bin@),
    Bin@ = xapian_query:encode(Query, Name2Slot, Bin@),
    Bin@ = xapian_record:encode(Meta, Name2Slot, Value2Type, Bin@),
    decode_records_result(control(Port, query_page, Bin@), Meta, Id2Name).


port_enquire(Port, Enquire, Name2Slot, Register) ->
    Bin@ = <<>>,
    Bin@ = xapian_enquire:encode(Enquire, Name2Slot, Register, Bin@),
    decode_resource_result(control(Port, enquire, Bin@)).


port_document(Port, DocId) ->
    Bin@ = <<>>,
    Bin@ = append_unique_document_id(DocId, Bin@),
    decode_resource_result(control(Port, document, Bin@)).


port_match_set(Port, MSetResourceNum, From, MaxItems, CheckAtLeast, SpyNums) ->
    Bin@ = <<>>,
    Bin@ = append_uint(MSetResourceNum, Bin@),
    Bin@ = append_uint(fix_uint(From), Bin@),
    Bin@ = append_max_items(MaxItems, Bin@),
    Bin@ = append_uint(fix_uint(CheckAtLeast), Bin@),
    Bin@ = append_match_spies(SpyNums, Bin@),
    decode_resource_result(control(Port, match_set, Bin@)).


fix_uint(X) when is_integer(X), X >= 0 -> X;
fix_uint(undefined) -> 0.


append_match_spies(Spies, Bin) when is_binary(Bin) ->
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



decode_qlc_info_result({ok, Bin@}) -> 
    {ResNum, Bin@} = read_uint(Bin@),
    {Size,   <<>>} = read_uint(Bin@),
    {ok, #internal_qlc_info{
            num_of_objects = Size,
            resource_number = ResNum %% Num of a QLC table
        }};

decode_qlc_info_result(Other) -> 
    Other.



decode_resource_info({ok, Bin}) ->
    {ok, decode_resource_info_cycle(Bin, [])};

decode_resource_info(Other) -> 
    Other.


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
         
ref_to_num(Register, ResRef, Type) ->
    case xapian_register:get(Register, ResRef) of
        {ok, #resource{type=Type, number=ResNum}} ->
            {ok, ResNum};
        {error, _} = Error ->
            Error;
        {ok, _Res} ->
            {error, bad_resource_type}
    end.


%% Helper for monades and list comprehensions
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

