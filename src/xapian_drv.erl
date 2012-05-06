%% This module is a `gen_server' that handles a single port connection.
-module(xapian_drv).
-behaviour(gen_server).

%% Used in handlers
-define(SERVER, ?MODULE).

%% Used for testing, then can be moved to an another file
-define(DRV, ?MODULE).

-define(DRIVER_NAME, "xapian_drv").

-record(state, {
    port :: port(),

    %% Information was retrieved from #x_prefix_name{}
    name_to_prefix :: orddict:orddict(),

    %% Information was retrieved from #x_value_name{}
    name_to_slot :: ordict:orddict(),

    %% Used for creating resources.
    %% It contains mapping from an atom to information, about how to create 
    %% new resource on C++ side of the application.
    name_to_resource :: ordict:orddict(),


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
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/2,
         last_document_id/1,
         read_document/3,
         close/1]).

%% For writable DB
-export([add_document/2,
         transaction/3,
         transaction/2]).

%% Queries
-export([query_page/5]). 
-export([enquire/2]).

%% Resources
-export([release_resource/2]).

%% Match set (M-set)
-export([match_set/2,
         match_set/3,
         match_set/4
        ]).

%% Intermodule export (non for a client!)
-export([internal_qlc_init/3,
         internal_qlc_get_next_portion/4,
         internal_qlc_lookup/3,

         internal_create_resource/2,
         internal_create_resource/3,
         internal_run_test/3]).


-import(xapian_common, [ 
    append_int8/2,
    append_uint/2,
    append_uint8/2,
    append_document_id/2,
    read_document_id/1,
    read_uint/1,
    read_uint8/1,
    read_string/1]).

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



%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Open the database with params.
%% Path is a directory name of the database.
%% Params is a list of:
%%
%% * Modes: read, write, overwrite, create, open
%% * Names for values and for prefixes:
%%      `#x_value_name{slot = 1, name = slotname}'
%%      `#x_prefix_name{name = author, prefix = <<$A>>}'
%% * The default stemmer. It will be used in `TermGenerator' and in the 
%%      `default_query_parser':
%%      `#x_stemmer{language="english"}'
%%
%% The `read' mode is only for reading. 
%% The `write' mode is for reading and for writing.
%% Write mode can be combined with:
%% `open' (default), `create', `overwrite'.
-spec open(term(), [term()]) -> {ok, x_server()}.

open(Path, Params) ->
    load_driver(),
    Args = [Path, Params],
    case proplists:get_value(name, Params) of
        undefined ->
            gen_server:start_link(?MODULE, Args, []);
        A when is_atom(A) ->
            gen_server:start_link({local, A}, ?MODULE, Args, []);
        Name ->
            gen_server:start_link(Name, ?MODULE, Args, [])
    end.


last_document_id(Server) ->
    call(Server, last_document_id).


%% @doc Close the database and kill a control process (aka Server).
%%      Database will be automaticly close, if the server process is dead.
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


match_set(Server, EnquireResource) ->
    call(Server, {match_set, EnquireResource, undefined, undefined}).

match_set(Server, EnquireResource, From) ->
    call(Server, {match_set, EnquireResource, From, undefined}).

match_set(Server, EnquireResource, From, MaxItems) ->
    call(Server, {match_set, EnquireResource, From, MaxItems}).


%% @doc Release resources.
-spec release_resource(x_server(), x_resource()) -> void().
release_resource(Server, ResourceRef) ->
    call(Server, {release_resource, ResourceRef}).


%% ------------------------------------------------------------------
%% API Function Definitions for writable DB
%% ------------------------------------------------------------------

add_document(Server, Document) ->
    call(Server, {add_document, Document}).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

internal_run_test(Server, TestName, Params) ->
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
    Ref = make_ref(),
    TransServers = 
        [ gen_server:call(Server, {transaction, Ref}) 
            || Server <- Servers ],

    ServersReady = lists:all(fun is_pid/1, TransServers),
    case ServersReady of
    %% Cannot init the transaction on one or more servers.
    false ->
        cannot_start_transaction(Ref, Servers, TransServers);

    %% All servers are ready.
    true ->
        transaction_ready(Ref, Servers, TransServers, F, Timeout)
    end.


%% ------------------------------------------------------------------
%% transaction Client Helpers
%% ------------------------------------------------------------------

transaction_ready(Ref, Servers, TransServers, F, Timeout) ->
    Home = self(),
    TransBody = fun() ->
            Result = F(TransServers),
            %% Send the result back.
            Home ! {result, Ref, Result}
        end,
    TransPidRef = erlang:spawn_monitor(TransBody),
    GroupMonPid = monitor_group(Home, Servers, Ref),
    Result = wait_transaction_result(TransPidRef, Ref, Servers, TransServers),
    stop_monitor(GroupMonPid),
    Result.


%% @doc Collects `#x_transaction_result{}' record.
wait_transaction_result({TransPid, TransRef}, Ref, Servers, TransServers) ->
    receive
        %% Error in the transaction body.
        {'DOWN', TransRef, process, TransPid, Reason} ->
            [ cancel_transaction(Server, Ref) || Server <- Servers ],
            Statuses = collect_status_info(Ref, Servers, []),
            #x_transaction_result{
                is_committed = false,
                is_consistent = lists:all(fun is_aborted/1, Statuses),
                statuses = Statuses,
                reason = Reason
            };

        %% One of the real servers dead.
        %% Fallback others.
        {'DOWN', Ref, process, Pid, Reason} ->
            true = lists:member(Pid, Servers),
            Servers2 = Servers -- [Pid],
            [ cancel_transaction(Server, Ref) || Server <- Servers2 ],
            Statuses = collect_status_info(Ref, Servers2, [{Pid, unknown}]),
            #x_transaction_result{
                is_committed = false,
                is_consistent = false, % unknown
                statuses = Statuses,
                reason = Reason
            };
            

        %% Result is ready. Try commit all changes.
        {result, Ref, ResultI} -> 
            [ catch close(Server) || Server <- TransServers ],
            Statuses = collect_status_info(Ref, Servers, []),
            Committed = lists:all(fun is_committed/1, Statuses),
            #x_transaction_result{
                is_committed = Committed,
                is_consistent = Committed 
                    orelse lists:all(fun is_aborted/1, Statuses),
                result = ResultI,
                statuses = Statuses
            }
    end.


%% @doc In most cases one of the servers is open only for reading, but other errors 
%%      can be also happen.
cannot_start_transaction(Ref, Servers, TransServers) ->
    Zipped = lists:zip(Servers, TransServers),
    Splitter = fun
        ({Server, TransServer}) -> is_pid(TransServer)
        end,
    {Valid, Invalid} = lists:splitwith(Splitter, Zipped),

    %% The Invalid list has the same format as the Statuses list.
    %% Cancel the transaction on Valid servers.
    ValidServers = 
        [ begin 
            cancel_transaction(Server, Ref), 
            Server %% Return the Pid of the real server
            end || {Server, _TransServer} <- Valid ],
    Statuses = collect_status_info(Ref, ValidServers, Invalid),
    #x_transaction_result{
        is_committed = false,
        is_consistent = true, %% Zero changes on the valid servers
        statuses = Statuses,
        reason = 'readonly_db'
    }.


is_committed({_Pid, committed}) -> true;
is_committed(_) -> false.


is_aborted({_Pid, aborted}) -> true;
is_aborted(_) -> false.


collect_status_info(_Ref, [], StatusList) -> 
    StatusList;

collect_status_info(Ref, Servers, StatusList) ->
    receive
        {transaction_status, Ref, Pid, Status} -> 
            NewServers = Servers -- [Pid],
            NewStatusList = [{Pid, Status} | StatusList],
            collect_status_info(Ref, NewServers, NewStatusList);
            
        {'DOWN', Ref, process, Pid, Reason} ->
            case lists:member(Pid, Servers) of
            true ->
                NewServers = Servers -- [Pid],
                NewStatusList = [{Pid, unknown} | StatusList],
                collect_status_info(Ref, NewServers, NewStatusList)
            end
    end.

    
cancel_transaction(Server, Ref) ->
    Server ! {cancel_transaction, Ref}.


%% ------------------------------------------------------------------
%% API for other modules
%% ------------------------------------------------------------------

-type qlc_params() :: #internal_qlc_mset_parameters{}.

%% Create a qlc resource, collect basic information about a set.
-spec internal_qlc_init(x_server(), reference(), qlc_params()) ->
    #internal_qlc_info{}.

internal_qlc_init(Server, ResourceRef, Params) ->
    call(Server, {qlc_init, ResourceRef, Params}).


%% Read next `Count' elements starting from `From' from QlcResNum.
-spec internal_qlc_get_next_portion(x_server(), 
    non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    binary().

internal_qlc_get_next_portion(Server, QlcResNum, From, Count) ->
    call(Server, {qlc_next_portion, QlcResNum, From, Count}).


-spec internal_qlc_lookup(x_server(), 
    non_neg_integer(), [x_document_id()]) ->
    binary().

internal_qlc_lookup(Server, ResNum, DocIds) ->
    call(Server, {qlc_lookup, ResNum, DocIds}).


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

    Name2PrefixDict = orddict:from_list(Name2Prefix),
    Name2SlotDict   = orddict:from_list(Name2Slot),

    %% This stemmer will be used by default
    DefaultStemmer = lists:keyfind(m_stemmer, 1, Params),


    Port = erlang:open_port({spawn, ?DRIVER_NAME}, []),

    do([error_m ||
        <<>> <- 
            open_database(Port, Path, Params),
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
        {ok, #state{
            port = Port,
            name_to_prefix = Name2PrefixDict,
            name_to_slot = Name2SlotDict,
            name_to_resource = Name2ResourceDict
        }}
        end]);

%% Add a copy of the server for the transaction
init([{from_state, State}]) ->
    {ok, State}.
    

handle_call(last_document_id, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_last_document_id(Port),
    {reply, Reply, State};

handle_call(close, _From, State=#state{master=undefined}) ->
    #state{ port = Port } = State,
    erlang:port_close(Port),
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

handle_call({test, TestName, Params}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_test(Port, TestName, Params),
    {reply, Reply, State};

handle_call({read_document_by_id, Id, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot } = State,
    Reply = port_read_document_by_id(Port, Id, Meta, Name2Slot),
    {reply, Reply, State};

handle_call({query_page, Offset, PageSize, Query, Meta}, _From, State) ->
    #state{ port = Port, name_to_slot = Name2Slot } = State,
    Reply = port_query_page(Port, Offset, PageSize, Query, Meta, Name2Slot),
    {reply, Reply, State};

handle_call({enquire, Query}, {FromPid, FromRef}, State) ->
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
            {ok, NewRegister, Ref} = 
            xapian_register:put(Register, FromPid, Elem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, Ref}, NewState}
    end;

handle_call({match_set, EnquireRef, From, MaxItems}, {FromPid, FromRef}, State) ->
    #state{port = Port, register = Register } = State,
    do_reply(State, do([error_m ||
        #resource{type=enquire, number=EnquireNum} 
            <- xapian_register:get(Register, EnquireRef),

        MSetNum <-
            port_match_set(Port, EnquireNum, From, MaxItems),

        begin
            MSetElem = #resource{type=mset, number=MSetNum},
            %% Reply is a reference
            {ok, NewRegister, MSetRef} = 
            xapian_register:put(Register, FromPid, MSetElem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, MSetRef}, NewState}
        end]));

handle_call({release_resource, Ref}, _From, State) ->
    #state{port = Port, register = Register } = State,
    case xapian_register:erase(Register, Ref) of
        {ok, NewRegister, Elem} ->
            #resource{type=ResourceType, number=ResourceNum} = Elem,
            Reply = port_release_resource(Port, ResourceType, ResourceNum),
            NewState = State#state{register = NewRegister},
            {reply, Reply, NewState};

        {error, _Reason} = Error ->
            {reply, Error, State}
    end;
    
handle_call({qlc_next_portion, QlcResNum, From, Count}, _From, State) ->
    #state{port = Port } = State,
    Reply = port_qlc_next_portion(Port, QlcResNum, From, Count),
    {reply, Reply, State};
    
handle_call({qlc_lookup, QlcResNum, DocIds}, _From, State) ->
    #state{port = Port } = State,
    Reply = port_qlc_lookup(Port, QlcResNum, DocIds),
    {reply, Reply, State};

%% Res into QlcRes
handle_call({qlc_init, ResRef, Params}, {FromPid, _FromRef}, State) ->
    #state{port = Port, register = Register } = State,
    do_reply(State, do([error_m ||
        %% Get an iterable resource by the reference
        #resource{type=ResType, number=ResNum} 
            <- xapian_register:get(Register, ResRef),

        %% Create QLC table (iterator-like object in Erlang)
        #internal_qlc_info{resource_number = QlcResNum} = Reply
            <- port_qlc_init(State, ResType, ResNum, Params),

        begin
            QlcElem = #resource{type=qlc, number=QlcResNum},
            %% Reply is a reference
            {ok, NewRegister, _Ref} = 
            xapian_register:put(Register, FromPid, QlcElem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, Reply}, NewState}
        end]));


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
                        ParamCreatorFun(Register);
                    true ->
                        {ok, <<>>}
                end,

        ResourceObjectNumber
            <- port_create_resource(
                Port, ResouceType, UserResourceNumber, ParamBin),

        begin
            Elem = #resource{type=ResouceType, number=ResourceObjectNumber},
            %% Reply is a reference
            {ok, NewRegister, Ref} = 
            xapian_register:put(Register, FromPid, Elem),
            NewState = State#state{register = NewRegister},
            {reply, {ok, Ref}, NewState}
        end]));


handle_call({transaction, Ref}, From, State) ->
    #state{ port = Port } = State,
    {FromPid, FromRef} = From,
    case port_start_transaction(Port) of
        started ->
            %% Clone this server
            Args = [{from_state, State#state{master = self()}}],
            {ok, NewServer} = gen_server:start_link(?MODULE, Args, []),

            %% Change the owner of the port
            erlang:port_connect(Port, NewServer),

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


handle_info(#'DOWN'{ref=Ref, type=process}, State) ->
    #state{ 
        port = Port, 
        register = Register } = State,
    case xapian_register:erase(Register, Ref) of
        {ok, NewRegister, Elem} ->
            #resource{type=ResourceType, number=ResourceNum} = Elem,
            %% TODO: handle a return value
            port_release_resource(Port, ResourceType, ResourceNum),
            NewState = State#state{register = NewRegister},
            {noreply, NewState};

        {error, _Reason} = Error ->
           {noreply, State}
    end.



terminate(_Reason, State = #state{master = undefined}) ->
    ok;

terminate(_Reason, State = #state{master = Master, port = Port}) ->
    %% Change the owner of the port
    erlang:port_connect(Port, Master),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

document_encode(Document, #state{
        name_to_prefix = Name2Prefix,
        name_to_slot = Name2Slot
    }) ->
    xapian_document:encode(Document, Name2Prefix, Name2Slot).

load_driver() ->
    PrivDir = code:priv_dir(xapian),
    case erl_ddll:load_driver(PrivDir, ?DRIVER_NAME) of
    ok -> ok;
    {error, Error} -> 
        Message = erl_ddll:format_error(Error),
        error_logger:error_msg("[~s:~w] Cannot load ~s~n" 
                "From: ~ts~n"
                "Error: ~s~n"
                "Error code: ~w~n", 
            [?MODULE_STRING, ?LINE, ?DRIVER_NAME, PrivDir, Message, Error]),
        erlang:exit(bad_lib)
    end.

%% Command ids
%% Returns an operation for port_control/3 
command_id(open)                        -> 0;
command_id(last_document_id)            -> 1;
command_id(add_document)                -> 2;
command_id(test)                        -> 3;
command_id(read_document_by_id)         -> 4;
command_id(start_transaction)           -> 5;
command_id(cancel_transaction)          -> 6;
command_id(commit_transaction)          -> 7;
command_id(query_page)                  -> 8;
command_id(set_default_stemmer)         -> 9;
command_id(set_default_prefixes)        -> 10;
command_id(enquire)                     -> 11;
command_id(release_resource)            -> 12;
command_id(match_set)                   -> 13;
command_id(qlc_init)                    -> 14;
command_id(qlc_next_portion)            -> 15;
command_id(qlc_lookup)                  -> 16;
command_id(get_resource_info)           -> 17;
command_id(create_resource)             -> 18.


open_mode_id(read_open)                 -> 0;
open_mode_id(write_create_or_open)      -> 1;
open_mode_id(write_create)              -> 2;
open_mode_id(write_create_or_overwrite) -> 3;
open_mode_id(write_open)                -> 4.


%% RESOURCE_TYPE_ID_MARK
resource_type_id(enquire)         -> 0;
resource_type_id(mset)            -> 1;
resource_type_id(qlc)             -> 2;
resource_type_id(weight)          -> 3;
resource_type_id(key_maker)       -> 4;
resource_type_id(x_query)         -> 5;
resource_type_id(match_decider)   -> 6;
resource_type_id(stem)            -> 7;
resource_type_id(date_value_range_processor) -> 8;
resource_type_id(match_spy)       -> 9;
resource_type_id(last)            -> 9.


resource_type_name(0) -> enquire;
resource_type_name(1) -> mset;
resource_type_name(2) -> qlc;
resource_type_name(3) -> weight;
resource_type_name(4) -> key_maker;
resource_type_name(5) -> x_query;
resource_type_name(6) -> match_decider;
resource_type_name(7) -> stem;
resource_type_name(8) -> date_value_range_processor;
resource_type_name(9) -> match_spy.


test_id(result_encoder) -> 1;
test_id(exception) -> 2.


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
        Create -> create;
        true -> write_open
    end.


control(Port, Operation) ->
    control(Port, Operation, <<>>).


control(Port, Operation, Data) ->
    <<Status:8/native-unsigned-integer, Result/binary>> = 
        erlang:port_control(Port, command_id(Operation), Data),
    case Status of
        0 -> {ok, Result};
        1 -> 
            {Type, Bin1} = read_string(Result),
            {Mess, <<>>} = read_string(Bin1),
            {error, #x_error{type=Type, reason=Mess}}
    end.


open_database(Port, Path, Params) ->
    PathBin = erlang:iolist_to_binary(Path),
    PathLen = erlang:byte_size(PathBin),
    Mode = open_mode_id(open_mode(Params)),
    Data = <<PathLen:32/native-signed-integer, 
             PathBin/binary, 
             Mode:8/native-signed-integer>>,
    control(Port, open, Data).


set_default_stemmer(Port, false) ->
    {ok, <<>>};

set_default_stemmer(Port, DefaultStemmer) ->
    Data = xapian_encode:append_prefix(DefaultStemmer, <<>>),
    control(Port, set_default_stemmer, Data).


set_default_prefixes(Port, []) ->
    {ok, <<>>};

set_default_prefixes(Port, DefaultPrefixes) ->
    Data = 
    lists:foldl(fun xapian_encode:append_prefix/2, 
        append_uint(erlang:length(DefaultPrefixes), <<>>), 
        DefaultPrefixes),
    control(Port, set_default_prefixes, Data).


port_add_document(Port, EncodedDocument) ->
    decode_docid_result(control(Port, add_document, EncodedDocument)).


port_last_document_id(Port) ->
    decode_docid_result(control(Port, last_document_id)).


port_test(Port, result_encoder, [From, To]) ->
    Num = test_id(result_encoder),
    Bin@ = <<>>,
    Bin@ = append_int8(Num, Bin@),
    Bin@ = append_document_id(From, Bin@),
    Bin@ = append_document_id(To, Bin@),
    control(Port, test, Bin@);

port_test(Port, exception, []) ->
    Num = test_id(exception),
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
port_read_document_by_id(Port, Id, Meta, Name2Slot) ->
    Bin@ = <<>>,
    Bin@ = append_document_id(Id, Bin@),
    Bin@ = xapian_record:encode(Meta, Name2Slot, Bin@),
    decode_record_result(control(Port, read_document_by_id, Bin@), Meta).


port_query_page(Port, Offset, PageSize, Query, Meta, Name2Slot) ->
    Bin@ = <<>>,
    Bin@ = append_uint(Offset, Bin@),
    Bin@ = append_uint(PageSize, Bin@),
    Bin@ = xapian_query:encode(Query, Name2Slot, Bin@),
    Bin@ = xapian_record:encode(Meta, Name2Slot, Bin@),
    decode_records_result(control(Port, query_page, Bin@), Meta).


port_enquire(Port, Enquire, Name2Slot, Register) ->
    Bin@ = <<>>,
    Bin@ = xapian_enquire:encode(Enquire, Name2Slot, Register, Bin@),
    decode_resource_result(control(Port, enquire, Bin@)).


port_match_set(Port, MSetResourceNum, From, MaxItems) ->
    From1 = 
        if 
            is_integer(From), From > 0 ->
                From;
            true ->
                0
        end,
    Bin@ = <<>>,
    Bin@ = append_uint(MSetResourceNum, Bin@),
    Bin@ = append_uint(From1, Bin@),
    Bin@ = 
        if 
            is_integer(MaxItems), From >= 0 ->
                append_uint(From, append_uint8(0, Bin@));
            true ->
                %% Value is undefined.
                append_uint8(1, Bin@)
        end,
    decode_resource_result(control(Port, match_set, Bin@)).


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


port_qlc_lookup(Port, QlcResNum, DocIds) ->
    Bin@ = <<>>,
    Bin@ = append_uint(QlcResNum, Bin@),
    Bin@ = lists:foldl(fun xapian_common:append_document_id/2, Bin@, DocIds),
    Bin@ = append_document_id(0, Bin@),
    control(Port, qlc_lookup, Bin@).


port_create_resource(Port, ResouceType, UserResourceNumber, ParamBin) ->
    Bin@ = <<>>,
    Bin@ = append_uint8(resource_type_id(ResouceType), Bin@),
    Bin@ = append_uint(UserResourceNumber, Bin@),
    Bin@ = <<Bin@/binary, ParamBin/binary>>,
    decode_resource_result(control(Port, create_resource, Bin@)).
    


port_get_resource_info(Port) ->
    decode_resource_info(control(Port, get_resource_info, <<>>)).


port_qlc_init(State, ResourceType, ResourceNum, Params) ->
    #state{ port = Port } = State,
    Bin@ = <<>>,
    Bin@ = append_uint8(resource_type_id(ResourceType), Bin@),
    Bin@ = append_uint(ResourceNum, Bin@),
    Bin@ = append_qlc_parameters(State, ResourceType, Params, Bin@),
    decode_qlc_info_result(control(Port, qlc_init, Bin@)).


append_qlc_parameters(State, mset, Params, Bin) ->
    #state{ name_to_slot = Name2Slot } = State,
    #internal_qlc_mset_parameters{ record_info = Meta } = Params,
    xapian_record:encode(Meta, Name2Slot, Bin).



%% -----------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------


decode_record_result({ok, Bin}, Meta) ->
    case xapian_record:decode(Meta, Bin) of
        {Rec, Rem} -> {ok, Rec}
    end;

decode_record_result(Other, _Meta) -> 
    Other.


decode_records_result({ok, Bin}, Meta) ->
    case xapian_record:decode_list(Meta, Bin) of
        {Recs, Rem} -> {ok, Recs}
    end;

decode_records_result(Other, _Meta) -> 
    Other.


decode_docid_result({ok, Bin}) -> 
    {Last, <<>>} = read_document_id(Bin),
    {ok, Last};

decode_docid_result(Other) -> 
    Other.


decode_resource_result({ok, Bin}) -> 
    {Id, <<>>} = read_uint(Bin),
    {ok, Id};

decode_resource_result(Other) -> 
    Other.


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
%% Helpers for monitoring.
%% -----------------------------------------------------------------

%% @doc Sends `{DOWN, Tag, process, Pid, Reason}' to Reciever when one of the 
%%      process deads.
%%      It helps monitor the group of the process using one Tag.
%%      Returned value must be used as `stop_monitor(Monitor)' to release 
%%      resources.
-spec monitor_group(pid(), [pid()], term()) -> pid().

monitor_group(Reciever, Servers, Tag) ->
    Monitor = fun() ->
        [ erlang:monitor(process, Server) || Server <- Servers ],
        monitor_cycle(Reciever, Tag)
        end,
    erlang:spawn_link(Monitor).

%% @doc Resend _all_ received monitors to `Reciever', tagged with Tag.
monitor_cycle(Reciever, Tag) ->
    receive
        {'DOWN', _Ref, process, Pid, Reason} ->
            Reciever ! {'DOWN', Tag, process, Pid, Reason},
            monitor_cycle(Reciever, Tag)
    end.

stop_monitor(Monitor) ->
    erlang:exit(Monitor, normal).




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

