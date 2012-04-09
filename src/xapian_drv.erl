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

    %% Pid of the real server (used by a transaction).
    %% If the process will be terminated, then new owner of the port will 
    %% be master.
    master
}).

-include_lib("xapian/include/xapian.hrl").
-define(DOCUMENT_ID(X), X:32/native-unsigned-integer).



%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/2,
         last_document_id/1,
         read_document/3,
         close/1]).

%% For writable DB
-export([add_document/2,
         transaction/3]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type x_server() :: xapian:x_server().
-type x_transaction() :: xapian:x_transaction().


%% ------------------------------------------------------------------
%% Tiny hacks
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec open(term(), [term()]) -> {ok, x_server()}.

open(Path, Params) ->
    load_driver(),
    Args = [Path, Params],
    gen_server:start_link(?MODULE, Args, []).


last_document_id(Server) ->
    call(Server, last_document_id).


close(Server) ->
    gen_server:call(Server, close).


read_document(Server, DocId, RecordMetaDefinition) ->
    call(Server, {read_document_by_id, DocId, RecordMetaDefinition}).


%% ------------------------------------------------------------------
%% API Function Definitions for writable DB
%% ------------------------------------------------------------------

add_document(Server, Document) ->
    call(Server, {add_document, Document}).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------
run_test(Server, TestName, Params) ->
    call(Server, {test, TestName, Params}).



%% @doc Runs function F for Servers as a transaction.
%%      Transaction will stop other operations with selected databases.
%%
%% This function runs a transaction on few servers.
%% Results from a server:
%%
%% * `committed' - A transaction was pass on this server. Data is consistent;
%% * `aborted' - A transaction was canceled on this server. Data is consistent;
%% * `failed' - An exeption was occured. Data is inconsistent.
%% 
%% If one of the servers crashed during transaction, the transaction process 
%% will be killed using `cancel_transaction' with reason `crashed_server'.
-spec transaction([x_server()], x_transaction(), timeout()) -> term().
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
            [ close(Server) || Server <- TransServers ],
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
    Name2Prefix = 
    [{Name, Prefix} 
        || #x_prefix_name{name = Name, prefix = Prefix} <- Params],

    Name2Slot = 
    [{Name, Slot} 
        || #x_value_name{name = Name, slot = Slot} <- Params],

    Name2PrefixDict = orddict:from_list(Name2Prefix),
    Name2SlotDict   = orddict:from_list(Name2Slot),

    Port = erlang:open_port({spawn, ?DRIVER_NAME}, []),
    Result = port_open(Port, Path, Params),
    case Result of
        {ok, <<>>} ->
            S = #state{
                port = Port,
                name_to_prefix = Name2PrefixDict,
                name_to_slot = Name2SlotDict
            },
            {ok, S};
        {error, Error} -> {error, Error}
    end;

%% Add a copy of the server for the transaction
init([{from_state, State}]) ->
    {ok, State}.
    

handle_call(last_document_id, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_last_document_id(Port),
    {reply, Reply, State};

handle_call(close, _From, State) ->
    #state{ port = Port } = State,
    Reply = ok,
    Reason = normal,
    {stop, Reason, Reply, State};

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

handle_info(_Info, State) ->
    {noreply, State}.

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
command_id(open) ->                        0;
command_id(last_document_id) ->            1;
command_id(add_document) ->                2;
command_id(test) ->                        3;
command_id(read_document_by_id) ->         4;
command_id(start_transaction) ->           5;
command_id(cancel_transaction) ->          6;
command_id(commit_transaction) ->          7.


open_mode_id(read_open) ->                 0;
open_mode_id(write_create_or_open) ->      1;
open_mode_id(write_create) ->              2;
open_mode_id(write_create_or_overwrite) -> 3;
open_mode_id(write_open) ->                4.


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


port_open(Port, Path, Params) ->
    PathBin = erlang:iolist_to_binary(Path),
    PathLen = erlang:byte_size(PathBin),
    Mode = open_mode_id(open_mode(Params)),
    Data = <<PathLen:32/native-signed-integer, 
             PathBin/binary, 
             Mode:8/native-signed-integer>>,
    control(Port, open, Data).


port_add_document(Port, EncodedDocument) ->
    decode_docid_result(control(Port, add_document, EncodedDocument)).


port_last_document_id(Port) ->
    decode_docid_result(control(Port, last_document_id)).


port_test(Port, result_encoder, [From, To]) ->
    Num = test_id(result_encoder),
    Data = <<Num:8/native-signed-integer, ?DOCUMENT_ID(From), ?DOCUMENT_ID(To)>>,
    control(Port, test, Data);

port_test(Port, exception, []) ->
    Num = test_id(exception),
    Data = <<Num:8/native-signed-integer>>,
    control(Port, test, Data).



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
    RecordDefinition = xapian_record:encode(Meta, Name2Slot),
    Data = <<?DOCUMENT_ID(Id), RecordDefinition/binary>>,
    decode_record_result(control(Port, read_document_by_id, Data), Meta).


decode_record_result({ok, Bin}, Meta) ->
    {ok, xapian_record:decode(Meta, Bin)};

decode_record_result(Other, _Meta) -> 
    Other.


decode_docid_result({ok, <<?DOCUMENT_ID(Last)>>}) -> 
    {ok, Last};

decode_docid_result(Other) -> 
    Other.


read_string(Bin) ->
    <<Num:32/native-unsigned-integer, Bin2/binary>> = Bin,  
    <<Str:Num/binary, Bin3/binary>> = Bin2,
    {Str, Bin3}.


%% -----------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------



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

simple_test() ->
    % Open test
    Path = filename:join([code:priv_dir(xapian), test_db, simple]),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = slot1}, 
        #x_prefix_name{name = author, prefix = <<$A>>}],
    {ok, Server} = ?DRV:open(Path, Params),
    Document =
        [ #x_stemmer{language = <<"english">>}
        , #x_data{value = "My test data as iolist"} 
        , #x_term{value = "Simple term"} 
        , #x_value{slot = 0, value = "Slot #0"} 
        , #x_value{slot = slot1, value = "Slot #1"} 
        , #x_text{value = "Paragraph 1"} 
        , #x_delta{}
        , #x_text{value = <<"Paragraph 2">>} 
        , #x_text{value = <<"Michael">>, prefix = author} 
        ],
    DocId = ?DRV:add_document(Server, Document),
    ?assert(is_integer(DocId)),
    Last = ?DRV:last_document_id(Server),
    ?DRV:close(Server),
    Last.


transaction_test_() ->
    % Open test
    Path1 = filename:join([code:priv_dir(xapian), test_db, transaction1]),
    Path2 = filename:join([code:priv_dir(xapian), test_db, transaction2]),
    Params = [write, create, overwrite],
    {ok, Server1} = ?DRV:open(Path1, Params),
    {ok, Server2} = ?DRV:open(Path2, Params),
    Fun = fun([S1, S2]) ->
        test_result
        end,
    BadFun = fun([S1, S2]) ->
        test_result = 1
        end,
    Result1 = transaction([Server1, Server2], Fun, infinity),
    Result2 = transaction([Server1, Server2], BadFun, infinity),
    ?DRV:close(Server1),
    ?DRV:close(Server2),
    #x_transaction_result{
        is_committed=Committed1,
        is_consistent=Consistent1
    } = Result1,
    #x_transaction_result{
        is_committed=Committed2,
        is_consistent=Consistent2
    } = Result2,
    {"Check transactions' results for good and bad functions.",
        [ ?_assertEqual(Committed1, true)
        , ?_assertEqual(Consistent1, true)
        , ?_assertEqual(Committed2, false)
        , ?_assertEqual(Consistent2, true)
        ]}.


transaction_readonly_error_test_() ->
    % Open test
    Path = filename:join([code:priv_dir(xapian), test_db, transaction1]),
    Params = [],
    {ok, Server} = ?DRV:open(Path, Params),
    Fun = fun([S]) ->
        test_result
        end,
    Result = transaction([Server], Fun, infinity),
    ?DRV:close(Server),
    #x_transaction_result{
        is_committed=Committed,
        is_consistent=Consistent,
        reason=Reason
    } = Result,
    {"Cannot start transaction for readonly server.",
        [ ?_assertEqual(Committed, false)
        , ?_assertEqual(Consistent, true)
        , ?_assertEqual(Reason, readonly_db)
        ]}.
    

    
    

result_encoder_test() ->
    % Open test
    Path = filename:join([code:priv_dir(xapian), test_db, simple]),
    Params = [],
    {ok, Server} = ?DRV:open(Path, Params),
    Reply = run_test(Server, result_encoder, [1, 1000]),
    Reply = run_test(Server, result_encoder, [1, 1000]),
    Reply = run_test(Server, result_encoder, [1, 1000]),
    ?DRV:close(Server),
    ?assertEqual(lists:seq(1, 1000), [ Id || <<?DOCUMENT_ID(Id)>> <= Reply ]),
    ok.
    

%% @doc Check an exception.
exception_test() ->
    Path = filename:join([code:priv_dir(xapian), test_db, simple]),
    Params = [],
    {ok, Server} = ?DRV:open(Path, Params),
    % ?assertException(ClassPattern, TermPattern, Expr)
    ?assertException(error, 
        #x_error{type = <<"MemoryAllocationDriverError">>}, 
        run_test(Server, exception, [])),
    ?DRV:close(Server),
    ok.


-record(rec_test, {docid, slot1, data}).

read_document_test() ->
    % Open test
    Path = filename:join([code:priv_dir(xapian), test_db, read_document]),
    Params = [write, create, overwrite, 
        #x_value_name{slot = 1, name = slot1}],
    {ok, Server} = ?DRV:open(Path, Params),
    Document =
        [ #x_stemmer{language = <<"english">>}
        , #x_data{value = "My test data as iolist"} 
        , #x_value{slot = slot1, value = "Slot #0"} 
        ],
    DocId = ?DRV:add_document(Server, Document),
    Meta = xapian_record:record(rec_test, record_info(fields, rec_test)),
    Rec = ?DRV:read_document(Server, DocId, Meta),
    ?assertEqual(Rec#rec_test.docid, 1),
    ?assertEqual(Rec#rec_test.slot1, <<"Slot #0">>),
    ?assertEqual(Rec#rec_test.data, <<"My test data as iolist">>),
    ?DRV:close(Server).


%% @doc Check an exception.
read_bad_test() ->
    % Open test
    Path = filename:join([code:priv_dir(xapian), test_db, read_document]),
    Params = [#x_value_name{slot = 1, name = slot1}],
    {ok, Server} = ?DRV:open(Path, Params),
    Meta = xapian_record:record(rec_test, record_info(fields, rec_test)),
    DocId = 2,
    % ?assertException(ClassPattern, TermPattern, Expr)
    ?assertException(error, 
        #x_error{type = <<"DocNotFoundError">>}, 
        ?DRV:read_document(Server, DocId, Meta)),
    ?DRV:close(Server).


-endif.



