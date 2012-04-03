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



%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/2,
         last_document_id/1,
         close/1]).

%% For writable DB
-export([add_document/2,
         transaction/3]).

-export([transaction_test/0, simple_test/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type x_server() :: xapian:x_server().


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
    gen_server:call(Server, last_document_id).


close(Server) ->
    gen_server:call(Server, close).


%% ------------------------------------------------------------------
%% API Function Definitions for writable DB
%% ------------------------------------------------------------------

add_document(Server, Document) ->
    gen_server:call(Server, {add_document, Document}).



-type x_transaction() :: fun(([x_server()]) -> term()).


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
    Home = self(),
    TransServers = 
        [ gen_server:call(Server, {transaction, Ref}) 
            || Server <- Servers ],
    
    TransBody = fun() ->
            Result = F(TransServers),
            %% Send the result back.
            Home ! {result, Ref, Result}
        end,
    {TransPid, TransRef} = erlang:spawn_monitor(TransBody),
    GroupMonPid = monitor_group(Home, Servers, Ref),

    Result = 
    receive
        %% Error in the transaction.
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
            

        {result, Ref, ResultI} -> 
            [ close(Server) || Server <- TransServers ],
            Statuses = collect_status_info(Ref, Servers, []),
            Committed = lists:all(fun is_committed/1, Statuses),
            if
                Committed ->
                    #x_transaction_result{
                        is_committed = true,
                        is_consistent = true, 
                        result = ResultI,
                        statuses = Statuses
                    };
                true -> % error
                    #x_transaction_result{
                        is_committed = false,
                        is_consistent = lists:all(fun is_aborted/1, Statuses), 
                        result = ResultI,
                        statuses = Statuses
                    }
            end
    end,
    stop_monitor(GroupMonPid),
    Result.


is_committed({_Pid, committed}) -> true;
is_committed(_) -> false.


is_aborted({_Pid, aborted}) -> true;
is_aborted(_) -> false.


collect_status_info(_Ref, [], StatusList) -> 
    StatusList;

collect_status_info(Ref, Servers, StatusList) ->
    receive
        {transaction_status, Ref, Pid, Status} -> 
            collect_status_info(Ref, Servers -- [Pid], [{Pid, Status} | StatusList]);
            
        {'DOWN', Ref, process, Pid, Reason} ->
            case lists:member(Pid, Servers) of
            true ->
                collect_status_info(Ref, Servers -- [Pid], [{Pid, unknown} | StatusList])
            end
    end.

    
cancel_transaction(Server, Ref) ->
    Server ! {cancel_transaction, Ref}.

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
    port_open(Port, Path, Params),
    S = #state{
        port = Port,
        name_to_prefix = Name2PrefixDict,
        name_to_slot = Name2SlotDict
    },
    {ok, S};

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

handle_call({transaction, Ref}, From, State) ->
    #state{ port = Port } = State,
    {FromPid, FromRef} = From,

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
    {noreply, State}.

port_commit_transaction(Port) -> committed.
port_cancel_transaction(Port) -> aborted.


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
command_id(add_document) ->                2.

open_mode_id(read_open) ->                 0;
open_mode_id(write_create_or_open) ->      1;
open_mode_id(write_create) ->              2;
open_mode_id(write_create_or_overwrite) -> 3;
open_mode_id(write_open) ->                4.


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


control(Port, Operation, Data) ->
    erlang:port_control(Port, command_id(Operation), Data).


port_open(Port, Path, Params) ->
    PathBin = erlang:iolist_to_binary(Path),
    PathLen = erlang:byte_size(PathBin),
    Mode = open_mode_id(open_mode(Params)),
    Data = <<PathLen:32/native-signed-integer, 
             PathBin/binary, 
             Mode:8/native-signed-integer>>,
    control(Port, open, Data).


port_add_document(Port, EncodedDocument) ->
    control(Port, add_document, EncodedDocument).


port_last_document_id(Port) ->
    <<Last:32/native-unsigned-integer>> 
        = control(Port, last_document_id, <<>>),
    Last.


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
    Last = ?DRV:last_document_id(Server),
    ?DRV:close(Server),
    Last.


transaction_test() ->
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
    {Result1, Result2}.
    




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
