%%% TODO: Add xapian-compact support.
-module(xapian_utils).

-behaviour(gen_server).

%% API
-export([tcp_server/2, 
         replicate_client/2, 
         replicate_server/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% -------------------------------------------------------------------
%%  Internal records 
%% -------------------------------------------------------------------
-record(state, {port}).


%% -------------------------------------------------------------------
%%  Internal types
%% -------------------------------------------------------------------

-type start_result() :: {ok, pid()} | {error, term()}.

-type replicate_server_param() :: 
      link 
    | one_shot 
    | {port, xapian:x_inet_port()}          %% required
    | {address, xapian:x_inet_address()}.

-type replicate_client_param() :: 
      writable 
    | link 
    | one_shot 
    | {interval, xapian:x_timeout()}        %% in seconds
    | {reader_time, xapian:x_timeout()}     %% in seconds
    | {port, xapian:x_inet_port()}          %% required
    | {address, xapian:x_init_address()}    %% required
    | {master_name, xapian:x_string()}.

-type tcp_server_param() :: 
      writable 
    | {timeout, xapian:x_timeout()}
    | link
    | {idle_timeout, xapian:x_timeout()}
    | {active_timeout, xapian:x_timeout()}
    | one_shot 
    | {port, xapian:x_inet_port()} %% required
    | {address, xapian:x_init_address()}.


%% -------------------------------------------------------------------
%%  API
%% -------------------------------------------------------------------

-spec tcp_server([string()], [tcp_server_param()]) -> start_result().

tcp_server(DbDirs, Params) ->
    %% xapian-tcpsrv [OPTIONS] DATABASE_DIRECTORY...
    Writable = lists:member(writable, Params),
    WritableOpt = if 
        Writable, length(DbDirs) =:= 1 -> ["--writable"]; 
        Writable -> erlang:error(multiple_writable_dbs);
        true -> [] end,

    OneShot = lists:member(one_shot, Params),
    OneShotOpt = if OneShot -> ["--one-shot"]; true -> [] end,

    Addr = proplists:get_value(address, Params),
    AddrOpt = case Addr of undefined -> []; _Addr -> ["--address", Addr] end,

    Port = proplists:get_value(port, Params),
    PortOpt = 
        case Port of undefined -> 
            erlang:error(undefined_port); 
            _Port -> ["--port", integer_to_list(Port)] 
    end,

    Timeout = proplists:get_value(timeout, Params),
    TimeoutOpt = encode_timeout("timeout", Timeout),

    IdleTimeout = proplists:get_value(idle_timeout, Params),
    IdleTimeoutOpt = encode_timeout("idle-timeout", IdleTimeout),

    ActTimeout = proplists:get_value(active_timeout, Params),
    ActTimeoutOpt = encode_timeout("active-timeout", ActTimeout),

    Args = ["xapian-tcpsrv", "--quiet"] 
           ++ WritableOpt ++ OneShotOpt ++ AddrOpt ++ PortOpt
           ++ TimeoutOpt ++ IdleTimeoutOpt ++ ActTimeoutOpt
           ++ DbDirs,
    start(Args, Params).
           

% Missing params:
% -v, --verbose       be more verbose
-spec replicate_client(string(), [replicate_client_param()]) -> start_result().

replicate_client(DbDir, Params) ->
    %% xapian-replicate [OPTIONS] DATABASE_DIRECTORY
    OneShot = lists:member(one_shot, Params),
    OneShotOpt = if OneShot -> ["--one-shot"]; true -> [] end,
    MasterName = proplists:get_value(master_name, Params),
    MasterNameOpt = 
        case MasterName of 
            undefined -> [];
            _MasterName -> ["--master", MasterName] 
        end,

    Addr = proplists:get_value(address, Params),
    AddrOpt = 
        case Addr of
            undefined -> erlang:error(undefined_address); 
            _Addr -> ["--address", Addr] 
        end,

    Port = proplists:get_value(port, Params),
    PortOpt = 
        case Port of 
            undefined -> erlang:error(undefined_port); 
            _Port -> ["--port", integer_to_list(Port)] 
        end,

    Interval = proplists:get_value(interval, Params),
    IntervalOpt = encode_timeout("interval", Interval),

    ReaderTime = proplists:get_value(reader_time, Params),
    ReaderTimeOpt = encode_timeout("reader-time", ReaderTime),

    Args = ["xapian-replicate"] ++ OneShotOpt 
           ++ AddrOpt ++ PortOpt ++ MasterNameOpt
           ++ IntervalOpt ++ ReaderTimeOpt ++ [DbDir],
    start(Args, Params). 
           

%% `DirWithDatabases' is a parent directory with replicated databases.
-spec replicate_server(string(), [replicate_server_param()]) -> start_result().

replicate_server(DirWithDatabases, Params) ->
    %% xapian-replicate-server [OPTIONS] DATABASE_DIRECTORY
    OneShot = lists:member(one_shot, Params),
    OneShotOpt = if OneShot -> ["--one-shot"]; true -> [] end,

    Addr = proplists:get_value(address, Params),
    AddrOpt = 
        case Addr of
            undefined -> []; 
            _Addr -> ["--address", Addr] 
        end,

    Port = proplists:get_value(port, Params),
    PortOpt = 
        case Port of 
            undefined -> erlang:error(undefined_port); 
            _Port -> ["--port", integer_to_list(Port)] 
        end,

    Args = ["xapian-replicate-server"] ++ OneShotOpt 
           ++ AddrOpt ++ PortOpt ++ [DirWithDatabases],
    start(Args, Params).


start(Args, Params) ->
    case lists:member(link, Params) of
        true ->
            gen_server:start_link(?MODULE, Args, []);
        false ->
            gen_server:link(?MODULE, Args, [])
    end.

%% -------------------------------------------------------------------
%%  gen_server callbacks
%% -------------------------------------------------------------------

init([Cmd|Args]) ->
   Port = open_port({spawn_executable, os:find_executable(Cmd)},
                     [{args, Args}, exit_status]),
    {ok, #state{port = Port}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------------
%%  Internal functions
%% -------------------------------------------------------------------

encode_timeout(_Name, undefined) ->
    [];

encode_timeout(Name, Timeout) ->
    TimeoutOpt = io_lib:format("~b", [Timeout]),
    [Name, TimeoutOpt]. 
