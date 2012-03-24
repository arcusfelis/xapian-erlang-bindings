-module(xapian_drv).
-behaviour(gen_server).

%% Used in handlers
-define(SERVER, ?MODULE).

%% Used for testing, then can be moved to an another file
-define(DRV, ?MODULE).

-define(DRIVER_NAME, "xapian_drv").

-record(state, {
    port :: port()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         open/3]).

-export([test/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    load_driver(),
    gen_server:start_link(?MODULE, [], []).


open(Server, Path, Params) ->
    gen_server:call(Server, {open, Path, Params}).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    Port = erlang:open_port({spawn, ?DRIVER_NAME}, []),
    S = #state{
        port = Port
    },
    {ok, S}.

handle_call({open, Path, Params}, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_open(Port, Path, Params),
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


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
command_id(open) -> 0.


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


port_close() ->
    ok.


test() ->
    Path = filename:join(code:priv_dir(xapian), test_db),
    Params = [write, create, overwrite],
    {ok, Server} = ?DRV:start_link(),
    ?DRV:open(Server, Path, Params).
