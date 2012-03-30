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
    name_to_slot :: ordict:orddict()
}).

-include_lib("xapian/include/xapian.hrl").



%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/2,
         last_document_id/1,
         close/1]).

%% For writable DB
-export([add_document/2]).

-export([test/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

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
    {ok, S}.
    

handle_call(last_document_id, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_last_document_id(Port),
    {reply, Reply, State};

handle_call(close, _From, State) ->
    #state{ port = Port } = State,
    Reply = port_close(Port),
    Reason = normal,
    {stop, Reason, Reply, State};

handle_call({add_document, Document}, _From, State) ->
    #state{ port = Port } = State,
    EncodedDocument = document_encode(Document, State),
    Reply = port_add_document(Port, EncodedDocument),
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


test() ->
    % Open test
    Path = filename:join(code:priv_dir(xapian), test_db),
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

