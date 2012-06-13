-module(xapian_port).
-export([open/1,
         close/1,
         connect/2,
         control/3,
         is_port_alive/1]).

-define(DRIVER_NAME, "xapian_drv").
-define(PORT_NAME,   "xapian_port").

-record(port_rec, {
        port :: port(), 
        type :: atom()
}).

load_driver() ->
    PrivDir = code:priv_dir(xapian),
    case erl_ddll:load_driver(PrivDir, ?DRIVER_NAME) of
    ok -> ok;
    {error, Error} -> 
        Message = erl_ddll:format_error(Error),
        error_logger:error_msg("[~s:~w] Cannot load ~s~n" 
                "From: ~ts~n"
                "Error message: ~s~n"
                "Error code: ~w~n",
            [?MODULE_STRING, ?LINE, ?DRIVER_NAME, PrivDir, Message, Error]),
        erlang:exit(bad_lib)
    end.


open(Type) ->
    #port_rec{type = Type, port = open_port(Type)}.


close(#port_rec{port = Port}) ->
    erlang:port_close(Port).


connect(#port_rec{port = Port}, NewOwnerPid) ->
    erlang:port_connect(Port, NewOwnerPid).


control(#port_rec{port = Port, type = driver}, Command, Data) ->
    erlang:port_control(Port, Command, Data);

control(#port_rec{port = Port, type = port}, Command, Data) ->
    Mess = <<Command:32/unsigned-native-integer, Data/binary>>,
    port_command(Port, Mess),
    receive
        {Port, {data, AnswerData}} ->
            AnswerData;
        {'EXIT', Port, Reason} ->
            erlang:error({port_exit, Reason});
        {Port, {exit_status, _Status}} ->
            erlang:error(port_exit)
    after 1000 ->
            erlang:error(port_timeout)
    end.
            


open_port(driver) ->
    load_driver(),
    erlang:open_port({spawn, ?DRIVER_NAME}, []);

open_port(port) ->
    PrivDir = code:priv_dir(xapian),
    Exe = filename:join(PrivDir, ?PORT_NAME),
    Opts = [{packet, 4}, binary, exit_status, use_stdio],
    erlang:open_port({spawn_executable, Exe}, Opts).


is_port_alive(#port_rec{port = Port}) ->
    undefined =/= erlang:port_info(Port, id).
