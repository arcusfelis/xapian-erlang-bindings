-module(xapian_transaction).
-export([ run_transaction/3
        , report_transaction_status/4
        ]).
-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").

         

%% ------------------------------------------------------------------
%% Transaction internals
%% ------------------------------------------------------------------

run_transaction(Servers, F, Timeout) ->
    Ref = make_ref(),
    ProxyServers = 
        [ lock_server(Server, Ref) 
            || Server <- Servers ],

    ServersReady = lists:all(fun is_pid/1, ProxyServers),
    case ServersReady of
    %% Cannot init the transaction on one or more servers.
    false ->
        cannot_start_transaction(Ref, Servers, ProxyServers);

    %% All servers are ready.
    true ->
        transaction_ready(Ref, Servers, ProxyServers, F, Timeout)
    end.



report_transaction_status(ToPid, TransRef, FromPid, Status) -> 
    ToPid ! {transaction_status, TransRef, FromPid, Status}.


%% ------------------------------------------------------------------
%% transaction Client Helpers
%% ------------------------------------------------------------------

transaction_ready(Ref, Servers, ProxyServers, F, Timeout) ->
    %% We are still in the client process.
    Home = self(),
    TransBody = fun() ->
            Result = F(ProxyServers),
            %% Send the result back.
            Home ! {result, Ref, Result}
        end,
    TransPidRef = erlang:spawn_monitor(TransBody),
    GroupMonPid = monitor_group(Home, Servers, Ref),
    Result = wait_transaction_result(TransPidRef, Ref, Servers, 
                                     ProxyServers, Timeout),
    stop_monitor(GroupMonPid),
    Result.


%% @doc Collects `#x_transaction_result{}' record.
wait_transaction_result({TransPid, TransRef}, Ref, Servers, ProxyServers, 
                        Timeout) ->
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
            %% Kill the transaction process.
            erlang:exit(TransPid, 
                        {transaction_error, {server_down, Reason}}),

            %% Take away the monitor of the transaction process.
            receive
                {'DOWN', TransRef, process, TransPid, _OtherReason} -> ok
            end,

            %% TransPid is dead. ProxyServers are still alive.

            %% If the server is not valid, then is is an error.
            %% The valid server was started, but does not reply yet.
            true = lists:member(Pid, Servers),
            %% Cancel the transaction on other valid servers.
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
            [ catch xapian_server:close(Server) || Server <- ProxyServers ],
            Statuses = collect_status_info(Ref, Servers, []),
            Committed = lists:all(fun is_committed/1, Statuses),

            %% Take away the monitor of the transaction process.
            receive
                {'DOWN', TransRef, process, TransPid, Reason} -> 
                    Reason = normal %% assert
            end,

            #x_transaction_result{
                is_committed = Committed,
                is_consistent = Committed 
                    orelse lists:all(fun is_aborted/1, Statuses),
                result = ResultI,
                statuses = Statuses
            }
    after Timeout ->
        %% Kill the transaction process.
        erlang:exit(TransPid, {transaction_error, timeout}),
        %% Handle the exit of the transaction process.
        wait_transaction_result({TransPid, TransRef}, Ref, 
                                Servers, ProxyServers, Timeout)
    end.


%% @doc In most cases one of the servers is open only for reading, but other errors 
%%      can be also happen.
cannot_start_transaction(Ref, Servers, ProxyServers) ->
    Zipped = lists:zip(Servers, ProxyServers),
    Splitter = fun
        ({_Server, ProxyServer}) -> is_pid(ProxyServer)
        end,
    {Valid, Invalid} = lists:partition(Splitter, Zipped),

    %% The Invalid list has the same format as the Statuses list.
    %% Cancel the transaction on Valid servers.
    ValidServers = 
        [ begin 
            cancel_transaction(Server, Ref), 
            Server %% Return the Pid of the real server
            end || {Server, _ProxyServer} <- Valid ],
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
            
        {'DOWN', Ref, process, Pid, _Reason} ->
            case lists:member(Pid, Servers) of
            true ->
                NewServers = Servers -- [Pid],
                NewStatusList = [{Pid, unknown} | StatusList],
                collect_status_info(Ref, NewServers, NewStatusList)
            end
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


%% -----------------------------------------------------------------
%% Import functions.
%% -----------------------------------------------------------------

cancel_transaction(Server, Ref) ->
    xapian_server:internal_transaction_cancel(Server, Ref).


lock_server(Server, Ref) ->
    xapian_server:internal_transaction_lock_server(Server, Ref).
