-module(xapian_drv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DRIVER_NAME, "xapian_drv").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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


