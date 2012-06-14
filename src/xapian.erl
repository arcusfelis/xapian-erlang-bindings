%%% @headerfile "xapian.hrl"
-module(xapian).
-export([start/0]).


-include_lib("xapian/include/xapian.hrl").


start() ->
    application:start(xapian).

