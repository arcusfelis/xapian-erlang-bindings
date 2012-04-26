%%% The client code can form records using few different formats.
%%% This module translate records into a standard form.
-module(xapian_check).
-export([check_prefix/1]).
-include_lib("xapian/include/xapian.hrl").

check_prefix(Rec = #x_prefix_name{prefix=Prefix}) ->
    Rec#x_prefix_name{
        prefix=prefix_to_binary(Prefix)
    }.


%% Encode a prefix from `#x_prefix_name.prefix' for passing to Xapian.
prefix_to_binary(B) when is_binary(B) ->
    B;

prefix_to_binary([_|_]=L) ->
    iolist_to_binary(L);

prefix_to_binary(C) when is_integer(C) ->
    prefix_to_binary([C]).
