-module(xapian_spy_info).
-export([encode/2,
         decode/2,
         value_count_properties/0]).

-import(xapian_common, [
    append_param/2,
    append_stop/1,
    read_document_count/1,
    read_slot/1
]).

-import(xapian_const, [spy_info_param_id/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

encode(Params, Bin) when is_list(Params) ->
    append_stop(lists:foldl(fun append_spy_info_param/2, Bin, Params));

encode(Param, Bin) ->
    append_stop(append_spy_info_param(Param, Bin)).

    
decode(Params, Bin) when is_list(Params) ->
    {RevRes, RemBin} = 
    lists:foldl(fun decode_spy_info_param/2, {[], Bin}, Params),
    {lists:reverse(RevRes), RemBin};

decode(Param, Bin) ->
    decode_spy_info_param2(Param, Bin).


append_spy_info_param(Param, Bin) when is_atom(Param) ->
    append_param(spy_info_param_id(Param), Bin).


decode_spy_info_param(Param, {Acc, Bin}) ->
    {Value, RemBin} = decode_spy_info_param2(Param, Bin),
    {[{Param, Value}|Acc], RemBin}.


decode_spy_info_param2(Param, Bin) when is_atom(Param) ->
    decode_param(Param, Bin).


%% @doc Return the ordered list of properties.
%% These properties can be accessed without an additional parameter.
value_count_properties() ->
    [ document_count
    , value_slot
    ].


-ifdef(TEST).

properties_test_() ->
    [?_assertEqual(value_count_properties(), lists:sort(value_count_properties()))].

-endif.

decode_param(document_count, Bin) ->
    read_document_count(Bin);

decode_param(value_slot, Bin) ->
    read_slot(Bin).
