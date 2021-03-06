%%% @doc Encodes and decodes information about the database.
-module(xapian_db_info).
-export([encode/3,
         decode/2,
         properties/0]).

-import(xapian_common, [
    append_param/2,
    append_stop/1,
    append_string/2,
    append_slot/3,
    append_document_id/2,
    read_document_count/1,
    read_string/1,
    read_boolean/1,
    read_document_id/1,
    read_document_length/1,
    read_term_count/1,
    read_maybe/2
]).

-import(xapian_const, [db_info_param_id/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
    
encode(Params, N2S, Bin) when is_list(Params) ->
    encode_params(Params, N2S, Bin);

encode(Param, N2S, Bin) ->
    append_stop(append_db_info_param(Param, N2S, Bin)).

    
decode(Params, Bin) when is_list(Params) ->
    {RevRes, RemBin} = 
    lists:foldl(fun decode_db_info_param/2, {[], Bin}, Params),
    {lists:reverse(RevRes), RemBin};

decode(Param, Bin) ->
    decode_db_info_param2(Param, Bin).

%% -------------------------------------------------------------------

encode_params([Param|Params], N2S, Bin) ->
    Bin2 = append_db_info_param(Param, N2S, Bin),
    encode_params(Params, N2S, Bin2);
encode_params([], _N2S, Bin) ->
    append_stop(Bin).


append_db_info_param(Param, _N2S, Bin) when is_atom(Param) ->
    true = lists:member(Param, properties()),
    append_param(db_info_param_id(Param), Bin);

append_db_info_param({Param, Value}, N2S, Bin) when is_atom(Param) ->
    encode_param(Param, Value, N2S, append_param(db_info_param_id(Param), Bin)).


decode_db_info_param(Param, {Acc, Bin}) ->
    {Value, RemBin} = decode_db_info_param2(Param, Bin),
    {[{Param, Value}|Acc], RemBin}.


decode_db_info_param2(Param, Bin) when is_atom(Param) ->
    decode_param(Param, Bin);

decode_db_info_param2({Param, _Term}, Bin) when is_atom(Param) ->
    decode_param(Param, Bin).


%% These properties can be accessed without an additional parameter.
properties() ->
    [ average_length
    , document_count
    , document_length_lower_bound
    , document_length_upper_bound
    , has_positions
    , last_document_id
    , uuid].


-ifdef(TEST).

properties_test_() ->
    [?_assertEqual(properties(), lists:sort(properties()))].

-endif.


decode_param(has_positions, Bin) ->
    read_boolean(Bin);

decode_param(document_count, Bin) ->
    read_document_count(Bin);

decode_param(last_document_id, Bin) ->
    read_document_id(Bin);

decode_param(average_length, Bin) ->
    read_document_length(Bin);

decode_param(term_exists, Bin) ->
    read_boolean(Bin);

decode_param(term_freq, Bin) ->
    read_maybe(fun xapian_common:read_document_count/1, Bin);

decode_param(collection_freq, Bin) ->
    read_maybe(fun xapian_common:read_term_count/1, Bin);

decode_param(value_freq, Bin) ->
    read_document_count(Bin);

decode_param(value_lower_bound, Bin) ->
    read_string(Bin);

decode_param(value_upper_bound, Bin) ->
    read_string(Bin);

decode_param(document_length_lower_bound, Bin) ->
    read_term_count(Bin);

decode_param(document_length_upper_bound, Bin) ->
    read_term_count(Bin);

decode_param(wdf_upper_bound, Bin) ->
    read_maybe(fun xapian_common:read_term_count/1, Bin);

decode_param(document_length, Bin) ->
    %% We did not use `read_document_length' here, 
    %% because it is for double, while uint is wanted.
    read_maybe(fun xapian_common:read_term_count/1, Bin);

decode_param(uuid, Bin) ->
    read_string(Bin);

decode_param(metadata, Bin) ->
    read_string(Bin).


encode_param(term_exists, Value, _N2S, Bin) ->
    append_string(Value, Bin);

encode_param(term_freq, Value, _N2S, Bin) ->
    append_string(Value, Bin);

encode_param(collection_freq, Value, _N2S, Bin) ->
    append_string(Value, Bin);

encode_param(value_freq, Value, N2S, Bin) ->
    append_slot(Value, N2S, Bin);

encode_param(value_lower_bound, Value, N2S, Bin) ->
    append_slot(Value, N2S, Bin);

encode_param(value_upper_bound, Value, N2S, Bin) ->
    append_slot(Value, N2S, Bin);

encode_param(wdf_upper_bound, Value, _N2S, Bin) ->
    append_string(Value, Bin);

encode_param(document_length, Value, _N2S, Bin) ->
    append_document_id(Value, Bin);

encode_param(metadata, Value, _N2S, Bin) ->
    append_string(Value, Bin).

