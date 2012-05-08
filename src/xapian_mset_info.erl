-module(xapian_mset_info).
-export([encode/2,
         decode/2,
         properties/0]).

-import(xapian_common, [
    append_uint8/2,
    append_string/2,
    read_document_count/1,
    read_weight/1
]).

    
encode(Params, Bin) ->
    append_uint8(mset_info_param_id(stop),
        lists:foldl(fun append_mset_info_param/2, Bin, Params)).

    
decode(Params, Bin) ->
    {RevRes, RemBin} = 
    lists:foldl(fun decode_mset_info_param/2, {[], Bin}, Params),
    {lists:reverse(RevRes), RemBin}.


append_mset_info_param(Param, Bin) when is_atom(Param) ->
    true = lists:member(Param, properties()),
    append_uint8(mset_info_param_id(Param), Bin);

append_mset_info_param({Param, Term}, Bin) when is_atom(Param) ->
    append_string(Term, append_uint8(mset_info_param_id(Param), Bin)).


decode_mset_info_param(Param, {Acc, Bin}) when is_atom(Param) ->
    {DecodedParam, RemBin} = decode_param(Param, Bin),
    {[{Param, DecodedParam}|Acc], RemBin};

decode_mset_info_param({Param, _Term} = Id, {Acc, Bin}) when is_atom(Param) ->
    {DecodedParam, RemBin} = decode_param(Param, Bin),
    {[{Id, DecodedParam}|Acc], RemBin}.


%% These properties can be accessed without an additional parameter.
properties() ->
    [matches_lower_bound, matches_estimated, matches_upper_bound,
     uncollapsed_matches_lower_bound, uncollapsed_matches_estimated,
     uncollapsed_matches_upper_bound, size, max_possible, max_attained].


mset_info_param_id(stop)                            -> 0;
mset_info_param_id(matches_lower_bound)             -> 1;
mset_info_param_id(matches_estimated)               -> 2;
mset_info_param_id(matches_upper_bound)             -> 3;
mset_info_param_id(uncollapsed_matches_lower_bound) -> 4;
mset_info_param_id(uncollapsed_matches_estimated)   -> 5;
mset_info_param_id(uncollapsed_matches_upper_bound) -> 6;
mset_info_param_id(size)                            -> 7;
mset_info_param_id(max_possible)                    -> 8;
mset_info_param_id(max_attained)                    -> 9;
mset_info_param_id(term_weight)                     -> 10;
mset_info_param_id(term_freq)                       -> 11.


decode_param(matches_lower_bound, Bin) ->
    read_document_count(Bin);

decode_param(matches_estimated, Bin) ->
    read_document_count(Bin);

decode_param(matches_upper_bound, Bin) ->
    read_document_count(Bin);

decode_param(uncollapsed_matches_lower_bound, Bin) ->
    read_document_count(Bin);

decode_param(uncollapsed_matches_estimated, Bin) ->
    read_document_count(Bin);

decode_param(uncollapsed_matches_upper_bound, Bin) ->
    read_document_count(Bin);

decode_param(size, Bin) ->
    read_document_count(Bin);

decode_param(max_possible, Bin) ->
    read_weight(Bin);

decode_param(max_attained, Bin) ->
    read_weight(Bin);

decode_param(term_weight, Bin) ->
    read_weight(Bin);

decode_param(term_freq, Bin) ->
    read_document_count(Bin).
