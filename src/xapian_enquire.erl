-module(xapian_enquire).
-export([encode/5]).
-compile({parse_transform, seqbind}).

-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").

-import(xapian_common, [ 
    append_resource/3,
    append_uint/2,
    append_slot/3,
    append_boolean/2,
    append_percent/2,
    append_weight/2]).

-import(xapian_const, [
    sort_order_value_type/1,
    order_type_id/1,
    docid_order_type_id/1,
    enquire_command_id/1]).


%% RA is a resource appender.
%% N2S is a name to slot.
%% S2T is a slot to type.
encode(Enquire=#x_enquire{}, N2S, S2T, RA, Bin@) ->
    #x_enquire{
        value = Query,
        query_len = QueryLen,
        order = Order,
        docid_order = DocidOrder,
        weighting_scheme = Weight,
        percent_cutoff = PercentCutoff,
        weight_cutoff = WeightCuttoff,
        collapse_key = CollapseKey,
        collapse_max = CollapseMax
    } = Enquire,
    Bin@ = append_query_len(QueryLen, Bin@),
    Bin@ = append_query(Query, N2S, S2T, RA, Bin@),
    Bin@ = append_order(Order, N2S, RA, Bin@),
    Bin@ = append_docid_order(DocidOrder, Bin@),
    Bin@ = append_weighting_scheme(Weight, RA, Bin@),
    Bin@ = append_cutoff(PercentCutoff, WeightCuttoff, Bin@),
    Bin@ = append_collapse_key(CollapseKey, CollapseMax, N2S, Bin@),
    Bin@ = append_command(stop, Bin@),
    Bin@;

encode(Query, N2S, S2T, RA, Bin@) ->
    Bin@ = append_query(Query, N2S, S2T, RA, Bin@),
    Bin@ = append_command(stop, Bin@),
    Bin@.



append_query_len(QueryLen, Bin@) ->
    Bin@ = append_command(query_len, Bin@),
    Bin@ = append_uint(QueryLen, Bin@),
    Bin@.


append_query(Query,  N2S, S2T, RA, Bin@) ->
    Bin@ = append_command(x_query, Bin@),
    Bin@ = xapian_query:encode(Query, N2S, S2T, RA, Bin@),
    Bin@.

    
append_order(relevance, _N2S, _RA, Bin@) ->
    Bin@;

append_order(#x_sort_order{type=relevance, is_reversed=false}, 
             _N2S, _RA, Bin@) ->
    Bin@;

append_order(#x_sort_order{type=relevance, is_reversed=true}, 
             _N2S, _RA, _Bin) ->
    erlang:error(badarg);

append_order(#x_sort_order{type=Type, value=Value, is_reversed=Reverse}, 
    N2S, RA, Bin@) ->
    Bin@ = append_command(order, Bin@),
    Bin@ = append_uint8(order_type_id(Type), Bin@),
    Bin@ = append_boolean(Reverse, Bin@),
    Bin@ = append_value(sort_order_value_type(Type), Value, N2S, RA, Bin@),
    Bin@.


append_value(key, Res, _N2S, RA, Bin) ->
    %% KeyMaker
    append_resource(RA, Res, Bin);

append_value(value, Value, N2S, _RA, Bin) ->
    append_slot(Value, N2S, Bin).


append_docid_order(DocidOrder, Bin) ->
    DefOrderId = docid_order_type_id(default),
    DocidOrderId = docid_order_type_id(DocidOrder),
    append_docid_order_id(DocidOrderId, DefOrderId, Bin).


append_docid_order_id(DefOrderId, DefOrderId, Bin) ->
    Bin;

append_docid_order_id(DocidOrderId, _DefOrderId, Bin@) ->
    Bin@ = append_command(docid_order, Bin@),
    Bin@ = append_uint8(DocidOrderId, Bin@),
    Bin@.


append_weighting_scheme(undefined, _RA, Bin) ->
    Bin;

append_weighting_scheme(Res, RA, Bin@) ->
    Bin@ = append_command(weighting_scheme, Bin@),
    %% Weight
    Bin@ = append_resource(RA, Res, Bin@),
    Bin@.


append_cutoff(0, 0, Bin) ->
    Bin;

append_cutoff(PercentCutoff, WeightCutoff, Bin@) ->
    Bin@ = append_command(cutoff, Bin@),
    Bin@ = append_percent(PercentCutoff, Bin@),
    Bin@ = append_weight(WeightCutoff, Bin@),
    Bin@.


%% `CollapseMax' is meaningful only when `CollapseKey' is defined.
append_collapse_key(undefined, _CollapseMax, _N2S, Bin) ->
    Bin;

append_collapse_key(CollapseKey, CollapseMax, N2S, Bin@) ->
    Bin@ = append_command(collapse_key, Bin@),
    Bin@ = append_slot(CollapseKey, N2S, Bin@),
    Bin@ = append_uint(CollapseMax, Bin@),
    Bin@.


append_command(Type, Bin) ->
    append_uint8(enquire_command_id(Type), Bin).


append_uint8(Value, Bin) ->
    <<Bin/binary, Value:8/native-unsigned-integer>>.
