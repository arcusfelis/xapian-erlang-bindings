-module(xapian_enquire).
-export([encode/4]).
-compile({parse_transform, seqbind}).

-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").

-import(xapian_common, [ 
    append_uint/2,
    append_boolean/2,
    slot_id/2,
    append_double/2]).

-import(xapian_const, [
    order_type_id/1,
    docid_order_type_id/1,
    enquire_command_id/1]).


encode(Enquire=#x_enquire{x_query=Query}, Name2Slot, Register, Bin@) ->
    #x_enquire{
        x_query = Query,
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
    Bin@ = append_query(Query, Name2Slot, Bin@),
    Bin@ = append_order(Order, Name2Slot, Register, Bin@),
    Bin@ = append_docid_order(DocidOrder, Bin@),
    Bin@ = append_weighting_scheme(Weight, Register, Bin@),
    Bin@ = append_cutoff(PercentCutoff, WeightCuttoff, Bin@),
    Bin@ = append_collapse_key(CollapseKey, CollapseMax, Name2Slot, Bin@),
    Bin@ = append_command(stop, Bin@),
    Bin@;

encode(Query, Name2Slot, _Register, Bin@) ->
    Bin@ = append_query(Query, Name2Slot, Bin@),
    Bin@ = append_command(stop, Bin@),
    Bin@.



append_query_len(QueryLen, Bin@) ->
    Bin@ = append_command(query_len, Bin@),
    Bin@ = append_uint(QueryLen, Bin@),
    Bin@.


append_query(Query,  N2S, Bin@) ->
    Bin@ = append_command(x_query, Bin@),
    Bin@ = xapian_query:encode(Query, N2S, Bin@),
    Bin@.

    
append_order(relevance, _N2S, _Register, Bin@) ->
    Bin@;

append_order(#x_sort_order{type=relevance}, _N2S, _Register, Bin@) ->
    Bin@;

append_order(#x_sort_order{type=Type, value=Value, is_reversed=Reverse}, 
    N2S, Register, Bin@) ->
    Bin@ = append_command(query_len, Bin@),
    Bin@ = append_uint8(order_type_id(Type), Bin@),
    Bin@ = append_boolean(Reverse, Bin@),
    Bin@ = append_value(Value, N2S, Register, Bin@),
    Bin@.


append_value(ResourceId, _N2S, Register, Bin) when is_reference(ResourceId) ->
    append_uint(xapian_register:fetch(Register, ResourceId), Bin);

append_value(Value, N2S, _Register, Bin) ->
    append_uint(slot_id(Value, N2S), Bin).


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


append_weighting_scheme(undefined, _Register, Bin) ->
    Bin;

append_weighting_scheme(ResourceId, Register, Bin@) 
    when is_reference(ResourceId) ->
    #resource{type = weight, number = WeightNum} = 
        xapian_register:fetch(Register, ResourceId),
    Bin@ = append_command(weighting_scheme, Bin@),
    Bin@ = append_uint(WeightNum, Bin@),
    Bin@.


append_cutoff(0, 0, Bin) ->
    Bin;

append_cutoff(PercentCutoff, WeightCutoff, Bin@) ->
    Bin@ = append_command(cutoff, Bin@),
    Bin@ = append_percent(PercentCutoff, Bin@),
    Bin@ = append_weight(WeightCutoff, Bin@),
    Bin@.


append_collapse_key(undefined, 1, _N2S, Bin) ->
    Bin;

%% TODO: Is Xapian::BAD_VALUENO 0?
append_collapse_key(undefined, CollapseMax, N2S, Bin) ->
    append_collapse_key(0, CollapseMax, N2S, Bin);

append_collapse_key(CollapseKey, CollapseMax, N2S, Bin@) ->
    Bin@ = append_command(collapse_key, Bin@),
    Bin@ = append_uint(slot_id(CollapseKey, N2S), Bin@),
    Bin@ = append_uint(CollapseMax, Bin@),
    Bin@.


append_command(Type, Bin) ->
    append_uint8(enquire_command_id(Type), Bin).


append_weight(Value, Bin) ->
    append_uint8(Value, Bin).


append_percent(Value, Bin) ->
    append_double(Value, Bin).


append_uint8(Value, Bin) ->
    <<Bin/binary, Value:8/native-unsigned-integer>>.
