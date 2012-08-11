-module(xapian_parse_string).
-export([encode/4
        ,decode/3
        ]).

-import(xapian_common, [ 
    append_stop/1,
    append_param/2,
    read_string/1,
    read_resource/2]).

-import(xapian_const, [ 
    parse_string_field_id/1]).

-compile({parse_transform, seqbind}).


%% #x_query_string{} = QS
encode(QS, RA, Fields = [_|_], Bin@) ->
    Bin@ = xapian_query:append_query_string(QS, RA, Bin@),
    Bin@ = lists:foldl(fun append_field/2, Bin@, Fields),
    Bin@ = append_stop(Bin@),
    Bin@;
encode(QS, RA, Field, Bin) ->
    encode(QS, RA, [Field], Bin).


append_field(Field, Bin) ->
    append_param(parse_string_field_id(Field), Bin).


decode(Fields = [_|_], RR, Bin) ->
    decode(Fields, RR, Bin, []);
decode(Field, RR, Bin) ->
    {{Pairs, RR2}, Bin2} = decode([Field], RR, Bin, []),
    [{Field, Value}] = Pairs,
    {{Value, RR2}, Bin2}.

%% P is a pair. 
decode([query_resource = H | T], RR, Bin, Ps) ->
    {ResRef, RR2, Bin2} = read_resource(RR, Bin),
    P = {H, ResRef},
    decode(T, RR2, Bin2, [P|Ps]);

decode([corrected_query_string = H | T], RR, Bin, Ps) ->
    {Str, Bin2} = read_string(Bin),
    Value = case Str of <<>> -> same; _ -> Str end,
    P = {H, Value},
    decode(T, RR, Bin2, [P|Ps]);

decode([], RR, Bin, Ps) ->
   {{lists:reverse(Ps), RR}, Bin}.
