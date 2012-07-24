-module(xapian_resource_info).
-export([encode/3,
         decode/3]).
-import(xapian_common, [
        append_uint8/2, 
        read_uint/1]).

-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").

-type decoder() :: fun((atom(), binary()) -> {term(), binary()}).

-spec encode(ResDesc, Params, Bin) -> Bin2 when
    ResDesc :: #resource{},
    Params :: [Field] | Field,
    Field :: atom(),
    Bin :: binary(),
    Bin2 :: binary().

encode(#resource{type=ResType}, [_|_] = Params, Bin) ->
    Encoder = select_encoder(ResType),
    lists:foldl(Encoder, Bin, Params);

encode(#resource{type=ResType}, Param, Bin) ->
    Encoder = select_encoder(ResType),
    Encoder(Param, Bin).


-spec select_encoder(Field) -> Fn when
    Fn :: fun((Field, Bin) -> Bin2),
    Field :: atom(),
    Bin :: binary(),
    Bin2 :: binary().

select_encoder(match_spy) ->
    fun enc_match_spy/2;
select_encoder(_) ->
    fun enc_common/2.


enc_match_spy(Field, Bin) ->
    Fn = fun xapian_const:resource_info_match_spy_field_id/1,
    enc_field(Fn, Field, Bin).
    
enc_common(_, Bin) -> Bin.


%% Append a field, using the `Fn' hof.
-spec enc_field(Fn, Field, Bin) -> Bin2 when
    Fn :: fun((Field) -> FieldId),
    Field :: atom(),
    FieldId :: 0..255,
    Bin :: binary(),
    Bin2 :: binary().

enc_field(Fn, Field, Bin) ->
    case Fn(Field) of
        undefined -> enc_common(Field, Bin);
        FieldId -> append_uint8(FieldId, Bin)
    end.



-spec decode(ResDesc, Params, Bin) -> Result when
    ResDesc :: #resource{},
    Params :: [Field] | Field,
    Field :: atom(),
    Bin :: binary(),
    Result :: {Pairs | Value, Bin},
    Pairs :: [{Field, Value}],
    Value :: term().

decode(ResDesc = #resource{}, [_|_] = Params, Bin) ->
    Decoder1 = generate_decoder(ResDesc),
    Decoder2 = foldlrise_decoder(Decoder1), 
    {Pairs, RemBin} = lists:foldl(Decoder2, {[], Bin}, Params),
    {lists:reverse(Pairs), RemBin};

decode(ResDesc = #resource{}, Param, Bin) ->
    %% Decode one field only.
    Decoder = generate_decoder(ResDesc),
    Decoder(Param, Bin).
    %% Return `{Value, RemBin}'.


%% @doc Add an adapter for `Decoder' (of the `decoder()' type).
%% It allows to use `Decoder' inside `lists:foldl/3'.
-spec foldlrise_decoder(Decoder) -> FoldlDecoder when
    Decoder      :: fun((Param, Bin) -> {Value, BinRem}),
    FoldlDecoder :: fun((Param, {Pairs, Bin}) -> {NewPairs, BinRem}),
    Param :: atom(),
    Value :: term(),
    Bin :: binary(), 
    BinRem :: binary(),
    NewPairs :: Pairs,
    Pairs :: [{Param, Value}].

foldlrise_decoder(Decoder) ->
    fun(Param, {Pairs, Bin}) ->
        {Value, BinRem} = Decoder(Param, Bin),
        NewPairs = [{Param, Value} | Pairs],
        {NewPairs, BinRem}
    end.


%% @doc Generate a HOF.
-spec generate_decoder(ResDesc) -> Decoder when
    ResDesc   :: #resource{},
    Decoder   :: decoder().

generate_decoder(ResDesc = #resource{type = ResType}) ->
    Otherwise = select_decoder(ResType),
    spice_common_decoder(ResDesc, Otherwise).


%% Add a common decoder before a type specific decoder (Otherwise).
-spec spice_common_decoder(ResDesc, Otherwise) -> Decoder when
    ResDesc   :: #resource{},
    Otherwise :: decoder(),
    Decoder   :: decoder(). 

spice_common_decoder(ResDesc = #resource{}, Otherwise) ->
    fun(Param, Bin) ->
            %% Decode common fields.
            case Param of
                type   -> {ResDesc#resource.type,   Bin};
                number -> {ResDesc#resource.number, Bin};
                %% Decode type-specific fields
                _ -> Otherwise(Param, Bin)
            end
        end.


-spec select_decoder(ResType) -> Decoder when
    Decoder   :: decoder(),
    ResType   :: atom().

select_decoder(match_spy) -> fun dec_match_spy/2;
select_decoder(_)         -> fun dec_bad_field/2.


%% @doc Show an error. 
%% Common fields are allowed only.
dec_bad_field(_Field, _Bin) -> erlang:error(bad_field).


%% @doc Decode information about a match_spy resource.
dec_match_spy(document_count, Bin) ->
    read_uint(Bin);
dec_match_spy(slot, Bin) ->
    read_uint(Bin).
