-module(xapian_resource).


%% Weight
-export([
    bool_weight/1, 
    bm25_weight/2, 
    bm25_weight/6, 
    trad_weight/1, 
    trad_weight/2]).

%% KeyMaker
-export([
    multi_value_key_maker/2]).

%% ValueRangeProcessor
-export([
    string_value_range_processor/2,
    string_value_range_processor/4,
    number_value_range_processor/2,
    number_value_range_processor/4,
    date_value_range_processor/4,
    date_value_range_processor/6
    ]).

-import(xapian_common, [
        append_double/2, 
        append_string/2, 
        append_boolean/2, 
        append_uint/2,
        append_int/2]).

-compile({parse_transform, seqbind}).

-include_lib("xapian/include/xapian.hrl").

-ifdef(TEST).
-import(xapian_helper, [testdb_path/1]).
-include_lib("eunit/include/eunit.hrl").
-define(SRV, xapian_server).
-define(RES, ?MODULE).
-endif.

-type x_server()        :: xapian_type:x_server().
-type x_resource()      :: xapian_type:x_resource().


bool_weight(Server) ->
    xapian_server:internal_create_resource(Server, bool_weight).


bm25_weight(Server, 
    #x_bm25_weight{k1=K1, k2=K2, l3=K3, b=B, min_normlen=MinNormLen}) ->
    bm25_weight(Server, K1, K2, K3, B, MinNormLen).

bm25_weight(Server, K1, K2, K3, B, MinNormLen) ->
    GenFn = 
        fun() ->
            Bin@ = <<>>,
            Bin@ = append_double(K1, Bin@),
            Bin@ = append_double(K2, Bin@),
            Bin@ = append_double(K3, Bin@),
            Bin@ = append_double(B, Bin@),
            Bin@ = append_double(MinNormLen, Bin@),
            {ok, Bin@}
        end,
    xapian_server:internal_create_resource(Server, bm25_weight, GenFn).
    

trad_weight(Server) ->
    trad_weight(Server, 1).

trad_weight(Server, K) ->
    GenFn = 
        fun() ->
            {ok, append_double(K, <<>>)}
        end,
    xapian_server:internal_create_resource(Server, trad_weight, GenFn).


-spec multi_value_key_maker(x_server(), Slots) -> x_resource() when
    Slots :: [xapian_type:x_slot_value() | ValueWithReversedOrder],
    ValueWithReversedOrder :: {reverse, xapian_type:x_slot_value()}.

multi_value_key_maker(Server, Slots) ->
    GenFn = 
        fun(State) ->
            N2S = xapian_server:name_to_slot(State),
            {ok, xapian_common:append_slots_with_order(N2S, Slots, <<>>)}
        end,
    xapian_server:internal_create_resource(Server, multi_value_key_maker, GenFn).


-spec date_value_range_processor(x_server(), Slot, EpochYear, PreferMDY) -> 
    x_resource() when
    Slot      :: xapian_type:x_slot_value(),
    EpochYear :: integer(),
    PreferMDY :: boolean().

date_value_range_processor(Server, Slot, EpochYear, PreferMDY) ->
    GenFn = 
        fun(State) ->
            SlotNum = xapian_server:name_to_slot(State, Slot),
            Bin@ = append_uint(SlotNum, <<>>),
            Bin@ = append_int(EpochYear, Bin@),
            Bin@ = append_boolean(PreferMDY, Bin@),
            {ok, Bin@}
        end,
    xapian_server:internal_create_resource(Server, 
                                           date_value_range_processor3, GenFn).


-spec date_value_range_processor(x_server(), Slot, Str, Prefix,
                                 EpochYear, PreferMDY) -> 
    x_resource() when
    Slot      :: xapian_type:x_slot_value(),
    Str       :: xapian_type:x_string(),
    EpochYear :: integer(),
    PreferMDY :: boolean(),
    Prefix    :: boolean() | prefix | suffix.

date_value_range_processor(Server, Slot, Str, Prefix, EpochYear, PreferMDY) ->
    GenFn1 = value_range_processor_gen(Slot, Str, Prefix),
    GenFn = 
        fun(State) ->
            {ok, Bin@} = GenFn1(State),
            Bin@ = append_int(EpochYear, Bin@),
            Bin@ = append_boolean(PreferMDY, Bin@),
            {ok, Bin@}
        end,
    xapian_server:internal_create_resource(Server, 
                                           date_value_range_processor5, GenFn).


-spec number_value_range_processor(x_server(), Slot, Str, Prefix) ->
    x_resource() when
    Slot      :: xapian_type:x_slot_value(),
    Str       :: xapian_type:x_string(),
    Prefix    :: boolean() | prefix | suffix.

number_value_range_processor(Server, Slot, Str, Prefix) ->
    GenFn = value_range_processor_gen(Slot, Str, Prefix),
    xapian_server:internal_create_resource(Server, 
                                           number_value_range_processor3, GenFn).


-spec number_value_range_processor(x_server(), Slot) -> x_resource() when
    Slot      :: xapian_type:x_slot_value().

number_value_range_processor(Server, Slot) ->
    GenFn = slot_gen(Slot),
    xapian_server:internal_create_resource(Server, 
                                           number_value_range_processor1, GenFn).


-spec string_value_range_processor(x_server(), Slot, Str, Prefix) ->
    x_resource() when
    Slot      :: xapian_type:x_slot_value(),
    Str       :: xapian_type:x_string(),
    Prefix    :: boolean() | prefix | suffix.

string_value_range_processor(Server, Slot, Str, Prefix) ->
    GenFn = value_range_processor_gen(Slot, Str, Prefix),
    xapian_server:internal_create_resource(Server, 
                                           string_value_range_processor3, GenFn).


-spec string_value_range_processor(x_server(), Slot) -> x_resource() when
    Slot      :: xapian_type:x_slot_value().

string_value_range_processor(Server, Slot) ->
    GenFn = slot_gen(Slot),
    xapian_server:internal_create_resource(Server, 
                                           string_value_range_processor1, GenFn).


value_range_processor_gen(Slot, Str, Prefix) ->
    fun(State) ->
        SlotNum = xapian_server:name_to_slot(State, Slot),
        Bin@ = append_uint(SlotNum, <<>>),
        Bin@ = append_string(Str, Bin@),
        Bin@ = append_boolean(handle_prefix_atom(Prefix), Bin@),
        {ok, Bin@}
    end.

slot_gen(Slot) ->
    fun(State) -> 
        SlotNum = xapian_server:name_to_slot(State, Slot),
        {ok, append_uint(SlotNum, <<>>)} 
    end.

handle_prefix_atom(prefix) -> true;
handle_prefix_atom(suffix) -> false.


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

cases_test_() ->
    Cases = 
    [ fun bool_weight_case/1
    , fun bm25_weight_case/1
    , fun trad_weight_case/1
    ],
    Server = resource_setup(),
    %% One setup for each test
    {setup, 
        fun() -> Server end, 
        fun resource_clean/1,
        [Case(Server) || Case <- Cases]}.


resource_setup() ->
    % Open test
    Path = testdb_path(resource_creation),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:start_link(Path, Params),
    Server.
    

resource_clean(Server) ->
%   ?SRV:close(Server).
    ok.


bool_weight_case(Server) ->
    Case = fun() ->
        ResourceId = ?RES:bool_weight(Server),
        io:format(user, "Xapian::BoolWeight ~p~n", [ResourceId])
        end,
    {"Check creation of Xapian::BoolWeight", Case}.


bm25_weight_case(Server) ->
    Case = fun() ->
        ResourceId = ?RES:bm25_weight(Server, #x_bm25_weight{}),
        io:format(user, "Xapian::BM25Weight ~p~n", [ResourceId])
        end,
    {"Check creation of Xapian::BM25Weight", Case}.


trad_weight_case(Server) ->
    Case = fun() ->
        ResourceId = ?RES:trad_weight(Server),
        io:format(user, "Xapian::TradWeight ~p~n", [ResourceId])
        end,
    {"Check creation of Xapian::TradWeight", Case}.

-endif.
