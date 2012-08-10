-module(xapian_resource).

%% Weight
-export([
    bool_weight/0, 
    bm25_weight/1, 
    bm25_weight/5, 
    trad_weight/0, 
    trad_weight/1]).

%% KeyMaker
-export([
    multi_value_key_maker/1]).

%% ValueRangeProcessor
-export([
    string_value_range_processor/1,
    string_value_range_processor/3,
    number_value_range_processor/1,
    number_value_range_processor/3,
    date_value_range_processor/3,
    date_value_range_processor/5
    ]).

%% MatchSpy
-export([
    value_count_match_spy/1
    ]).

-import(xapian_common, [
        append_double/2, 
        append_string/2, 
        append_boolean/2, 
        append_uint/2,
        append_int/2]).

%% internal
-export([
    create/2,
    compile/3]).

-compile({parse_transform, seqbind}).

-include_lib("xapian/include/xapian.hrl").

-ifdef(TEST).
-import(xapian_helper, [testdb_path/1]).
-include_lib("eunit/include/eunit.hrl").
-define(SRV, xapian_server).
-define(RES, ?MODULE).
-endif.

-type x_resource_con()      :: xapian_type:x_resource_con().
-record(resourse_const, {name, generator}).

con(Name, Gen) ->
    #resourse_const{name = Name, generator = Gen}.

con(Name) ->
    #resourse_const{name = Name}.

%% @doc Append the second parameter as a binary.
%% @see xapian_server:compile_resource/3
compile(State, #resourse_const{name = ConName, generator = Gen}, Bin) ->
    xapian_server:execute_generator(State, ConName, Gen, Bin).

create(Server, #resourse_const{name = ConName, generator = Gen}) ->
    xapian_server:internal_create_resource(Server, ConName, Gen).

bool_weight() ->
    con(bool_weight).


bm25_weight(#x_bm25_weight{k1=K1, k2=K2, l3=K3, b=B, min_normlen=MinNormLen}) ->
    bm25_weight(K1, K2, K3, B, MinNormLen).

bm25_weight(K1, K2, K3, B, MinNormLen) ->
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
    con(bm25_weight, GenFn).
    

trad_weight() ->
    trad_weight(1).

trad_weight(K) ->
    GenFn = 
        fun() ->
            {ok, append_double(K, <<>>)}
        end,
    con(trad_weight, GenFn).


-spec multi_value_key_maker(Slots) -> x_resource_con() when
    Slots :: [xapian_type:x_slot_value() | ValueWithReversedOrder],
    ValueWithReversedOrder :: {reverse, xapian_type:x_slot_value()}.

multi_value_key_maker(Slots) ->
    GenFn = 
        fun(State) ->
            N2S = xapian_server:name_to_slot(State),
            {ok, xapian_common:append_slots_with_order(N2S, Slots, <<>>)}
        end,
    con(multi_value_key_maker, GenFn).


-spec date_value_range_processor(Slot, EpochYear, PreferMDY) -> 
    x_resource_con() when
    Slot      :: xapian_type:x_slot_value(),
    EpochYear :: integer(),
    PreferMDY :: boolean().

date_value_range_processor(Slot, EpochYear, PreferMDY) ->
    GenFn = 
        fun(State) ->
            SlotNum = xapian_server:name_to_slot(State, Slot),
            Bin@ = append_uint(SlotNum, <<>>),
            Bin@ = append_int(EpochYear, Bin@),
            Bin@ = append_boolean(PreferMDY, Bin@),
            {ok, Bin@}
        end,
    con(date_value_range_processor3, GenFn).


-spec date_value_range_processor(Slot, Str, Prefix,
                                 EpochYear, PreferMDY) -> 
    x_resource_con() when
    Slot      :: xapian_type:x_slot_value(),
    Str       :: xapian_type:x_string(),
    EpochYear :: integer(),
    PreferMDY :: boolean(),
    Prefix    :: boolean() | prefix | suffix.

date_value_range_processor(Slot, Str, Prefix, EpochYear, PreferMDY) ->
    GenFn1 = value_range_processor_gen(Slot, Str, Prefix),
    GenFn = 
        fun(State) ->
            {ok, Bin@} = GenFn1(State),
            Bin@ = append_int(EpochYear, Bin@),
            Bin@ = append_boolean(PreferMDY, Bin@),
            {ok, Bin@}
        end,
    con(date_value_range_processor5, GenFn).


-spec number_value_range_processor(Slot, Str, Prefix) ->
    x_resource_con() when
    Slot      :: xapian_type:x_slot_value(),
    Str       :: xapian_type:x_string(),
    Prefix    :: boolean() | prefix | suffix.

number_value_range_processor(Slot, Str, Prefix) ->
    GenFn = value_range_processor_gen(Slot, Str, Prefix),
    con(number_value_range_processor3, GenFn).


-spec number_value_range_processor(Slot) -> x_resource_con() when
    Slot      :: xapian_type:x_slot_value().

number_value_range_processor(Slot) ->
    GenFn = slot_gen(Slot),
    con(number_value_range_processor1, GenFn).


-spec string_value_range_processor(Slot, Str, Prefix) ->
    x_resource_con() when
    Slot      :: xapian_type:x_slot_value(),
    Str       :: xapian_type:x_string(),
    Prefix    :: boolean() | prefix | suffix.

string_value_range_processor(Slot, Str, Prefix) ->
    GenFn = value_range_processor_gen(Slot, Str, Prefix),
    con(string_value_range_processor3, GenFn).


-spec string_value_range_processor(Slot) -> x_resource_con() when
    Slot      :: xapian_type:x_slot_value().

string_value_range_processor(Slot) ->
    GenFn = slot_gen(Slot),
    con(string_value_range_processor1, GenFn).


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


-spec value_count_match_spy(Slot) -> Spy
    when Slot :: xapian_type:x_slot_value(),
          Spy :: xapian_type:x_resource_con().

%% Create Xapian::ValueCountMatchSpy Object as a resource
value_count_match_spy(Slot) ->
    GenFn = 
        fun(State) ->
            SlotNo = xapian_server:name_to_slot(State, Slot),
            {ok, xapian_common:append_slot(SlotNo, <<>>)}
        end,
    con(value_count_match_spy, GenFn).

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
            ResourceId = ?SRV:create_resource(Server, ?RES:bool_weight()),
        io:format(user, "Xapian::BoolWeight ~p~n", [ResourceId])
        end,
    {"Check creation of Xapian::BoolWeight", Case}.


bm25_weight_case(Server) ->
    Case = fun() ->
        ResourceId = ?SRV:create_resource(Server, ?RES:bm25_weight(#x_bm25_weight{})),
        io:format(user, "Xapian::BM25Weight ~p~n", [ResourceId])
        end,
    {"Check creation of Xapian::BM25Weight", Case}.


trad_weight_case(Server) ->
    Case = fun() ->
        ResourceId = ?SRV:create_resource(Server, ?RES:trad_weight()),
        io:format(user, "Xapian::TradWeight ~p~n", [ResourceId])
        end,
    {"Check creation of Xapian::TradWeight", Case}.

-endif.
