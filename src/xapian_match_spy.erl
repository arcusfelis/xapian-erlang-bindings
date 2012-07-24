-module(xapian_match_spy).
-export([value_count/2]).

-spec value_count(Server, Slot) -> Spy
    when Server :: xapian_type:x_server(),
         Slot :: xapian_type:x_slot_value(),
         Spy :: xapian_type:x_resource().

%% Create Xapian::ValueCountMatchSpy Object as a resource
value_count(Server, Slot) ->
    GenFn = 
        fun(State) ->
            SlotNo = xapian_server:name_to_slot(State, Slot),
            {ok, xapian_common:append_slot(SlotNo, <<>>)}
        end,
    xapian_server:internal_create_resource(Server, value_count_match_spy, GenFn).
