-module(xapian_match_spy).
-export([value_count/2]).

%% Create Xapian::ValueCountMatchSpy Object as a resource
value_count(Server, Slot) ->
    GenFn = 
        fun(State) ->
            SlotNo = xapian_drv:name_to_slot(State, Slot),
            {ok, xapian_common:append_slot(SlotNo, <<>>)}
        end,
    xapian_drv:internal_create_resource(Server, value_count_match_spy, GenFn).
