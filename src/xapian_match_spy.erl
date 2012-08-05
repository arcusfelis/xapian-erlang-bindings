-module(xapian_match_spy).
-export([value_count/2]).

-spec value_count(Server, Slot) -> Spy
    when Server :: xapian_type:x_server(),
         Slot :: xapian_type:x_slot_value(),
         Spy :: xapian_type:x_resource().

%% Create Xapian::ValueCountMatchSpy Object as a resource
value_count(Server, Slot) ->
    Con = xapian_resource:value_count_match_spy(Slot),
    xapian_server:create_resource(Server, Con).
