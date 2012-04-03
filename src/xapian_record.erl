%% It contains helpers for extracting.
-module(xapian_record).
-export([]).

%% Creates tuples {Name, Field1, ....}
encode(Name, Fields, Name2Slot) ->
    Slots = [ orddict:fetch(Field, Name2Slot) || Field <- Fields ].


    
