%% @doc Usuful miniprograms.
-module(xapian_helper).
-export([stem/3]).

-include_lib("xapian/include/xapian.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(term, {value, positions, wdf}).


stem(Server, Lang, Str) ->
    CD = [ #x_stemmer{language = Lang}, #x_text{value = Str} ],
    RD = xapian_server:document(Server, CD),
    Meta = xapian_term_record:record(term, record_info(fields, term)),
    Table = xapian_term_qlc:document_term_table(Server, RD, Meta),
    UnsortedQH = qlc:q([#x_term{value = V, position = P, frequency = F}
                       || #term{value = V, positions = P, wdf = F} 
                       <- Table]),
    SortedQH = qlc:keysort(#term.positions, UnsortedQH, []),
    qlc:e(SortedQH).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(HELP, ?MODULE).

stem_test_() ->
    {ok, Server} = xapian_server:open([], []),
    [?_assertEqual(?HELP:stem(Server, "none", "erlang and xapian"),
                  [ #x_term{frequency = 1, position = [1], value = <<"erlang">>}
                  , #x_term{frequency = 1, position = [2], value = <<"and">>}
                  , #x_term{frequency = 1, position = [3], value = <<"xapian">>}
                  ])].
-endif.
