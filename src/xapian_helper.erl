%% @doc Useful miniprograms, experimental code, helpers for using from tests
%%      and from console.
-module(xapian_helper).
-export([stem/3, stem_qlc/3]).
-export([testdb_path/1]).

-include_lib("xapian/include/xapian.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(term, {value, positions, wdf}).

-spec stem(xapian_type:x_server(),
           xapian_type:x_language_code(), 
           xapian_type:x_string()) -> [#x_term{}].
stem(Server, Lang, Str) ->
    qlc:e(stem_qlc(Server, Lang, Str)).


stem_qlc(Server, Lang, Str) ->
    CD = [ #x_stemmer{language = Lang}, #x_text{value = Str} ],
    RD = xapian_server:document(Server, CD),
    Meta = xapian_term_record:record(term, record_info(fields, term)),
    Table = xapian_term_qlc:document_term_table(Server, RD, Meta),
    UnsortedQH = qlc:q([#x_term{value = V, position = P, frequency = F}
                       || #term{value = V, positions = P, wdf = F} 
                       <- Table]),
    qlc:keysort(#term.positions, UnsortedQH, []).


testdb_path(Name) when is_atom(Name) -> 
    %% Hello, dialyzer! 
    %% Atoms are fine, but D says about an error :(
    %% That is why this clause is here.
    testdb_path(atom_to_list(Name));

testdb_path(Name) -> 
    io:format(user, "~nTest DB: ~s~n ", [Name]),
    case code:priv_dir(xapian) of
    {error, Reason} ->
            erlang:error(Reason);
    PrivDir ->
        TestDir = filename:join(PrivDir, "test_db"),
        file:make_dir(TestDir),
        filename:join(TestDir, Name)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(HELP, ?MODULE).

stem_test_() ->
    {ok, Server} = xapian_server:start_link([], []),
    [?_assertEqual(?HELP:stem(Server, "none", "erlang and xapian"),
                  [ #x_term{frequency = 1, position = [1], value = <<"erlang">>}
                  , #x_term{frequency = 1, position = [2], value = <<"and">>}
                  , #x_term{frequency = 1, position = [3], value = <<"xapian">>}
                  ])].
-endif.
