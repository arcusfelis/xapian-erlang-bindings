-module(xapian_server_bench).

-include_lib("emark/include/emark.hrl").
-include_lib("xapian/include/xapian.hrl").


-ifdef(BENCHMARK).
-define(SRV, xapian_server).

testdb_path(Name) -> 
    io:format(user, "~nTest DB: ~s~n ", [Name]),
	TestDir = filename:join(code:priv_dir(xapian), test_db),
	file:make_dir(TestDir),
	filename:join(TestDir, Name).


add_empty_document_benchmark(N) ->
    Path = testdb_path(add_empty_doc_bm),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:open(Path, Params),
    emark:start({ ?SRV, add_document, 2 }),
    [ ?SRV:add_document(Server, []) || _ <- lists:seq(1, N) ],
    ?SRV:close(Server),
    ok.


add_nonempty_document_benchmark(N) ->
    Path = testdb_path(add_doc_bm),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:open(Path, Params),
    emark:start({ ?SRV, add_document, 2 }),
    [ ?SRV:add_document(Server, [#x_text{value="haskell erlang scala"}]) 
        || _ <- lists:seq(1, N) ],
    ?SRV:close(Server),
    ok.

-endif.
