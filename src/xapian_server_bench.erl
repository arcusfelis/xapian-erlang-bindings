-module(xapian_server_bench).
-export([simple_search/2]).

-ifdef(BENCHMARK).
-import(xapian_helper, [testdb_path/1]).
-define(EMARK_NOAUTO, true).
-include_lib("emark/include/emark.hrl").
-endif.

-define(SRV, xapian_server).

-include_lib("xapian/include/xapian.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(document, {docid}).

simple_search(Server, Query) ->
    EnquireResourceId = ?SRV:enquire(Server, Query),                             
    MSetResourceId = ?SRV:match_set(Server, EnquireResourceId),                  
    ?SRV:release_resource(Server, EnquireResourceId),                               
                                                                                 
    %% Meta is a record, which contains some information about                   
    %% structure of a document record.                                           
    %% The definition of Meta is incapsulated inside `xapian_record' module.     
    Meta = xapian_record:record(document, record_info(fields, document)),                
                                                                                 
    %% Create QlcTable from MSet.                                                
    %% After creation of QlcTable, MSet can be removed.                          
    Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),                 
    ?SRV:release_resource(Server, MSetResourceId),                               
                                                                                 
    %% QueryAll is a list of all matched records.                                
    QueryAll = qlc:q([X || X <- Table]),
    Records = qlc:e(QueryAll),
    ?SRV:release_table(Server, Table),
    Records.


-ifdef(BENCHMARK).

add_empty_document_benchmark(N) ->
    Path = testdb_path(add_empty_doc_bm),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:open(Path, Params),
    emark:start(?SRV, add_document, 2),
    [ ?SRV:add_document(Server, []) || _ <- lists:seq(1, N) ],
%   ?SRV:close(Server),
    ok.


add_nonempty_document_benchmark(N) ->
    Path = testdb_path(add_doc_bm),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:open(Path, Params),
    emark:start(?SRV, add_document, 2),
    [ ?SRV:add_document(Server, [#x_text{value="haskell erlang scala"}]) 
        || _ <- lists:seq(1, N) ],
%   ?SRV:close(Server),
    ok.


simple_query_benchmark(N) ->
    Path = testdb_path(simple_query_bm),
    Params = [write, create, overwrite],
    {ok, Server} = ?SRV:open(Path, Params),
    %% Add 1000 documents.
    [ ?SRV:add_document(Server, [#x_term{value=integer_to_list(X)}]) 
        || X <- lists:seq(1, 1000) ],

    emark:start({ ?MODULE, simple_search, 2 }),

    [ ?MODULE:simple_search(Server, integer_to_list(random:uniform(1000))) 
        || _ <- lists:seq(1, N) ],
%   ?SRV:close(Server),
    ok.


-endif.
