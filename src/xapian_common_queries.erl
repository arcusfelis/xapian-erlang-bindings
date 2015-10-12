-module(xapian_common_queries).
-export([all_docids/2,
         all_docids_with_term/2,
         all_docid_percents/2]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("xapian/include/xapian.hrl").
-record(document, {docid}).
-record(document_percent, {docid, percent}).

mset_table(Server, Query, document) ->
    Meta = xapian_record:record(document, record_info(fields, document)),
    mset_table(Server, Query, Meta);

mset_table(Server, Query, document_percent) ->
    Meta = xapian_record:record(document_percent, record_info(fields, document_percent)),
    mset_table(Server, Query, Meta);

mset_table(Server, Query, Meta) ->
    EnquireResourceId = xapian_server:enquire(Server, Query),
    MSetResourceId = xapian_server:match_set(Server, EnquireResourceId),
    Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),
    %% Table has a pointer on resources.
    xapian_server:release_resource(Server, EnquireResourceId),
    xapian_server:release_resource(Server, MSetResourceId),
    Table.

all_docids(Server, Query) ->
    Table = mset_table(Server, Query, document),
    Ids = qlc:e(qlc:q([Id || #document{docid=Id} <- Table])),
    Ids.

all_docids_with_term(Server, Term) ->
    Query = #x_query_term{name = Term},
    all_docids(Server, Query).

%% @doc Ask for docids and percent (shows relevance)
all_docid_percents(Server, Query) ->
    Table = mset_table(Server, Query, document_percent),
    IdsPercents = qlc:e(qlc:q([{Id, P} || #document_percent{docid=Id, percent=P} <- Table])),
    IdsPercents.
