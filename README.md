Xapian binding for Erlang
=========================

__License__: MIT, GPL2 or higher (Xapian is still under GPL only.)

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))


[Xapian](http://xapian.org/) is an Open Source Search Engine Library, 
written in C++.
Xapian is a highly adaptable toolkit which allows developers to easily add 
advanced indexing and search facilities to their own applications.


Installation
============

I use rebar for building.

```bash
git clone git://github.com/freeakk/xapian.git
cd xapian
./rebar get-deps
./rebar compile
```

I use [google sparse hash](http://code.google.com/p/sparsehash/?redir=1) 
for storing resources' ids.

Here is a package name of this library in the Ubuntu repository
`libsparsehash-dev`.


Tests
=====

Next command runs tests:

```bash
./rebar eunit skip_deps=true
```


A pool of readers
=================

```erlang
Path = filename:join([code:priv_dir(xapian), test_db, simple]).
{ok, Pid} = xapian_pool:open([{name, simple}], Path, []).
result = xapian_pool:checkout([simple], fun([Server]) -> io:write(Server), result end).
```

Readers use the Poolboy application. 
There is only one writer for each database, so there is no a writer pool.
You can use a named process and a supervisor instead:

```erlang
{ok, Pid} = xapian_drv:open(Path, [{name, simple_writer}, write]).
xapian_drv:add_document(simple_writer, [#x_text{value = "Paragraph 1"}]).
```

If you try run this code from console, then next command will be useful:

```erlang
rr(code:lib_dir(xapian, include) ++ "/xapian.hrl").
```

It loads information about records into console.


A pool is supervised by `xapian\_sup`. That is why, call of 
`xapian\_pool:open` function do *not* link the parent process with the new 
process. 

As with `xapian\_drv:transaction`, you can checkout few pools.

```erlang
xapian_pool:checkout([pool1, poo2], fun([Server1, Server2]) -> actions_here end).
```
 
If an error will occured, an exception will be thrown and workers 
will be returned into the pool.

```erlang
catch xapian_pool:checkout([simple], fun([S]) -> 5 = 2 + 2 end).
{'EXIT',{{badmatch,4},[{erl_eval,expr,3,[]}]}}
```


Multi-database support
======================


You can use this code for opening two databases from "DB1" and "DB2" directories.

```erlang
{ok, Server} = xapian_driver:open(
    [#x_database{path="DB1"}, #x_database{path="DB2"}], []).
```

Only read-only databases can be used.

There are two fields meaning a document's id: `docid` and `multi\_docid`.
They are equal if only one database is used.

Otherwise, first field contains a document id (can be repeated) and 
`multi\_docid` is a unique idintifier, which is calculated from 
`docid` and `db\_number`.

`db\_number` is a number of the document's database encounted from 1.

`db\_name` field contains pseudonyms of the databases.
Information from `name` field of `#x\_database{}` record will be used for 
this. This field is `undefined` by default.

```erlang
 #x_database{name=db1, path="DB1"}
```

Full multi-database example:

```erlang
-record(document, {docid, db_name, multi_docid, db_number}).

example() ->
    DB1 = #x_database{name=db1, path="DB1"}, 
    DB2 = #x_database{name=db1, path="DB2"},
    {ok, Server} = xapian_driver:open([DB1, DB2], []),
    EnquireResourceId = xapian_driver:enquire(Server, "query string"),
    MSetResourceId = xapian_driver:match_set(Server, EnquireResourceId),
    Meta = xapian_record:record(document, record_info(fields, document)),
    Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta),
    qlc:e(qlc:q([X || #document{multi_docid=DocId} <- Table])).
```
