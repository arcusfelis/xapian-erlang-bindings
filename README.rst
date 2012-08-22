.. highlight:: erlang

Xapian binding for Erlang
=========================

**License**: MIT, GPL2 or higher (Xapian is still under GPL only.)

**Author**: Uvarov Michael (freeakk@gmail.com)

`Xapian <http://xapian.org/>`_ is an Open Source Search Engine Library,
written in C++. Xapian is a highly adaptable toolkit which allows
developers to easily add advanced indexing and search facilities to
their own applications.

Installation
============

I use rebar for building::
    $ git clone git://github.com/freeakk/xapian.git 
    $ cd xapian ./rebar get-deps ./rebar compile

Google hash map (optional)
--------------------------

You can use `google sparse
hash <http://code.google.com/p/sparsehash/?redir=1>`_ for storing
resources' ids.

Here is a package name of this library in the Ubuntu repository
``libsparsehash-dev``.

The C++-predprocessor's macro ``GOOGLE_HASH_MAP`` enables google hash
map as a hash map.

Using
=====

This application uses records, defined in the file
``include/xapian.hrl``. To include it use::
    -include_lib("xapian/include/xapian.hrl").

Tests
=====

Next command runs tests::
    $ ./rebar eunit skip_deps=true

A pool of readers
=================

.. code-block:: erlang

    Path = filename:join([code:priv_dir(xapian), test_db, simple]). 
    {ok, Pid} = xapian_pool:open([{name, simple}], Path, []). 
    result = xapian_pool:checkout([simple], 
        fun([Server]) -> io:write(Server), result end).

Readers use the Poolboy application. There is only one writer for each
database, so there is no a writer pool. You can use a named process and
a supervisor instead:::
    {ok, Pid} = xapian_server:open(Path, [{name, simple_writer}, write]). 
    xapian_server:add_document(simple_writer, [#x_text{value = "Paragraph 1"}]).

If you try run this code from console, then next command will be useful:::
    rr(code:lib_dir(xapian, include) ++ "/xapian.hrl").

It loads information about records into console.

A pool is supervised by ``xapian_sup``. That is why, call of
``xapian_pool:open`` function do *not* link the parent process with the
new process.

As with ``xapian_drv:transaction``, you can checkout few pools.::
    xapian_pool:checkout([pool1, poo2], 
                         fun([Server1, Server2]) -> actions_here end).

If an error will occured, an exception will be thrown and workers will
be returned into the pool.::
    catch xapian_pool:checkout([simple], fun([S]) -> 5 = 2 + 2 end). 
    {'EXIT',{{badmatch,4},[{erl_eval,expr,3,[]}]}}

Multi-database support
======================

You can use this code for opening two databases from "DB1" and "DB2"
directories.::

    {ok, Server} = xapian_driver:open([#x_database{path="DB1"}, 
                                       #x_database{path="DB2"}], []).

Only read-only databases can be used.

There are two fields meaning a document's id: ``docid`` and
``multi_docid``. They are equal if only one database is used.

Otherwise, first field contains a document id (can be repeated) and
``multi_docid`` is a unique idintifier, which is calculated from
``docid`` and ``db_number``.

``db_number`` is a number of the document's database encounted from 1.

``db_name`` field contains pseudonyms of the databases. Information from
``name`` field of ``#x_database{}`` record will be used for this. This
field is ``undefined`` by default.

A full multi-database example:::

    -record(document, {docid, db_name, multi_docid, db_number}).

    example() -> 
        DB1 = #x_database{name=db1, path="DB1"}, 
        DB2 = #x_database{name=db1, path="DB2"}, 
        {ok, Server} = xapian_driver:open([DB1, DB2], []), 
        EnquireResourceId = xapian_driver:enquire(Server, "query string"), 
        MSetResourceId = xapian_driver:match_set(Server, EnquireResourceId), 
        %% Use a record_info call for retrieving a list of field names 
        Meta = xapian_record:record(document, record_info(fields, document)), 
        Table = xapian_mset_qlc:table(Server, MSetResourceId, Meta), 
        qlc:e(qlc:q([X || #document{multi_docid=DocId} <- Table])). 

Resources
=========

A resource is a C++ object, which can be passed and stored inside an
Erlang VM. Each server can have an own set of resources. Resources from
other servers cannot be used or controlled. Resources are *not*
automatically garbidge-collected, but if a control process (server)
dies, all its resources are released.

Use the ``release_resource(Server, Resource)`` function call to free 
unused anymore resource.

The second call of this function with the same arguments will cause an
error:::
    1> Path = filename:join([code:priv_dir(xapian), test_db, simple]). 
    "/home/user/erlang/xapian/priv/test_db/simple" 
    2> {ok, Server} = xapian_server:open(Path, []). {ok,<0.57.0>} 
    3> ResourceId = xapian_server:enquire(Server, "query").                     
    #Ref<0.0.0.69> 
    4> xapian_server:release_resource(Server, ResourceId).                      
    ok 
    5> xapian_server:release_resource(Server, ResourceId). 
    ** exception error: elem_not_found

Using a port
============

Ports cannot crash Erlang VM. The port program will be compilled by
rebar.

For running a single server in the port mode use:::
    {ok, Server} = xapian_driver:open(Path, [port|Params]).

For running all servers in the port mode use:::
    application:set_env(xapian, default_open_parameters, [port]).

Testing a port
--------------

.. code-block::

    $ erl -pa ./.eunit/ ./../xapian/ebin ./deps/?*/ebin
    application:set_env(xapian, default_open_parameters, [port]).
    eunit:test({application, xapian}, [verbose]). 


Document forms
==============

-  Document Constructor (CD)
-  Extracted Document (ED)
-  Document Id (ID)
-  Document Resource (RD)

Conversations:

-  ID to RD: xapian_server:document(S, ID) -> RD
-  CD to RD: xapian_server:document(S, CD) -> RD
-  DC to EC: xapian_server:document_info(S, DC, Meta) -> EC
-  ID to EC: xapian_server:read_document(S, ID, Meta) -> EC

Helpers
=======

Stand-alone Stemmer
-------------------

.. code-block:: erlang

    1> {ok, S} = xapian_server:open([],[]).
    {ok,<0.79.0>}

    2> xapian_helper:stem(S, <<"english">>, "octopus cat").
    [#x_term{value = <<"Zcat">>,position = [],frequency = 1},
     #x_term{value = <<"Zoctopus">>,position = [],frequency = 1},
     #x_term{value = <<"cat">>, position = [2], frequency = 1},
     #x_term{value = <<"octopus">>, position = [1], frequency = 1}]

    3> xapian_helper:stem(S, <<"english">>, "octopus cats").
    [#x_term{value = <<"Zcat">>,position = [],frequency = 1},
     #x_term{value = <<"Zoctopus">>,position = [],frequency = 1},
     #x_term{value = <<"cats">>, position = [2], frequency = 1},
     #x_term{value = <<"octopus">>, position = [1], frequency = 1}]

    4> xapian_helper:stem(S, none, "octopus cats").
    [#x_term{value = <<"cats">>, position = [2], frequency = 1},
     #x_term{value = <<"octopus">>, position = [1], frequency = 1}]

    5> xapian_helper:stem(S, "english", "Zcat").
    [#x_term{value = <<"Zzcat">>,position = [], frequency = 1},
     #x_term{value = <<"zcat">>, position = [1], frequency = 1}]

    6> xapian_helper:stem(S, "english", "cat octo-cat").
    [#x_term{value = <<"Zcat">>,position = [],frequency = 2},
     #x_term{value = <<"Zocto">>,position = [],frequency = 1},
     #x_term{value = <<"cat">>, position = [1,3], frequency = 2},
     #x_term{value = <<"octo">>, position = [2], frequency = 1}] 

``"Z"`` is a prefix. It means, that this term is stemmed.

Code examples
=============

-  `escripts <https://github.com/freeakk/xapian-examples>`_

Documentation
=============

-  `Edoc Reference <http://freeakk.github.com/xapian/index.html>`_
-  `Sphinx
   Manual <http://freeakk.github.com/xapian-docsprint/index.html>`_
-  `C++ Doxygen
   Reference <http://freeakk.github.com/xapian/doxygen/index.html>`_
-  `Sphinx raws <https://github.com/freeakk/xapian-docsprint>`_

