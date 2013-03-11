Xapian binding for Erlang
=========================

**License**: MIT, GPL2 or higher (Xapian is still under GPL only.)

**Author**: Uvarov Michael (arcusfelis@gmail.com)

`Xapian <http://xapian.org/>`_ is an Open Source Search Engine Library,
written in C++. Xapian is a highly adaptable toolkit which allows
developers to easily add advanced indexing and search facilities to
their own applications.

Installation
============

I use rebar for building.

Try as a stand-alone Erlang application:

.. code-block:: shell

    git clone git://github.com/arcusfelis/xapian-erlang-bindings.git xapian
    cd xapian 
    ./rebar get-deps compile
    ./start-dev.sh
 
Add as a dependency to ``rebar.config``:

.. code-block:: erlang

    {deps, [                                                                     
       {xapian, ".*",                                               
           {git, "git://github.com/arcusfelis/xapian-erlang-bindings.git", "master"}}
    ]}.



Google hash map (optional)
--------------------------

You can use `google sparse
hash <http://code.google.com/p/sparsehash/?redir=1>`_ for storing
resources' ids.

In the Debian and Ubuntu repositories, this is packaged as
``libsparsehash-dev``.

The C++-preprocessor macro ``GOOGLE_HASH_MAP`` enables using google hash
map as a hash map.

Using
=====

This application uses records, defined in the file
``include/xapian.hrl``. To include it use:

.. code-block:: erlang

    -include_lib("xapian/include/xapian.hrl").

Tests
=====

Next command runs tests:

.. code-block:: shell

    $ ./rebar eunit skip_deps=true

A pool of readers
=================

.. code-block:: erlang

    Path = filename:join([code:priv_dir(xapian), test_db, simple]). 
    {ok, Pid} = xapian_pool:open([{name, simple}], Path, []). 
    result = xapian_pool:checkout([simple], 
        fun([Server]) -> io:write(Server), result end).

Readers use the Poolboy application. There is only one writer for each
database, so there is no writer pool. You can use a named process and
a supervisor instead:

.. code-block:: erlang

    {ok, Pid} = xapian_server:open(Path, [{name, simple_writer}, write]). 
    xapian_server:add_document(simple_writer, [#x_text{value = "Paragraph 1"}]).

If you try to run this code from the console, then next command will be useful:

.. code-block:: erlang

    rr(code:lib_dir(xapian, include) ++ "/xapian.hrl").

It loads information about records into the console.

A pool is supervised by ``xapian_sup``. That is why calling the
``xapian_pool:open`` function does *not* link the parent process with the
new process.

As with ``xapian_drv:transaction``, you can checkout a few pools.

.. code-block:: erlang

    xapian_pool:checkout([pool1, poo2], 
                         fun([Server1, Server2]) -> actions_here end).

If an error occurs, an exception will be thrown and workers will
be returned into the pool.

.. code-block:: erlang

    catch xapian_pool:checkout([simple], fun([S]) -> 5 = 2 + 2 end). 
    {'EXIT',{{badmatch,4},[{erl_eval,expr,3,[]}]}}

Multi-database support
======================

You can use this code for opening two databases from the directories
"DB1" and "DB2".

.. code-block:: erlang

    {ok, Server} = xapian_driver:open([#x_database{path="DB1"}, 
                                       #x_database{path="DB2"}], []).

Only read-only databases can be used.

There are two fields meaning a document's id: ``docid`` and
``multi_docid``. They are equal if only one database is used.

Otherwise, the first field contains a document id (can be repeated) and
``multi_docid`` is a unique idintifier, which is calculated from
``docid`` and ``db_number``.

``db_number`` is the number of the document's database counting from 1.

``db_name`` field contains pseudonyms of the databases. Information from
``name`` field of ``#x_database{}`` record will be used for this. This
field is ``undefined`` by default.

Here is a full multi-database example:

.. code-block:: erlang

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
Erlang VM. Each server can have its own set of resources. Resources from
other servers cannot be used or controlled. Resources are *not*
automatically garbidge-collected, but if a control process (server)
dies, all its resources are released.

Use the ``release_resource(Server, Resource)`` function call to free 
a resource which is no longer needed.

A second call of this function with the same arguments will cause an
error:

.. code-block:: erlang

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

Ports cannot crash the Erlang VM. The port program will be compiled by
rebar.

For running a single server in port mode use:

.. code-block:: erlang

    {ok, Server} = xapian_driver:open(Path, [port|Params]).

For running all servers in port mode use:

.. code-block:: erlang

    application:set_env(xapian, default_open_parameters, [port]).

Testing a port
--------------

.. code-block:: shell

    $ erl -pa ./.eunit/ ./../xapian/ebin ./deps/?*/ebin

.. code-block:: erlang

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

``"Z"`` is a prefix. It means that this term is stemmed.

Code examples
=============

-  `escripts <https://github.com/arcusfelis/xapian-examples>`_

Documentation
=============

-  `Edoc Reference <http://arcusfelis.github.com/xapian/index.html>`_
-  `Sphinx
   Manual <http://arcusfelis.github.com/xapian-docsprint/index.html>`_
-  `C++ Doxygen
   Reference <http://arcusfelis.github.com/xapian/doxygen/index.html>`_
-  `Sphinx raws <https://github.com/arcusfelis/xapian-docsprint>`_

