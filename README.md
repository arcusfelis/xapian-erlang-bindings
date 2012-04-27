

Installation
============

I use rebar for building.

```
git clone git://github.com/freeakk/xapian.git
cd xapian
./rebar get-deps
./rebar compile
```


Tests
=====

Next command runs tests:

```
./rebar eunit skip_deps=true
```


Cheatsheet
==========

delve (cli) - Inspect the contents of a Xapian database


A pool of readers
=================

```
Path = filename:join([code:priv_dir(xapian), test_db, simple]).
xapian_pool:open([{name, simple}], Path, []).
result = xapian_pool:checkout([simple], fun([Server]) -> io:write(Server), result end).
```

Readers use the Poolboy application. 
There is only one writer for each database, so there is no a writer pool.
You can use a named process and a supervisor instead:

```
xapian_drv:open(Path, [{name, simple_writer}, write]).
xapian_drv:add_document(simple_writer, [#x_text{value = "Paragraph 1"}]).
```

If you try run this code from console, then next command will be useful:

```
rr(code:lib_dir(xapian, include) ++ "/xapian.hrl").
```

It loads information about records into console.


A pool is supervised by `xapian\_sup`. That is why, call of 
`xapian\_pool:open` function do *not* link the parent process with the new 
process. 

As with `xapian\_drv:transaction`, you can checkout few pools.

```
xapian_pool:checkout([pool1, poo2], fun([Server1, Server2]) -> actions_here end).
```
 
If an error will occured, an exception will be thrown and workers 
will be returned into the pool.

```
catch xapian_pool:checkout([simple], fun([S]) -> 5 = 2 + 2 end).
{'EXIT',{{badmatch,4},[{erl_eval,expr,3,[]}]}}
```

