

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
