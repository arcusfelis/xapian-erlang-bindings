If a string is passed, it will be handled as a term.

Two next expressions are equal:

```erlang
xapian_server:enquire(Server, "erlang").
```

```erlang
Query = #x_term{value = "erlang"},
xapian_server:enquire(Server, Query).
```

If you want search by few terms, use:

```erlang
Query = #x_query{value = ["erlang", "haskell"]}.
```

Here "erlang" and "haskell" will be combined with "AND".

You can combain them with "OR" operator:

```erlang
Query = #x_query{op='OR', value = ["erlang", "haskell", "ocalm"]}.
```
