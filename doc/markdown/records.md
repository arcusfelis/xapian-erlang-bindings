x_term
======

Term record.

x_term.value
------------

The name of the term.
This field is required.


x_term.position
---------------

The position of the term. 
If position = undefined, then we will use Xapian::Document::add_term,
otherwise Xapian::Document::add_posting.

You can put few positions as a list.

`#x_term{term=term, action=set, position=[]}` deletes all positions 
of the term="term".

`#x_term{action=set, position=[]}` deletes all positions for all terms.

This field is optional.


x_term.frequency
----------------

The within-document frequency, or wdf, of a term t in D is the 
number of times it is pulled out of D in the indexing process. 
Usually this is the size of the wdp vector, but in Xapian it can 
exceed it, since we can apply extra wdf to some parts of the 
document text. For example, often this is done for the document 
title and abstract to attach extra importance to their contents 
compared to the rest of the document text.

WDF of the current term will be increase by this value when indexing.
If WDF = 0, then Xapian::Document::add_boolean_term will be called.

WDF can be `{cur, integer()}` or `{abs, non_neg_integer()}`. 
1 means `{cur, 1}`.

All postings and terms with the same value have common WDF.

The defaut value of this field is 1.


x_term.action
-------------

* If position is undefined, then:
   * If action = remove and WDF = 0, then the term will be deleted.
   * If action = remove and WDF != 0, then the term with exactly same 
     WDF will be deleted otherwise error will be occured (it can
     be diabled with ignore = true). 
* If position is an integer, then:
   * If action = add, then posting will be added, WDF will be increased 
     with frequency. 
   * If action = remove, then `Xapian::Document::remove_posting`
     will be used. 
* If the action is `add`, then the term must not exist. 
* If the action is `set`, then don't care about old version of the term.
* If the action is `update`, then the term must exist. 


The defaut value of this field is `set`.


x_term.ignore
-------------

Ignore errors.


* If `action = add AND ignore`, 
  then if a term exists, then it will be skipped. 
* If `action = add AND NOT ignore`, 
  then if a term exists, then an an exception will be thrown. 
* If `action = update AND ignore`, 
  then if a term exists, then it will be skipped. 
* If `action = update AND NOT ignore`, 
  then if a term not exists, then an an exception will be thrown. 
* If `action = set`, 
  then if a term exists, then it will be rewritten. 
* If `action = set`, 
  then if a term does not exist, then it will be created. 
* If `action = remove`, 
  then if a term exists, then it will be deleted. 
* If `action = remove AND ignore`, 
  then if a term does not exist, then it will be skipped. 
* If `action = remove AND NOT ignore`, 
  then if a term does not exist, then an an exception will be thrown. 


The default value of this field is `true`.




x_prefix_name
=============

Describes meta-information about term prefixes.

This record is used with `xapian_server:open` as a parameter.

Example of using for 
```c++
queryparser.add_prefix("title", "S");
```

```erlang
#x_prefix_name{name=title, prefix=<<"S">>}
```

It contains alternative name for keys.
Xapian is schemaless, so the schema is storing in the client code.
Prefixes are a part of the schema.

`is_boolean`, `is_exclusive` and `is_default` are only 
important for QueryParser.


x_prefix_name.name
------------------

It is a full field name.

A prefix name `#x_prefix_name.name` is a pseudonym for prefix 
`#x_prefix_name.prefix`.

This name is for a client and the prefix is for Xapian.

A prefix name is usually an atom, because of speed.
But it can be any iolist, it will be converted into a binary with 
`iolist_to_binary` before to pass to Xapian (for example, it will be
passed for `QueryParser`).

In Erlang code, the name can be a term, but it will be matched with an 
operator `=:=`. 
That is why, `#x_prefix_name{name="author"}` and `#x_prefix_name{name=author}`
are different prefixes. That is why just use atoms for names everywhere.



x_prefix_name.prefix
--------------------

It is a short field name.

This value is used internally by Xapian.
It often is an uppercase letter, so it can be a char, for example,
`#x_prefix_name{prefix=$A, name=author}`. 

There is no difference in the format for this field.


If true, then QueryParser will combine this prefix 
with `Xapian::Query::OP_FILTER`.

This allows to use `field:value` for retrieve only documents, that have 
the field with the name `field` and the value `value`.

For example, we want to search for books about Erlang, then we can pass 
`#x_prefix_name{name = language, prefix = $L}` as a config parameter and
call a parser with a query string `"language:erlang process linux OTP"`.

This field is only used with `Xapian::QueryParser`.


```c++
Xapian::QueryParser::add_boolean_prefix
```


x_prefix_name.is_exclusive
--------------------------

If `true`, each document can have at most one term with this prefix, 
so multiple filters with this prefix should be combined with `OR`. 

If `false`, each document can have multiple terms with this prefix, 
so multiple filters should be combined with `AND`, like happens with 
filters with different prefixes.

Ignored, if `is_boolean` is not `true`.


x_prefix_name.is_default
------------------------

This rule only used, when this record is passed as a parameter of
the `xapian_server:open` method.

* If `true`, this rule will be applied for a default QueryParser.
* If `false`, this rule will be ignored for a default QueryParser.
