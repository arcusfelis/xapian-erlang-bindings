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

This record is used with `xapian_server:open` or with `QueryParser` 
as a parameter.

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


x_prefix_name.is_boolean
------------------------

If true, then QueryParser will combine this prefix 
with `Xapian::Query::OP_FILTER`.

This allows to use `field:value` for retrieve only documents, that have 
the field with the name `field` and the value `value`.

For example, we want to search for books about Erlang, then we can pass 
`#x_prefix_name{name = language, prefix = $L}` as a config parameter and
call a parser with a query string `"language:erlang process linux OTP"`.

This field is only used with `Xapian::QueryParser`.


If `thue`, then this method will be called:

```c++
Xapian::QueryParser::add_boolean_prefix
```


x_prefix_name.is_exclusive
--------------------------

This field is used by `QueryParser`.

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

* If `true`, this rule will be applied for a default `QueryParser`.
* If `false`, this rule will be ignored for a default `QueryParser`.


x_value_name
============

Xapian uses slots for storing information inside a document. Values 
are not indexed for fulltext search, but can be used for sorting or filtering.
A slot is an unsigned integer. For example, you can use slot 0 for a price and 
slot 1 for an year.

Erlang Bindings allows to use atoms as an id for a slot number.


x_value_name.name
-----------------

It is a full field name. It is an atom, because it used as a record field name
from MSet QLC tables.


x_value_name.slot
-----------------

It is a positive integer or zero. It is used by Xapian as a nubmer of slot.


x_value_name.type
-----------------

It can be:

- string - by default; 
- float - a value in the slot is double. It will be serialized. It can be 
          sorted in the numerical order. For example, this slot can be used for 
          storing information about prices with this type.


x_match_set
===========

This record is used because to decrease a count of parameters of the 
`xapian_server:match_set/2` function.


x_match_set.enquire
-------------------

This field is an enquire resource that defines a set of documents to handle.
It is required.


x_match_set.from
----------------

This field  contains non negative integer, that means how many objects to skip 
from the beginning.
The default value is 0.


x_match_set.max_items
---------------------

This field means the maximum count of the records inside the set. 
`undefined` means all documents (this is by default).


x_match_set.check_at_least
--------------------------

It contains the minimum count of documents, 
which will be processed by `MatchSpy`.

Actually, the lower count of documents can be passed to `MatchSpy`. 
That means, that the count of matched documents is lower than the 
`check_at_least` value.

The default value is 0.


x_match_set.spies
-----------------

Contains a list of resources of objects, which will calculate 
statistics while a query will be processed.



x_enquire
=========

x_enquire.value
---------------

It is required. It holds a query.


x_enquire.query_len
-------------------

The query length to use in weight calculations.
By default (0 value) the sum of the wqf of all terms is used. 


x_enquire.order
---------------

Primary document order.
Documents will be sorted by `relevance` by default.
`#x_sort_order{}` allows to use an advanced document order.


x_enquire.docid_order
---------------------

Secondary document order.

* `desc` - descending document order;
* `asc` and `default` are equal and mean ascending document order;
* `dont_care and `undefined` are equal.

Set the direction in which documents are ordered by document id 
in the returned MSet.

This order only has an effect on documents which would otherwise have 
equal rank. For a weighted probabilistic match with no sort value, this 
means documents with equal weight. For a boolean match, with no sort 
value, this means all documents. 
And if a sort value is used, this means documents with equal sort value 
(and also equal weight if ordering on relevance after the sort).

Note: If you add documents in strict date order, then a boolean search - 
i.e. 

```erlang
try
    WS = xapian_resource:bool_weight(Server),
    #x_enquire{weighting_scheme = WS, docid_order = desc}
    ...
after
    xapian_server:release_resource(WS)
end
```

 is a very efficient way to perform "sort by date, newest first".



x_enquire.weighting_scheme
--------------------------

Weighting scheme.
It is a resource defines how to rank documents.


x_enquire.percent_cutoff
------------------------


x_enquire.weight_cutoff
-----------------------


x_enquire.collapse_key
----------------------

Defines the slot in which the same values will be collapsed 
(each valuae of the slot will be repeated only once).
It is `undefined` by default. That means that this feature is disabled.


x_enquire.collapse_max
----------------------

number of items with the same key to leave after collapsing.
The default value is 1.



x_query_parser
==============

Converts a string, constructed using the special 
[syntax](http://xapian.org/docs/queryparser.html) into a query.

These records are equal:

```erlang
#x_query{op='OR', value=[
    #x_query_term{name="weather"},
    #x_query_term{name="forecast", position=8}]}
```

```erlang
#x_query_parser{value="weather forecast"}
```


There are few predefined parsers:

<table>
  <tr><th>Name</th><th>Stemmer</th><th>Prefixes</th></tr>
  <tr><td>default</td><td>from open params</td><td>from open params</td></tr>
  <tr><td>standard</td><td>not defined</td><td>not defined</td></tr>
</table>


x_query_parser.name
-------------------

It is a parser type to use as a prototype to extend.

* `standard` - the basic Xapian parser will be selected;
* `default` - the parser with a stemmer, which is passed to 
    `xapian_server:open/2` will be selected. 

If a stemmer did not passed to `xapian_server:open/2`, then `standard` and 
`default` parsers uses the same stemmer. 

It the name is not defined, then `default` will be used.


x_query_parser.stemmer
----------------------

When this field is `undefined`:
* and `name` is `standard` - the `standard` stemmer will be used;
* and `name` is `default` - the stemmer, passed into `xapian_server:open/2` will be used.
    
Pass `#x_stemmer{}` to use another stemmer.


x_query_parser.stemming_strategy
--------------------------------

Set the stemming strategy.

This controls how the query parser will apply the stemming algorithm. 
Note that the stemming algorithm is only applied to words in probabilistic 
fields - boolean filter terms are never stemmed.

Possible values are:

* `none`: Don't perform any stemming (the default).
* `some`: Search for stemmed forms of terms except for those which start with 
          a capital letter, or are followed by certain characters 
          (currently: `(/@<>=*[{" `), or are used with operators which need 
          positional information. Stemmed terms are prefixed with 'Z'.
* `all`:  Search for stemmed forms of all words (note: no 'Z' prefix is added). 


x_query_parser.max_wildcard_expansion
-------------------------------------

0 or `unlimited' for no limit (by default).


x_query_parser.default_op
-------------------------

It is a default operator for combining terms.
The most useful values for this are `'OR'` (the default) and `'AND'`. `'NEAR'` 
and `'PHRASE'` can also be useful.

So for example, 'weather forecast' is parsed as if it were 
'weather OR forecast' by default. 


x_query_parser.prefixes
-----------------------

A list of `#x_prefix_name{}` can be passed.
If defined, then these prefixes will be added to existed prefixes.

`Xapian::QueryParser::add_prefix` will be called for each prefix.

If `name` = `default`, then default prefixes 
    (prefixes, passed to `xapian_server:open/2`) will be already added.

If `name` = `standard`, then default prefixes will be ignored.

