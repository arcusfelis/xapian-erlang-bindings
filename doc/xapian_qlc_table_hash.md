

#Module xapian_qlc_table_hash#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This structure is for converting beetween qlc_reference and table_hash.


<a name="types"></a>

##Data Types##




###<a name="type-hr_error">hr_error()</a>##



<pre>hr_error() = elem_not_found</pre>



###<a name="type-hr_key">hr_key()</a>##



<pre>hr_key() = <a href="#type-qlc_reference">qlc_reference()</a> | <a href="#type-table_hash">table_hash()</a></pre>



###<a name="type-hr_read_result">hr_read_result()</a>##



<pre>hr_read_result() = {ok, {<a href="#type-qlc_reference">qlc_reference()</a>, <a href="#type-table_hash">table_hash()</a>}} | {error, <a href="#type-hr_error">hr_error()</a>}</pre>



###<a name="type-hr_store">hr_store()</a>##



<pre>hr_store() = #qlc_table_hash_register{}</pre>



###<a name="type-hr_write_result">hr_write_result()</a>##



<pre>hr_write_result() = {ok, {<a href="#type-hr_store">hr_store()</a>, <a href="#type-qlc_reference">qlc_reference()</a>, <a href="#type-table_hash">table_hash()</a>}} | {error, <a href="#type-hr_error">hr_error()</a>}</pre>



###<a name="type-qlc_reference">qlc_reference()</a>##



<pre>qlc_reference() = reference()</pre>



###<a name="type-table_hash">table_hash()</a>##



<pre>table_hash() = non_neg_integer()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-2">erase/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="erase-2"></a>

###erase/2##




<pre>erase(Store::<a href="#type-hr_store">hr_store()</a>, Key::<a href="#type-hr_key">hr_key()</a>) -> <a href="#type-hr_write_result">hr_write_result()</a></pre>
<br></br>


<a name="get-2"></a>

###get/2##




<pre>get(Store::<a href="#type-hr_store">hr_store()</a>, Key::<a href="#type-hr_key">hr_key()</a>) -> <a href="#type-hr_read_result">hr_read_result()</a></pre>
<br></br>


<a name="hash-1"></a>

###hash/1##




<pre>hash(Table::term()) -> <a href="#type-table_hash">table_hash()</a></pre>
<br></br>


<a name="new-0"></a>

###new/0##




<pre>new() -> <a href="#type-hr_store">hr_store()</a></pre>
<br></br>


<a name="put-3"></a>

###put/3##




<pre>put(Store::<a href="#type-hr_store">hr_store()</a>, Ref::<a href="#type-qlc_reference">qlc_reference()</a>, Hash::<a href="#type-table_hash">table_hash()</a>) -> <a href="#type-hr_store">hr_store()</a></pre>
<br></br>


