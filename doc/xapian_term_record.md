

#Module xapian_term_record#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode_list-2">decode_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode_list2-2">decode_list2/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode_list3-2">decode_list3/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#key_position-1">key_position/1</a></td><td></td></tr><tr><td valign="top"><a href="#record-2">record/2</a></td><td>You can use special names for fields:.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="decode-2"></a>

###decode/2##




<pre>decode(Meta::term(), Bin::binary()) -&gt; {term(), binary()}</pre>
<br></br>


<a name="decode_list-2"></a>

###decode_list/2##




`decode_list(Meta, Bin@) -> any()`

<a name="decode_list2-2"></a>

###decode_list2/2##




`decode_list2(Meta, Bin) -> any()`

<a name="decode_list3-2"></a>

###decode_list3/2##




`decode_list3(Meta, Bin@) -> any()`

<a name="encode-1"></a>

###encode/1##




`encode(Meta) -> any()`

<a name="encode-2"></a>

###encode/2##




`encode(Meta, Bin) -> any()`

<a name="key_position-1"></a>

###key_position/1##




`key_position(Rec) -> any()`

<a name="record-2"></a>

###record/2##




`record(TupleName, TupleFields) -> any()`





You can use special names for fields:

* wdf
* freq
* value
* positions