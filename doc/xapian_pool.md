

#Module xapian_pool#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)





<a name="types"></a>

##Data Types##




###<a name="type-pool_param">pool_param()</a>##



<pre>pool_param() = {name, atom() | {local, atom()} | {global, atom()}} | {worker_module, atom()} | {size, non_neg_integer()} | {max_overflow, non_neg_integer()}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#checkout-2">checkout/2</a></td><td></td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="checkout-2"></a>

###checkout/2##




`checkout(PoolNames, Fun) -> any()`

<a name="close-1"></a>

###close/1##




<pre>close(PoolName::atom() | pid()) -&gt; term()</pre>
<br></br>


<a name="open-3"></a>

###open/3##




<pre>open(PoolParams::[<a href="#type-pool_param">pool_param()</a>], Path::iolist(), Params::[term()]) -> {ok, pid()} | {error, term}</pre>
<br></br>


<a name="start_link-1"></a>

###start_link/1##




`start_link(Args) -> any()`

