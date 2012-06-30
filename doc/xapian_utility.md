

#Module xapian_utility#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_server`](gen_server.md).
<a name="types"></a>

##Data Types##




###<a name="type-replicate_client_param">replicate_client_param()</a>##



<pre>replicate_client_param() = writable | link | one_shot | {interval, <a href="xapian_type.md#type-x_timeout">xapian_type:x_timeout()</a>} | {reader_time, <a href="xapian_type.md#type-x_timeout">xapian_type:x_timeout()</a>} | {port, <a href="xapian_type.md#type-x_inet_port">xapian_type:x_inet_port()</a>} | {address, <a href="xapian_type.md#type-x_init_address">xapian_type:x_init_address()</a>} | {master_name, <a href="xapian_type.md#type-x_string">xapian_type:x_string()</a>}</pre>



###<a name="type-replicate_server_param">replicate_server_param()</a>##



<pre>replicate_server_param() = link | one_shot | {port, <a href="xapian_type.md#type-x_inet_port">xapian_type:x_inet_port()</a>} | {address, <a href="xapian_type.md#type-x_inet_address">xapian_type:x_inet_address()</a>}</pre>



###<a name="type-start_result">start_result()</a>##



<pre>start_result() = {ok, pid()} | {error, term()}</pre>



###<a name="type-tcp_server_param">tcp_server_param()</a>##



<pre>tcp_server_param() = writable | {timeout, <a href="xapian_type.md#type-x_timeout">xapian_type:x_timeout()</a>} | link | {idle_timeout, <a href="xapian_type.md#type-x_timeout">xapian_type:x_timeout()</a>} | {active_timeout, <a href="xapian_type.md#type-x_timeout">xapian_type:x_timeout()</a>} | one_shot | {port, <a href="xapian_type.md#type-x_inet_port">xapian_type:x_inet_port()</a>} | {address, <a href="xapian_type.md#type-x_init_address">xapian_type:x_init_address()</a>}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#replicate_client-2">replicate_client/2</a></td><td></td></tr><tr><td valign="top"><a href="#replicate_server-2">replicate_server/2</a></td><td></td></tr><tr><td valign="top"><a href="#tcp_server-2">tcp_server/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="code_change-3"></a>

###code_change/3##




`code_change(OldVsn, State, Extra) -> any()`

<a name="handle_call-3"></a>

###handle_call/3##




`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

###handle_cast/2##




`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

###handle_info/2##




`handle_info(Info, State) -> any()`

<a name="init-1"></a>

###init/1##




`init(Args) -> any()`

<a name="replicate_client-2"></a>

###replicate_client/2##




<pre>replicate_client(DbDir::string(), Params::[<a href="#type-replicate_client_param">replicate_client_param()</a>]) -> <a href="#type-start_result">start_result()</a></pre>
<br></br>


<a name="replicate_server-2"></a>

###replicate_server/2##




<pre>replicate_server(DirWithDatabases::string(), Params::[<a href="#type-replicate_server_param">replicate_server_param()</a>]) -> <a href="#type-start_result">start_result()</a></pre>
<br></br>


<a name="tcp_server-2"></a>

###tcp_server/2##




<pre>tcp_server(DbDirs::[string()], Params::[<a href="#type-tcp_server_param">tcp_server_param()</a>]) -> <a href="#type-start_result">start_result()</a></pre>
<br></br>


<a name="terminate-2"></a>

###terminate/2##




`terminate(Reason, State) -> any()`

