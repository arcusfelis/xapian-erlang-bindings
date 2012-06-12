

#Module xapian_server#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_server`](gen_server.md).
<a name="types"></a>

##Data Types##




###<a name="type-db_path">db_path()</a>##



<pre>db_path() = <a href="#type-x_string">x_string()</a> | <a href="#type-multi_db_path">multi_db_path()</a></pre>



###<a name="type-multi_db_path">multi_db_path()</a>##



<pre>multi_db_path() = [#x_database{} | #x_prog_database{} | #x_tcp_database{}]</pre>



###<a name="type-void">void()</a>##



__abstract datatype__: `void()`



###<a name="type-x_document_id">x_document_id()</a>##



__abstract datatype__: `x_document_id()`



###<a name="type-x_document_index_part">x_document_index_part()</a>##



__abstract datatype__: `x_document_index_part()`



###<a name="type-x_meta">x_meta()</a>##



__abstract datatype__: `x_meta()`



###<a name="type-x_query">x_query()</a>##



__abstract datatype__: `x_query()`



###<a name="type-x_record">x_record()</a>##



__abstract datatype__: `x_record()`



###<a name="type-x_resource">x_resource()</a>##



__abstract datatype__: `x_resource()`



###<a name="type-x_server">x_server()</a>##



__abstract datatype__: `x_server()`



###<a name="type-x_string">x_string()</a>##



__abstract datatype__: `x_string()`



###<a name="type-x_transaction">x_transaction()</a>##



__abstract datatype__: `x_transaction()`



###<a name="type-x_unique_document_id">x_unique_document_id()</a>##



__abstract datatype__: `x_unique_document_id()`
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_document-2">add_document/2</a></td><td></td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close the database and kill a control process (aka Server).</td></tr><tr><td valign="top"><a href="#database_info-1">database_info/1</a></td><td>Returns the list of all properties.</td></tr><tr><td valign="top"><a href="#database_info-2">database_info/2</a></td><td>Returns the list of selected properties and wanted values.</td></tr><tr><td valign="top"><a href="#delete_document-2">delete_document/2</a></td><td></td></tr><tr><td valign="top"><a href="#document-2">document/2</a></td><td>Return a document.</td></tr><tr><td valign="top"><a href="#enquire-2">enquire/2</a></td><td>Return an enquire.</td></tr><tr><td valign="top"><a href="#last_document_id-1">last_document_id/1</a></td><td>Return an identifier of the last added document.</td></tr><tr><td valign="top"><a href="#match_set-2">match_set/2</a></td><td>Return a match set (M-Set).</td></tr><tr><td valign="top"><a href="#mset_info-2">mset_info/2</a></td><td>Returns the list of all properties.</td></tr><tr><td valign="top"><a href="#mset_info-3">mset_info/3</a></td><td>Returns the list of selected properties and wanted values.</td></tr><tr><td valign="top"><a href="#multi_docid-3">multi_docid/3</a></td><td></td></tr><tr><td valign="top"><a href="#name_to_slot-1">name_to_slot/1</a></td><td></td></tr><tr><td valign="top"><a href="#name_to_slot-2">name_to_slot/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>Open the database with params.</td></tr><tr><td valign="top"><a href="#query_page-5">query_page/5</a></td><td>Return a list of records.</td></tr><tr><td valign="top"><a href="#read_document-3">read_document/3</a></td><td>Read the document with <code>DocId</code> from <code>Server</code> and put it into the record,
defined by <code>RecordMetaDefinition</code>.</td></tr><tr><td valign="top"><a href="#release_resource-2">release_resource/2</a></td><td>Release a resource.</td></tr><tr><td valign="top"><a href="#replace_document-3">replace_document/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_metadata-3">set_metadata/3</a></td><td></td></tr><tr><td valign="top"><a href="#subdb_names-1">subdb_names/1</a></td><td></td></tr><tr><td valign="top"><a href="#transaction-2">transaction/2</a></td><td>Run a transaction with 5-second timeout.</td></tr><tr><td valign="top"><a href="#transaction-3">transaction/3</a></td><td>Runs function <code>F</code> for writable <code>Servers</code> as a transaction.</td></tr><tr><td valign="top"><a href="#update_document-3">update_document/3</a></td><td>Extend (edit) the document with data.</td></tr><tr><td valign="top"><a href="#update_or_create_document-3">update_or_create_document/3</a></td><td></td></tr><tr><td valign="top"><a href="#value_to_type-1">value_to_type/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_document-2"></a>

###add_document/2##




<pre>add_document(Server::<a href="#type-x_server">x_server()</a>, Document::[<a href="#type-x_document_index_part">x_document_index_part()</a>]) -> <a href="#type-x_document_id">x_document_id()</a></pre>
<br></br>


<a name="close-1"></a>

###close/1##




`close(Server) -> any()`





Close the database and kill a control process (aka Server).       
Database will be automaticly close, if a supervised server       
process will dead.

This function is used for flushing changes of the writable database.
The caller will be blocked while all changes will not flushed.<a name="database_info-1"></a>

###database_info/1##




`database_info(Server) -> any()`



Returns the list of all properties.<a name="database_info-2"></a>

###database_info/2##




`database_info(Server, Params) -> any()`



Returns the list of selected properties and wanted values.
Properties:
* `has_positions`;
* `document_count`;
* `last_document_id`
{see last_document_id/1};
* `average_length`;
* `document_length_lower_bound`;
* `document_length_upper_bound`;
* `uuid`
Get a UUID for the database;
* `{term_exists, Term}`;
* `{term_freq, Term}`;
* `{collection_freq, WTF}`;
* `{value_freq, Value}`;
* `{value_lower_bound, Value}`;
* `{value_upper_bound, Value}`;
* `{wdf_upper_bound, Term}`;
* `{document_length, DocId}`;
* `{metadata, Key}`
Get the user-specified metadata associated with a given key.<a name="delete_document-2"></a>

###delete_document/2##




<pre>delete_document(Server::<a href="#type-x_server">x_server()</a>, DocIdOrUniqueTerm::<a href="#type-x_unique_document_id">x_unique_document_id()</a>) -> ok</pre>
<br></br>


<a name="document-2"></a>

###document/2##




<pre>document(Server::<a href="#type-x_server">x_server()</a>, DocId::<a href="#type-x_unique_document_id">x_unique_document_id()</a>) -> <a href="#type-x_resource">x_resource()</a></pre>
<br></br>




Return a document.<a name="enquire-2"></a>

###enquire/2##




<pre>enquire(Server::<a href="#type-x_server">x_server()</a>, Query::<a href="#type-x_query">x_query()</a>) -> <a href="#type-x_resource">x_resource()</a></pre>
<br></br>




Return an enquire.<a name="last_document_id-1"></a>

###last_document_id/1##




<pre>last_document_id(Server::<a href="#type-x_server">x_server()</a>) -> <a href="#type-x_document_id">x_document_id()</a> | undefined</pre>
<br></br>




Return an identifier of the last added document.
If the database is empty, returns `undefined`.<a name="match_set-2"></a>

###match_set/2##




<pre>match_set(Server::<a href="#type-x_server">x_server()</a>, X_match_set::#x_match_set{} | <a href="#type-x_resource">x_resource()</a>) -> <a href="#type-x_resource">x_resource()</a></pre>
<br></br>






Return a match set (M-Set).
A match set can be created from:
* an enquire (`x_resource()` type);
* from record `#x_match_set{}`, which contains an enquire and    
addition parameters.



Match set record is:

<pre>   #x_match_set{
       enquire = EnquireResource,
       from = From,
       max_items = MaxItems,
       check_at_least = CheckAtLeast,
       spies = Spies
   }</pre>

where
* `EnquireResource` contains the result of the search.

__See also:__ [xapian_mset_qlc](xapian_mset_qlc.md), [It is required;
* `From` means how many elements to skip. It is 0 by default;
* `MaxItems` means how many elements to return.
Not more than `MaxItems` elements will be return.
It is `undefined` by default,
that means all items will be selected;
* `Spies` is a list of MatchSpy resources (@see xapian_match_spy).
](#enquire-2), [mset_info/3](#mset_info-3).<a name="mset_info-2"></a>

###mset_info/2##




`mset_info(Server, MSetResource) -> any()`



Returns the list of all properties.<a name="mset_info-3"></a>

###mset_info/3##




`mset_info(Server, MSetResource, Params) -> any()`



Returns the list of selected properties and wanted values.
Properties:
* `matches_lower_bound`;
* `matches_estimated`;
* `matches_upper_bound`;
* `uncollapsed_matches_lower_bound`;
* `uncollapsed_matches_estimated`;
* `uncollapsed_matches_upper_bound`;
* `size`;
* `max_possible`;
* `max_attained`;
* `{term_weight, Term}`;
* `{term_freq, Term}`.<a name="multi_docid-3"></a>

###multi_docid/3##




`multi_docid(State, DocId, SubDbName) -> any()`

<a name="name_to_slot-1"></a>

###name_to_slot/1##




`name_to_slot(State) -> any()`

<a name="name_to_slot-2"></a>

###name_to_slot/2##




`name_to_slot(ServerOrState, Slot) -> any()`

<a name="open-2"></a>

###open/2##




<pre>open(Path::<a href="#type-db_path">db_path()</a>, Params::[term()]) -> {ok, <a href="#type-x_server">x_server()</a>}</pre>
<br></br>






Open the database with params.
`Path` is a directory name of the database.  
For opening multiple databases you can pass a list of:



* #x_database{};  
* #x_prog_database{};  
* #x_tcp_database{}.



See the description of these records for more information.



`Params` is a list of:



* Modes: read, write, overwrite, create, open;
* Names for values and for prefixes:
`#x_value_name{slot = 1, name = slotname}`
`#x_prefix_name{name = author, prefix = <<$A>>}`;
* The default stemmer. It will be used in `TermGenerator` and in the
`default_query_parser`:
`#x_stemmer{language="english"}`;
* An interface to work: `port` (or `driver` by default).

The `read` mode is only for reading.
The `write` mode is for reading and for writing.
Write mode can be combined with:
`open` (default), `create`, `overwrite`.<a name="query_page-5"></a>

###query_page/5##




<pre>query_page(Server::<a href="#type-x_server">x_server()</a>, Offset::non_neg_integer(), PageSize::non_neg_integer(), Query::<a href="#type-x_query">x_query()</a>, RecordMetaDefinition::<a href="#type-x_meta">x_meta()</a>) -> [<a href="#type-x_record">x_record()</a>]</pre>
<br></br>




Return a list of records.<a name="read_document-3"></a>

###read_document/3##




`read_document(Server, DocId, RecordMetaDefinition) -> any()`





Read the document with `DocId` from `Server` and put it into the record,
defined by `RecordMetaDefinition`.

<pre>       RecordMetaDefinition =
           xapian_record:record(record_name, record_info(fields, record_fields)).</pre><a name="release_resource-2"></a>

###release_resource/2##




<pre>release_resource(Server::<a href="#type-x_server">x_server()</a>, ResourceRef::<a href="#type-x_resource">x_resource()</a>) -> <a href="#type-void">void()</a></pre>
<br></br>




Release a resource.<a name="replace_document-3"></a>

###replace_document/3##




<pre>replace_document(Server::<a href="#type-x_server">x_server()</a>, DocIdOrUniqueTerm::<a href="#type-x_unique_document_id">x_unique_document_id()</a>, NewDocument::[<a href="#type-x_document_index_part">x_document_index_part()</a>]) -> <a href="#type-x_document_id">x_document_id()</a></pre>
<br></br>


<a name="set_metadata-3"></a>

###set_metadata/3##




<pre>set_metadata(Server::<a href="#type-x_server">x_server()</a>, Key::<a href="#type-x_string">x_string()</a>, Value::<a href="#type-x_string">x_string()</a>) -> ok</pre>
<br></br>


<a name="subdb_names-1"></a>

###subdb_names/1##




`subdb_names(State) -> any()`

<a name="transaction-2"></a>

###transaction/2##




`transaction(Servers, F) -> any()`



Run a transaction with 5-second timeout.<a name="transaction-3"></a>

###transaction/3##




<pre>transaction(Servers::[<a href="#type-x_server">x_server()</a>], F::<a href="#type-x_transaction">x_transaction()</a>, Timeout::timeout()) -> #x_transaction_result{}</pre>
<br></br>






Runs function `F` for writable `Servers` as a transaction.       
Transaction will stop other operations with selected databases.



This function runs a transaction on few servers.
`F` will be called as:

<pre>  Servers = [Server1, Server2, Server3].
   F([TransServer1, TransServer2, TransServer3]).</pre>



Results (`#x_transaction_result.statuses`) from a server:



* `committed` - A transaction was pass on this server. Data is consistent;
* `aborted` - A transaction was canceled on this server. Data is consistent;
* `failed` - An exeption was occured. Data is inconsistent.

If one of the servers crashed during transaction, the transaction process
will be killed using `cancel_transaction` with reason `crashed_server`.<a name="update_document-3"></a>

###update_document/3##




<pre>update_document(Server::<a href="#type-x_server">x_server()</a>, DocIdOrUniqueTerm::<a href="#type-x_unique_document_id">x_unique_document_id()</a>, NewDocument::[<a href="#type-x_document_index_part">x_document_index_part()</a>]) -> <a href="#type-x_document_id">x_document_id()</a></pre>
<br></br>




Extend (edit) the document with data.<a name="update_or_create_document-3"></a>

###update_or_create_document/3##




<pre>update_or_create_document(Server::<a href="#type-x_server">x_server()</a>, DocIdOrUniqueTerm::<a href="#type-x_unique_document_id">x_unique_document_id()</a>, NewDocument::[<a href="#type-x_document_index_part">x_document_index_part()</a>]) -> <a href="#type-x_document_id">x_document_id()</a></pre>
<br></br>


<a name="value_to_type-1"></a>

###value_to_type/1##




`value_to_type(State) -> any()`

