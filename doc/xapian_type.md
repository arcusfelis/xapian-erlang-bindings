

#Module xapian_type#
* [Data Types](#types)





<a name="types"></a>

##Data Types##




###<a name="type-x_data">x_data()</a>##



<pre>x_data() = #x_data{}</pre>



###<a name="type-x_database_name">x_database_name()</a>##



<pre>x_database_name() = atom()</pre>



###<a name="type-x_delta">x_delta()</a>##



<pre>x_delta() = #x_delta{}</pre>



###<a name="type-x_document_constructor">x_document_constructor()</a>##



<pre>x_document_constructor() = [<a href="#type-x_document_index_part">x_document_index_part()</a>]</pre>



###<a name="type-x_document_id">x_document_id()</a>##



<pre>x_document_id() = non_neg_integer()</pre>



###<a name="type-x_document_index_part">x_document_index_part()</a>##



<pre>x_document_index_part() = <a href="#type-x_term">x_term()</a> | <a href="#type-x_value">x_value()</a> | <a href="#type-x_data">x_data()</a> | <a href="#type-x_delta">x_delta()</a> | <a href="#type-x_text">x_text()</a></pre>



###<a name="type-x_inet_address">x_inet_address()</a>##



<pre>x_inet_address() = <a href="#type-x_string">x_string()</a></pre>



###<a name="type-x_inet_port">x_inet_port()</a>##



<pre>x_inet_port() = 0..65535</pre>



###<a name="type-x_language_code">x_language_code()</a>##



<pre>x_language_code() = none | &lt;&lt;&gt;&gt; | en | english | da | danish | nl | dutch | fi | finnish | fr | french | de | german | german2 | hu | hungarian | it | italian | nb | nn | no | norwegian | pt | portuguese | ro | romanian | ru | russian | es | spanish | sv | swedish | tr | turkish | lovins | porter | kraaij_pohlmann</pre>



###<a name="type-x_meta">x_meta()</a>##



<pre>x_meta() = [atom()]</pre>



###<a name="type-x_non_empty_string">x_non_empty_string()</a>##



<pre>x_non_empty_string() = nonempty_string() | &lt;&lt;_:8, _:_*8&gt;&gt;</pre>



###<a name="type-x_operator">x_operator()</a>##



<pre>x_operator() = 'AND' | 'OR' | 'AND NOT' | 'XOR' | 'AND MAYBE' | 'FILTER' | 'NEAR' | 'PHRASE' | 'VALUE RANGE' | 'SCALE WEIGHT' | 'ELITE SET' | 'VALUE GE' | 'VALUE LE' | 'SYNONYM' | greater | lower | less</pre>



###<a name="type-x_order_type">x_order_type()</a>##



<pre>x_order_type() = key | relevance | value | key_relevance | relevance_key | relevance_value | value_relevance</pre>



###<a name="type-x_parser_feature">x_parser_feature()</a>##



<pre>x_parser_feature() = 'BOOLEAN' | 'PHRASE' | 'LOVEHATE' | 'BOOLEAN ANY CASE' | 'WILDCARD' | 'PURE NOT' | 'PARTIAL' | 'SPELLING CORRECTION' | 'SYNONYM' | 'AUTO SYNONYMS' | 'AUTO MULTIWORD SYNONYMS' | 'DEFAULT' | 'SYNONYMS' | boolean | phrase | lovehate | boolean_any_case | wildcard | pure_not | partial | spelling_correction | synonym | synonyms | auto_synonyms | default</pre>



###<a name="type-x_port">x_port()</a>##



<pre>x_port() = <a href="xapian_port.md#type-x_port">xapian_port:x_port()</a></pre>



###<a name="type-x_position">x_position()</a>##



<pre>x_position() = non_neg_integer()</pre>



###<a name="type-x_prefix_name">x_prefix_name()</a>##



<pre>x_prefix_name() = #x_prefix_name{}</pre>



###<a name="type-x_query">x_query()</a>##



<pre>x_query() = #x_query{} | #x_query_value{} | #x_query_value_range{} | #x_query_term{} | #x_query_string{} | #x_query_scale_weight{}</pre>



###<a name="type-x_query_parser">x_query_parser()</a>##



<pre>x_query_parser() = #x_query_parser{}</pre>



###<a name="type-x_record">x_record()</a>##



<pre>x_record() = tuple()</pre>



###<a name="type-x_resource">x_resource()</a>##



<pre>x_resource() = reference()</pre>



###<a name="type-x_server">x_server()</a>##



<pre>x_server() = pid()</pre>



###<a name="type-x_slot">x_slot()</a>##



<pre>x_slot() = non_neg_integer()</pre>



###<a name="type-x_slot_name">x_slot_name()</a>##



<pre>x_slot_name() = atom()</pre>



###<a name="type-x_slot_value">x_slot_value()</a>##



<pre>x_slot_value() = <a href="#type-x_slot">x_slot()</a> | <a href="#type-x_slot_name">x_slot_name()</a></pre>



###<a name="type-x_stemmer">x_stemmer()</a>##



<pre>x_stemmer() = #x_stemmer{}</pre>



###<a name="type-x_string">x_string()</a>##



<pre>x_string() = string() | binary()</pre>



###<a name="type-x_table">x_table()</a>##



<pre>x_table() = <a href="qlc.md#type-query_handle">qlc:query_handle()</a></pre>



###<a name="type-x_term">x_term()</a>##



<pre>x_term() = #x_term{}</pre>



###<a name="type-x_term_count">x_term_count()</a>##



<pre>x_term_count() = non_neg_integer()</pre>



###<a name="type-x_text">x_text()</a>##



<pre>x_text() = #x_text{}</pre>



###<a name="type-x_timeout">x_timeout()</a>##



<pre>x_timeout() = non_neg_integer()</pre>



###<a name="type-x_transaction">x_transaction()</a>##



<pre>x_transaction() = fun(([<a href="#type-x_server">x_server()</a>]) -> term())</pre>



###<a name="type-x_unique_document_id">x_unique_document_id()</a>##



<pre>x_unique_document_id() = <a href="#type-x_document_id">x_document_id()</a> | <a href="#type-x_string">x_string()</a></pre>



###<a name="type-x_value">x_value()</a>##



<pre>x_value() = #x_value{}</pre>



###<a name="type-x_wdf_difference">x_wdf_difference()</a>##



<pre>x_wdf_difference() = integer() | {abs, non_neg_integer()} | {cur, integer()}</pre>
