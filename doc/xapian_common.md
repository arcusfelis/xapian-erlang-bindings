

#Module xapian_common#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append_boolean-2">append_boolean/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_docids-2">append_docids/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_document_id-2">append_document_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_double-2">append_double/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_int-2">append_int/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_int8-2">append_int8/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_iolist-2">append_iolist/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_slot-2">append_slot/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_slot-3">append_slot/3</a></td><td></td></tr><tr><td valign="top"><a href="#append_terms-2">append_terms/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_uint-2">append_uint/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_uint16-2">append_uint16/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_uint8-2">append_uint8/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_unique_document_id-2">append_unique_document_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#append_value-2">append_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#fix_value-3">fix_value/3</a></td><td>Returns the fixed value: float or string if all is ok.</td></tr><tr><td valign="top"><a href="#index_of-2">index_of/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_boolean-1">read_boolean/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_db_id-1">read_db_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_doccount-1">read_doccount/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_document_count-1">read_document_count/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_document_id-1">read_document_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_document_length-1">read_document_length/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_double-1">read_double/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_maybe-2">read_maybe/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_percent-1">read_percent/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_position_list-1">read_position_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_rank-1">read_rank/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_string-1">read_string/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_term_count-1">read_term_count/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_uint-1">read_uint/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_uint8-1">read_uint8/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_unknown_type_value-1">read_unknown_type_value/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_weight-1">read_weight/1</a></td><td></td></tr><tr><td valign="top"><a href="#slot_id-2">slot_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#slot_type-2">slot_type/2</a></td><td></td></tr><tr><td valign="top"><a href="#string_to_binary-1">string_to_binary/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="append_boolean-2"></a>

###append_boolean/2##




`append_boolean(Value, Bin) -> any()`

<a name="append_docids-2"></a>

###append_docids/2##




`append_docids(DocIds, Bin@) -> any()`

<a name="append_document_id-2"></a>

###append_document_id/2##




`append_document_id(Id, Bin) -> any()`

<a name="append_double-2"></a>

###append_double/2##




`append_double(Value, Bin) -> any()`

<a name="append_int-2"></a>

###append_int/2##




`append_int(Num, Bin) -> any()`

<a name="append_int8-2"></a>

###append_int8/2##




`append_int8(Num, Bin) -> any()`

<a name="append_iolist-2"></a>

###append_iolist/2##




`append_iolist(Str, Bin) -> any()`

<a name="append_slot-2"></a>

###append_slot/2##




`append_slot(Slot, Bin) -> any()`

<a name="append_slot-3"></a>

###append_slot/3##




`append_slot(Slot, N2S, Bin) -> any()`

<a name="append_terms-2"></a>

###append_terms/2##




`append_terms(Terms, Bin@) -> any()`

<a name="append_uint-2"></a>

###append_uint/2##




`append_uint(Value, Bin) -> any()`

<a name="append_uint16-2"></a>

###append_uint16/2##




`append_uint16(Value, Bin) -> any()`

<a name="append_uint8-2"></a>

###append_uint8/2##




`append_uint8(Value, Bin) -> any()`

<a name="append_unique_document_id-2"></a>

###append_unique_document_id/2##




`append_unique_document_id(Id, Bin) -> any()`

<a name="append_value-2"></a>

###append_value/2##




`append_value(Value, Bin@) -> any()`

<a name="fix_value-3"></a>

###fix_value/3##




`fix_value(Slot, Value, Slot2TypeArray) -> any()`



Returns the fixed value: float or string if all is ok.
Othervise, throws an error.<a name="index_of-2"></a>

###index_of/2##




`index_of(Item, List) -> any()`

<a name="read_boolean-1"></a>

###read_boolean/1##




`read_boolean(Bin) -> any()`

<a name="read_db_id-1"></a>

###read_db_id/1##




`read_db_id(Bin) -> any()`

<a name="read_doccount-1"></a>

###read_doccount/1##




`read_doccount(Bin) -> any()`

<a name="read_document_count-1"></a>

###read_document_count/1##




`read_document_count(Bin) -> any()`

<a name="read_document_id-1"></a>

###read_document_id/1##




`read_document_id(Bin) -> any()`

<a name="read_document_length-1"></a>

###read_document_length/1##




`read_document_length(Bin) -> any()`

<a name="read_double-1"></a>

###read_double/1##




`read_double(Bin) -> any()`

<a name="read_maybe-2"></a>

###read_maybe/2##




`read_maybe(Fn, Bin) -> any()`

<a name="read_percent-1"></a>

###read_percent/1##




`read_percent(Bin) -> any()`

<a name="read_position_list-1"></a>

###read_position_list/1##




`read_position_list(Bin@) -> any()`

<a name="read_rank-1"></a>

###read_rank/1##




`read_rank(Bin) -> any()`

<a name="read_string-1"></a>

###read_string/1##




`read_string(Bin) -> any()`

<a name="read_term_count-1"></a>

###read_term_count/1##




`read_term_count(Bin) -> any()`

<a name="read_uint-1"></a>

###read_uint/1##




`read_uint(Bin) -> any()`

<a name="read_uint8-1"></a>

###read_uint8/1##




`read_uint8(Bin) -> any()`

<a name="read_unknown_type_value-1"></a>

###read_unknown_type_value/1##




`read_unknown_type_value(Bin1) -> any()`

<a name="read_weight-1"></a>

###read_weight/1##




`read_weight(Bin) -> any()`

<a name="slot_id-2"></a>

###slot_id/2##




`slot_id(Name, N2S) -> any()`

<a name="slot_type-2"></a>

###slot_type/2##




`slot_type(Slot, N2S) -> any()`

<a name="string_to_binary-1"></a>

###string_to_binary/1##




`string_to_binary(Str) -> any()`

