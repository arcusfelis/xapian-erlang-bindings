

#Module xapian#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)





<a name="types"></a>

##Data Types##




###<a name="type-x_term">x_term()</a>##



<pre>x_term() = #x_term{value = <a href="xapian_type.md#type-x_string">xapian_type:x_string()</a>, position = <a href="xapian_type.md#type-x_position">xapian_type:x_position()</a> | [<a href="xapian_type.md#type-x_position">xapian_type:x_position()</a>] | undefined, frequency = <a href="xapian_type.md#type-x_term_count">xapian_type:x_term_count()</a>, action = add | set | update | remove, ignore = boolean()}</pre>




###<a name="Term_record">Term record</a>##



####<a name="x_term.value">x_term.value</a>##


  
The name of the term.  
This field is required.




####<a name="x_term.position">x_term.position</a>##


  
The position of the term.  
If position = undefined, then we will use Xapian::Document::add_term,  
otherwise Xapian::Document::add_posting.



You can put few positions as a list.



`#x_term{term=term, action=set, position=[]}` deletes all positions  
of the term="term".



`#x_term{action=set, position=[]}` deletes all positions for all terms.



This field is optional.




####<a name="x_term.frequency">x_term.frequency</a>##


  
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




####<a name="x_term.action">x_term.action</a>##




If position is undefined, then:  
* If action = remove and WDF = 0, then the term will be deleted.  
* If action = remove and WDF != 0, then the term with exactly same         
WDF will be deleted otherwise error will be occured (it can         
be diabled with ignore = true).



If position is an integer, then:
* If action = add, then posting will be added, WDF will be increased
with frequency.
*  If action = remove, then `Xapian::Document::remove_posting`       
will be used.



If the action is `add`, then the term must not exist.



If the action is `set`, then don't care about old version of the term.



If the action is `update`, then the term must exist.



The defaut value of this field is `set`.




####<a name="x_term.ignore">x_term.ignore</a>##


  
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
** If `action = remove AND NOT ignore`,       
then if a term does not exist, then an an exception will be thrown.

The default value of this field is `true`.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="start-0"></a>

###start/0##




`start() -> any()`

