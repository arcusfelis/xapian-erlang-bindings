-module(xapian_db).
-export([get_document/2]).

open(Path) ->
    ok.

%% Get a document from the database, given its document id.

%% This method returns a Xapian::Document object which provides 
%% the information about a document.

%% Xapian::Document Xapian::Database::get_document
%%    (   Xapian::docid   did      )      const
get_document(DB, DocumentId) ->
    ok.

search(DB, Query) ->
    ok.


