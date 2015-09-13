-module(xapian_term_qlc).
-export([
    value_count_match_spy_table/3, 
    value_count_match_spy_table/4,
    top_value_count_match_spy_table/4, 
    top_value_count_match_spy_table/5,
    document_term_table/3,
    document_term_table/4,
    unstem_query_parser_table/4,
    unstem_query_parser_table/5,
    stop_list_query_parser_table/3,
    stop_list_query_parser_table/4,
    spelling_table/2,
    spelling_table/3,
    synonym_table/3,
    synonym_table/4,
    synonym_key_table/3,
    synonym_key_table/4,
    metadata_key_table/3,
    metadata_key_table/4,
    term_table/3,
    term_table/4
    ]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_const, [
        spy_type_id/1, 
        query_parser_type_id/1,
        db_term_iter_type_id/1]).
-import(xapian_common, [append_string/2]).

-type x_server()    :: xapian_server:x_server().
-type x_table()     :: xapian_server:x_table().
-type x_string()    :: xapian_server:x_string().
-type x_resource()  :: xapian_server:x_resource().
-type x_term_meta() :: record().

%% @equiv spelling_table(Server, Meta, []) 
spelling_table(Server, Meta) ->
    spelling_table(Server, Meta, []).

%% @doc An iterator which returns all the spelling correction targets.
%%
%% Valid fields are:
%% <ul> <li>
%% value;
%% </li><li>
%% freq - The frequency of each word.
%% </li></ul>
%%
%% The `wdf' field isn't meaningful.
%% The records are sorted by the value field.
-spec spelling_table(x_server(), x_term_meta(), [term()]) -> x_table().
spelling_table(Server, Meta, UserParams) ->
    EncoderFun = fun(Bin@) -> 
        Bin@ = append_db_term_iter_type(spellings, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, database_terms, EncoderFun, undefined,
              Meta, UserParams2).


%% @equiv synonym_table(Server, Term, Meta, [])
synonym_table(Server, Term, Meta) ->
    synonym_table(Server, Term, Meta, []).


%% @doc An iterator which returns all the synonyms for a given term. 
-spec synonym_table(x_server(), x_string(), x_term_meta(), 
                    [term()]) -> x_table().
synonym_table(Server, Term, Meta, UserParams) ->
    EncoderFun = fun(Bin@) ->
        Bin@ = append_db_term_iter_type(synonyms, Bin@),
        Bin@ = append_string(Term, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    %% TODO: check, if sorted or not
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, database_terms, EncoderFun, undefined, 
              Meta, UserParams2).


%% @equiv synonym_key_table(Server, Prefix, Meta, [])
synonym_key_table(Server, Prefix, Meta) ->
    synonym_key_table(Server, Prefix, Meta, []).


%% @doc An iterator which returns all terms which have synonyms. 
%%
%% Only value field is meaningful.
-spec synonym_key_table(x_server(), x_string(), x_term_meta(), 
                        [term()]) -> x_table().
synonym_key_table(Server, Prefix, Meta, UserParams) ->
    EncoderFun = fun(Bin@) ->
        Bin@ = append_db_term_iter_type(synonym_keys, Bin@),
        Bin@ = append_string(Prefix, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    %% TODO: check, if sorted or not
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, database_terms, EncoderFun, undefined, 
              Meta, UserParams2).


%% @equiv metadata_key_table(Server, Prefix, Meta, [])
metadata_key_table(Server, Prefix, Meta) ->
    metadata_key_table(Server, Prefix, Meta, []).


%% @doc An iterator which returns all user-specified metadata keys. 
-spec metadata_key_table(x_server(), x_string(), x_term_meta(), 
                         [term()]) -> x_table().
metadata_key_table(Server, Prefix, Meta, UserParams) ->
    EncoderFun = fun(Bin@) ->
        Bin@ = append_db_term_iter_type(metadata_keys, Bin@),
        Bin@ = append_string(Prefix, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    %% TODO: check, if sorted or not
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, database_terms, EncoderFun, undefined, 
              Meta, UserParams2).


%% @equiv term_table(Server, Prefix, Meta, [])
term_table(Server, Prefix, Meta) ->
    term_table(Server, Prefix, Meta, []).


%% @doc An iterator which runs across all terms with a given prefix. 
-spec term_table(x_server(), x_string(), x_term_meta(), 
                [term()]) -> x_table().
term_table(Server, Prefix, Meta, UserParams) ->
    EncoderFun = fun(Bin@) ->
        Bin@ = append_db_term_iter_type(all_terms, Bin@),
        Bin@ = append_string(Prefix, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    %% TODO: check, if sorted or not
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, database_terms, EncoderFun, undefined, 
              Meta, UserParams2).


%% @equiv stop_list_query_parser_table(Server, QueryParserRes, Meta, [])
stop_list_query_parser_table(Server, QueryParserRes, Meta) ->
    stop_list_query_parser_table(Server, QueryParserRes, Meta, []).


%% @doc Iterate over terms omitted from the query as stopwords. 
%%
%% Calls `Xapian::QueryParser:toplist_begin()'.
-spec stop_list_query_parser_table(x_server(), x_resource(), x_term_meta(), 
                                   [term()]) -> x_table().
stop_list_query_parser_table(Server, QueryParserRes, Meta, UserParams)
    when is_reference(QueryParserRes) ->
    EncoderFun = fun(Bin@) ->
        Bin@ = append_query_parser_type(stop_list, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    %% TODO: check, if sorted or not
    UserParams2 = [{is_sorted_value, false} | UserParams],
    table_int(Server, query_parser_terms, EncoderFun, QueryParserRes, 
              Meta, UserParams2).


%% @equiv unstem_query_parser_table(Server, QueryParserRes, Term, Meta, [])
unstem_query_parser_table(Server, QueryParserRes, Term, Meta) ->
    unstem_query_parser_table(Server, QueryParserRes, Term, Meta, []).


%% @doc Iterate over unstemmed forms of the given (stemmed) term used 
%%      in the query.
%%
%% Calls `Xapian::QueryParser::unstem_begin(const std::string &term)'.
-spec unstem_query_parser_table(x_server(), x_resource(), x_string(), 
                                x_term_meta(), [term()]) -> x_table().
unstem_query_parser_table(Server, QueryParserRes, Term, Meta, UserParams)
    when is_reference(QueryParserRes) ->
    EncoderFun = fun(Bin@) ->
        Bin@ = append_query_parser_type(unstem, Bin@),
        Bin@ = xapian_common:append_string(Term, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    %% TODO: check, if sorted or not
    UserParams2 = [{is_sorted_value, false} | UserParams],
    table_int(Server, query_parser_terms, EncoderFun, QueryParserRes, 
              Meta, UserParams2).


%% @equiv value_count_match_spy_table(Server, SpyRes, Meta, [])
value_count_match_spy_table(Server, SpyRes, Meta) ->
    value_count_match_spy_table(Server, SpyRes, Meta, []).


%% @doc Get an iterator over the values seen in the slot. 
%%  Calls `Xapian::ValueCountMatchSpy::values_begin()'.
%% Fields of the record are limited: value, freq.
%%
%%  Records are sorted by `value' in ascending alphabetical order.
-spec value_count_match_spy_table(x_server(), x_resource(), 
                                  x_term_meta(), [term()]) -> x_table().
value_count_match_spy_table(Server, SpyRes, Meta, UserParams)
    when is_reference(SpyRes) ->
    Meta1 = xapian_term_record:fix_spy_meta(Server, SpyRes, Meta),
    EncoderFun = fun(Bin@) ->
        Bin@ = append_spy_type(values, Bin@),
        xapian_term_record:encode(Meta1, Bin@)
        end,
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, spy_terms, EncoderFun, SpyRes, Meta1, UserParams2).


%% @equiv top_value_count_match_spy_table(Server, SpyRes, Limit, Meta, [])
top_value_count_match_spy_table(Server, SpyRes, Limit, Meta) ->
    top_value_count_match_spy_table(Server, SpyRes, Limit, Meta, []).


%% @doc Get an iterator over the most frequent values seen in the slot. 
%% Calls `Xapian::ValueCountMatchSpy::top_values_begin (size_t maxvalues)'.
%% Records are sorted by `freq' in descending order.
-spec top_value_count_match_spy_table(x_server(), x_resource(), non_neg_integer(), 
                                      x_term_meta(), [term()]) -> x_table().
top_value_count_match_spy_table(Server, SpyRes, Limit, Meta, UserParams)
    when is_reference(SpyRes) ->
    Meta1 = xapian_term_record:fix_spy_meta(Server, SpyRes, Meta),
    EncoderFun = fun(Bin@) ->
        Bin@ = append_spy_type(top_values, Bin@),
        Bin@ = xapian_common:append_uint(Limit, Bin@),
        xapian_term_record:encode(Meta1, Bin@)
        end,
    UserParams2 = [{is_sorted_value, false} | UserParams],
    table_int(Server, spy_terms, EncoderFun, SpyRes, Meta1, UserParams2).


%% @equiv document_term_table(Server, DocRes, Meta, [])
document_term_table(Server, DocRes, Meta) ->
    document_term_table(Server, DocRes, Meta, []).


%% @doc Iterator for the terms in this document. 
%% Records are sorted by `value' in ascending alphabetical order.
%% Second argument can be a resource of a document or a document.
%% Calls `Xapian::Document::termlist_begin ()'.
-spec document_term_table(x_server(), x_resource(), 
                          x_term_meta(), [term()]) -> x_table().
document_term_table(Server, DocRes, Meta, UserParams) 
    when is_reference(DocRes) ->
    EncoderFun = fun(Bin) ->
        xapian_term_record:encode(Meta, Bin)
        end,
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, terms, EncoderFun, DocRes, Meta, UserParams2);

%% Extract the document and run again.
document_term_table(Server, DocId, Meta, UserParams) ->
    DocRes = xapian_server:document(Server, DocId),
    try
        document_term_table(Server, DocRes, Meta, UserParams)
    after
        xapian_server:release_resource(Server, DocRes)
    end.


% ===================================================================

%% User parameters are:
%% <ul> <li>
%% ignore_empty - catch an error, if term set is empty.
%% </li><li>
%% {page_size, non_neg_integer()}
%% </li><li>
%% {from, non_neg_integer()}
%% </li></ul>
%%
%% IterRes stands for an iterable resource (Document or MatchSpy).
table_int(Server, QlcType, EncFun, IterRes, Meta, UserParams) 
    when is_reference(IterRes) orelse IterRes =:= undefined ->
    IgnoreEmpty = lists:member(ignore_empty, UserParams),
    IsSortedValue = lists:member(is_sorted_value, UserParams),
    
    InitializationResult = 
    try
        fix_unknown_num_of_object(
            xapian_server:internal_qlc_init(Server, QlcType, IterRes, EncFun))
    catch error:#x_error{type = <<"EmptySetDriverError">>} when IgnoreEmpty ->
        empty_table()
    end,

    %% Skip first few elements
    From = proplists:get_value(from, UserParams, 0),
    case InitializationResult of
        #internal_qlc_info{num_of_objects = Size, resource_ref = QlcRes} ->
            if 
            Size >= From ->
                %% If it is not an error.
                init_not_empty_table(Server, InitializationResult, Meta,
                                     UserParams, IsSortedValue);


%% Error handling

            %% Cannot skip more elements then in the set.
            true ->
                xapian_server:release_resource(Server, QlcRes),
                %% Throw an error or return an empty table
                [erlang:error(empty_sub_list) || not IgnoreEmpty],
                empty_table()
            end;
                
        %% It is an empty table
        AlreadyCreatedTable ->
            AlreadyCreatedTable
    end.


empty_table() ->
    TraverseFun = fun() -> [] end,
    TableId = xapian_qlc_table_hash:get_table_id(),
    InfoFun = 
    fun(num_of_objects) -> 0;
       (xapian_table_id) -> TableId;
       (_) -> undefined
       end,
    qlc:table(TraverseFun, [{info_fun, InfoFun}]).


init_not_empty_table(Server, Info, Meta, UserParams, SortedValue) ->
    #internal_qlc_info{
        num_of_objects = Size,
        resource_number = ResNum,
        resource_ref = ResRef
    } = Info,
    %% Field `value' is a key.
    KeyPos = xapian_term_record:key_position(Meta),
    KeyName = xapian_term_record:field_position_to_name(Meta, KeyPos),
    From = proplists:get_value(from, UserParams, 0),
    Len = proplists:get_value(page_size, UserParams, 20),
    TraverseFun = traverse_fun(Server, ResNum, Meta, From, Len, Size),
    TableId = xapian_qlc_table_hash:get_table_id(),
    InfoFun = 
    fun(num_of_objects) -> Size;
       (keypos) -> KeyPos;
       (is_sorted_key) -> KeyPos =/= undefined andalso SortedValue;
       (is_unique_objects) -> true;
       (xapian_table_id) -> TableId;
       (_) -> undefined
       end,
    LookupFun = lookup_fun(Server, ResNum, Meta, KeyPos, KeyName),
    Table = qlc:table(TraverseFun, 
            [{info_fun, InfoFun} 
            ,{lookup_fun, LookupFun}
            ,{key_equality,'=:='}]),
    xapian_server:internal_register_qlc_table(Server, ResRef, Table),
    Table.
    

lookup_fun(Server, ResNum, Meta, KeyPos, KeyName) ->
    fun(KeyPosI, Terms) when KeyPosI =:= KeyPos ->
        Encoder = xapian_term_record:encoder(Terms, KeyName),
        Bin = xapian_server:internal_qlc_lookup(Server, Encoder, ResNum),
        {Records, <<>>} = xapian_term_record:decode_list2(Meta, Bin),
        Records
        end.


%% Maximum `Len' records can be retrieve for a call.
%% `From' records will be skipped from the beginning of the collection.
traverse_fun(Server, ResNum, Meta, From, Len, TotalLen) ->
    fun() ->
        Bin = xapian_server:internal_qlc_get_next_portion(Server, ResNum, From, Len),
        NextFrom = From+Len,
        MoreFun = traverse_fun(Server, ResNum, Meta, NextFrom, Len, TotalLen),
        {Records, <<>>} = xapian_term_record:decode_list3(Meta, Bin),
        if
            %% Last group of records of the iterator with unknown size
            TotalLen =:= undefined andalso length(Records) < Len ->
                Records;
            TotalLen =:= undefined ->
                lists:reverse(lists:reverse(Records), MoreFun);
            NextFrom < TotalLen ->
                lists:reverse(lists:reverse(Records), MoreFun);
            true ->
                Records
        end
    end.


fix_unknown_num_of_object(Info=#internal_qlc_info{num_of_objects = 0}) ->
    Info#internal_qlc_info{num_of_objects = undefined};

fix_unknown_num_of_object(Info) ->
    Info.


append_spy_type(Type, Bin) ->
    xapian_common:append_uint8(spy_type_id(Type), Bin).

append_query_parser_type(Type, Bin) ->
    xapian_common:append_uint8(query_parser_type_id(Type), Bin).

append_db_term_iter_type(Type, Bin) ->
    xapian_common:append_uint8(db_term_iter_type_id(Type), Bin).
