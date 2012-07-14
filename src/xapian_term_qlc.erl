-module(xapian_term_qlc).
-export([
    value_count_match_spy_table/3, 
    value_count_match_spy_table/4,
    top_value_count_match_spy_table/4, 
    top_value_count_match_spy_table/5,
    document_term_table/3,
    document_term_table/4 ]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_const, [spy_type_id/1]).


%% Fields of the record are limited:
%%  * value
%%  * freq
%%
%%  Records are sorted by `value' in ascending alphabetical order.
value_count_match_spy_table(Server, SpyRes, Meta) ->
    value_count_match_spy_table(Server, SpyRes, Meta, []).


value_count_match_spy_table(Server, SpyRes, Meta, UserParams) ->
    EncoderFun = fun(match_spy, _DrvState, Bin@) ->
        Bin@ = append_spy_type(values, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    UserParams2 = [{is_sorted_value, true} | UserParams],
    table_int(Server, spy_terms, EncoderFun, SpyRes, Meta, UserParams2).


%%  Records are sorted by `freq' in descending order.
top_value_count_match_spy_table(Server, SpyRes, Limit, Meta) ->
    top_value_count_match_spy_table(Server, SpyRes, Limit, Meta, []).


top_value_count_match_spy_table(Server, SpyRes, Limit, Meta, UserParams) ->
    EncoderFun = fun(match_spy, _DrvState, Bin@) ->
        Bin@ = append_spy_type(top_values, Bin@),
        Bin@ = xapian_common:append_uint(Limit, Bin@),
        xapian_term_record:encode(Meta, Bin@)
        end,
    UserParams2 = [{is_sorted_value, false} | UserParams],
    table_int(Server, spy_terms, EncoderFun, SpyRes, Meta, UserParams2).


%%  Records are sorted by `value' in ascending alphabetical order.
document_term_table(Server, DocRes, Meta) ->
    document_term_table(Server, DocRes, Meta, []).


%% Second argument can be a resource of a document or a document.
document_term_table(Server, DocRes, Meta, UserParams) 
    when is_reference(DocRes) ->
    EncoderFun = fun(document, Bin) ->
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


%% User parameters are:
%% * ignore_empty - catch an error, if term set is empty.
%% * {page_size, non_neg_integer()}
%% * {from, non_neg_integer()}
%%
%% IterRes stands for an iterable resource (Document or MatchSpy).
table_int(Server, QlcType, EncFun, IterRes, Meta, UserParams) 
    when is_reference(IterRes) ->
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
    InfoFun = 
    fun(num_of_objects) -> 0;
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
    From = proplists:get_value(from, UserParams, 0),
    Len = proplists:get_value(page_size, UserParams, 20),
    TraverseFun = traverse_fun(Server, ResNum, Meta, From, Len, Size),
    InfoFun = 
    fun(num_of_objects) -> Size;
       (keypos) -> KeyPos;
       (is_sorted_key) -> KeyPos =/= undefined andalso SortedValue;
       (is_unique_objects) -> true;
       (_) -> undefined
       end,
    LookupFun = lookup_fun(Server, ResNum, Meta, KeyPos),
    Table = qlc:table(TraverseFun, 
            [{info_fun, InfoFun} 
            ,{lookup_fun, LookupFun}
            ,{key_equality,'=:='}]),
    xapian_server:internal_register_qlc_table(Server, ResRef, Table),
    Table.
    

lookup_fun(Server, ResNum, Meta, KeyPos) ->
    fun(KeyPosI, Terms) when KeyPosI =:= KeyPos ->
        Bin = xapian_server:internal_qlc_lookup(Server, encoder(Terms), ResNum),
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


%% Lookup encoder appends terms for searching.
encoder(Terms = [_|_]) ->
    fun(Bin) -> 
        xapian_common:append_terms(Terms, Bin)
        end.


append_spy_type(Type, Bin) ->
    xapian_common:append_uint8(spy_type_id(Type), Bin).
