-module(xapian_term_qlc).
-export([table/3, table/4]).
-export([value_count_match_spy_table/3, value_count_match_spy_table/4]).
-export([top_value_count_match_spy_table/4, top_value_count_match_spy_table/5]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("xapian/include/xapian.hrl").
-include("xapian.hrl").


table(Server, DocRes, Meta) ->
    table(Server, terms, DocRes, Meta, []).

table(Server, DocRes, Meta, UserParams) ->
    table(Server, terms, DocRes, Meta, UserParams).


value_count_match_spy_table(Server, SpyRes, Meta) ->
    table(Server, spy_terms, SpyRes, Meta, []).

value_count_match_spy_table(Server, SpyRes, Meta, UserParams) ->
    table(Server, spy_terms, SpyRes, Meta, UserParams).


top_value_count_match_spy_table(Server, SpyRes, Limit, Meta) ->
    table(Server, top_spy_terms, SpyRes, Meta, [{max_values, Limit}]).

top_value_count_match_spy_table(Server, SpyRes, Limit, Meta, UserParams) ->
    table(Server, top_spy_terms, SpyRes, Meta, [{max_values, Limit} | UserParams]).


%% User parameters are:
%% * ignore_empty - catch an error, if term set is empty.
%% * {page_size, non_neg_integer()}
%% * {from, non_neg_integer()}
table(Server, QlcType, DocRes, Meta, UserParams) 
    when is_reference(DocRes) ->
    QlcParams = 
    #internal_qlc_term_parameters{ record_info = Meta, 
        user_parameters = UserParams },

    IgnoreEmpty = lists:member(ignore_empty, UserParams),
    
    InitializationResult = 
    try
        fix_unknown_num_of_object(
            xapian_drv:internal_qlc_init(Server, QlcType, DocRes, QlcParams))
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
                init_not_empty_table(Server, InitializationResult, Meta, UserParams);


%% Error handling

            %% Cannot skip more elements then in the set.
            true ->
                xapian_drv:release_resource(Server, QlcRes),
                %% Throw an error or return an empty table
                [erlang:error(empty_sub_list) || not IgnoreEmpty],
                empty_table()
            end;
                
        %% It is an empty table
        AlreadyCreatedTable ->
            AlreadyCreatedTable
    end;

table(Server, QlcType, DocId, Meta, UserParams) ->
    DocRes = xapian_drv:document(Server, DocId),
    try
    table(Server, QlcType, DocRes, Meta, UserParams)
    after
        xapian_drv:release_resource(Server, DocRes)
    end.


empty_table() ->
    TraverseFun = fun() -> [] end,
    InfoFun = 
    fun(num_of_objects) -> 0;
       (_) -> undefined
       end,
    qlc:table(TraverseFun, [{info_fun, InfoFun}]).


init_not_empty_table(Server, Info, Meta, UserParams) ->
    #internal_qlc_info{
        num_of_objects = Size,
        resource_number = ResNum
    } = Info,
    KeyPos = xapian_term_record:key_position(Meta),
    From = proplists:get_value(from, UserParams, 0),
    Len = proplists:get_value(page_size, UserParams, 20),
    TraverseFun = traverse_fun(Server, ResNum, Meta, From, Len, Size),
    InfoFun = 
    fun(num_of_objects) -> Size;
       (keypos) -> KeyPos;
       (is_sorted_key) -> false;
       (is_unique_objects) -> true;
       (_) -> undefined
       end,
    LookupFun = lookup_fun(Server, ResNum, Meta, KeyPos),
    qlc:table(TraverseFun, 
            [{info_fun, InfoFun} 
            ,{lookup_fun, LookupFun}
            ,{key_equality,'=:='}]).
    

lookup_fun(Server, ResNum, Meta, KeyPos) ->
    fun(KeyPosI, Terms) when KeyPosI =:= KeyPos ->
        case lists:all(fun is_valid_term_name/1, Terms) of
        true ->
            Bin = xapian_drv:internal_qlc_lookup(Server, encoder(Terms), ResNum),
            {Records, <<>>} = xapian_term_record:decode_list2(Meta, Bin),
            Records;
        false ->
            erlang:error(bad_term_name)
        end
        end.


%% Maximum `Len' records can be retrieve for a call.
%% `From' records will be skipped from the beginning of the collection.
traverse_fun(Server, ResNum, Meta, From, Len, TotalLen) ->
    fun() ->
        Bin = xapian_drv:internal_qlc_get_next_portion(Server, ResNum, From, Len),
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


encoder(Terms) ->
    fun(Bin) -> 
        xapian_common:append_terms(Terms, Bin)
        end.


is_valid_term_name(_Term) -> true.
