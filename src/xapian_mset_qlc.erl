-module(xapian_mset_qlc).
-export([table/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("xapian.hrl").



table(Server, MSet, Meta) ->
    QlcParams = 
    #internal_qlc_mset_parameters{ record_info = Meta },
    #internal_qlc_info{
        num_of_objects = Size,
        resource_number = ResNum
    } = xapian_drv:internal_qlc_init(Server, MSet, QlcParams),
    KeyPos = xapian_record:key_position(Meta),
    From = 0,
    Len = 20,
    TraverseFun = traverse(Server, ResNum, Meta, From, Len, Size),
    InfoFun = 
    fun(num_of_objects) -> Size;
       (keypos) -> KeyPos;
       (is_sorted_key) -> false;
       (is_unique_objects) -> true;
       (_) -> undefined
       end,
%   LookupFun =
%   fun(1, Ks) ->
%       lists:flatmap(fun(K) ->
%           case gb_trees:lookup(K, T) of
%               {value, V} -> [{K,V}];
%               none -> []
%           end
%       end, Ks)
%   end,
    qlc:table(TraverseFun, 
        [{info_fun, InfoFun} 
%       ,{lookup_fun, LookupFun}
        ,{key_equality,'=:='}]).


%% Maximum `Len' records can be retrieve for a call.
%% `From' records will be skipped from the beginning of the collection.
traverse(Server, ResNum, Meta, From, Len, TotalLen) ->
    fun() ->
        Bin = xapian_drv:internal_qlc_get_next_portion(Server, ResNum, From, Len),
        NextFrom = From+Len,
        MoreFun = traverse(Server, ResNum, Meta, NextFrom, Len, TotalLen),
        {Records, <<>>} = xapian_record:decode_list(Meta, Bin),
        if
            NextFrom < TotalLen ->
                lists:reverse(lists:reverse(Records), MoreFun);
            true ->
                Records
        end
    end.
