-module(xapian_term_qlc).
-export([table/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("xapian.hrl").



table(Server, DocRes, Meta) when is_reference(DocRes) ->
    QlcParams = 
    #internal_qlc_term_parameters{ record_info = Meta },
    #internal_qlc_info{
        num_of_objects = Size,
        resource_number = ResNum,
        resource_ref = QlcRes
    } = xapian_drv:internal_qlc_init(Server, terms, DocRes, QlcParams),

    KeyPos = xapian_term_record:key_position(Meta),
    From = 0,
    Len = 20,
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
            ,{key_equality,'=:='}]);

table(Server, DocId, Meta) ->
    DocRes = xapian_drv:document(Server, DocId),
    try
    table(Server, DocRes, Meta)
    after
        xapian_drv:release_resource(Server, DocRes)
    end.
    

lookup_fun(Server, ResNum, Meta, KeyPos) ->
    fun(KeyPosI, Terms) when KeyPosI =:= KeyPos ->
        case lists:all(fun is_valid_term_name/1, Terms) of
        true ->
            Bin = xapian_drv:internal_qlc_lookup(Server, encoder(Terms), ResNum),
            {Records, <<>>} = xapian_term_record:decode_list(Meta, Bin),
            Records;
        false ->
            erlang:error(bad_term_name)
        end
        end.


encoder(Terms) ->
    fun(Bin) -> 
        xapian_common:append_terms(Terms, Bin)
        end.


is_valid_term_name(_Term) -> true.


%% Maximum `Len' records can be retrieve for a call.
%% `From' records will be skipped from the beginning of the collection.
traverse_fun(Server, ResNum, Meta, From, Len, TotalLen) ->
    fun() ->
        Bin = xapian_drv:internal_qlc_get_next_portion(Server, ResNum, From, Len),
        NextFrom = From+Len,
        MoreFun = traverse_fun(Server, ResNum, Meta, NextFrom, Len, TotalLen),
        {Records, <<>>} = xapian_term_record:decode_list(Meta, Bin),
        if
            NextFrom < TotalLen ->
                lists:reverse(lists:reverse(Records), MoreFun);
            true ->
                Records
        end
    end.
