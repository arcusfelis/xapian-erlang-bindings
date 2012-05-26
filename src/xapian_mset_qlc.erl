-module(xapian_mset_qlc).
-export([table/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("xapian.hrl").



table(Server, MSet, Meta) ->
    EncoderFun = fun(mset, DrvState, Bin) ->
        Name2Slot  = xapian_drv:name_to_slot(DrvState),
        Value2Type = xapian_drv:value_to_type(DrvState),
        xapian_record:encode(Meta, Name2Slot, Value2Type, Bin)
        end,
    #internal_qlc_info{
        num_of_objects = Size,
        resource_number = ResNum
    } = xapian_drv:internal_qlc_init(Server, mset, MSet, EncoderFun),
    From = 0,
    Len = 20,
    SubDbNames = xapian_drv:subdb_names(Server),
    DocIdPos      = xapian_record:key_position(Meta, docid),
    MultiDocIdPos = xapian_record:key_position(Meta, multi_docid),
    KeyPos = 
    case size(SubDbNames) of
         1 -> DocIdPos;
         _ -> 
            case MultiDocIdPos of
                undefined   -> DocIdPos;
                Pos         -> Pos
            end
    end,
    TraverseFun = traverse_fun(Server, ResNum, Meta, SubDbNames, From, Len, Size),
    InfoFun = 
    fun(num_of_objects) -> Size;
       (keypos) -> KeyPos;
       (is_sorted_key) -> false;
       (is_unique_objects) -> 
            case KeyPos of 
                docid       -> size(SubDbNames) =:= 1;
                multi_docid -> true;
                _           -> false
            end;
       (indices) -> [X || X <- [MultiDocIdPos, DocIdPos], X =/= undefined];
       (_) -> undefined
       end,
    LookupFun = lookup_fun(Server, ResNum, Meta, SubDbNames),
    qlc:table(TraverseFun, 
        [{info_fun, InfoFun} 
        ,{lookup_fun, LookupFun}
        ,{key_equality,'=:='}
        ]).


lookup_fun(Server, ResNum, Meta, SubDbNames) ->
    %% {name, fields, ...}
    Tuple = xapian_record:tuple(Meta),
    fun(KeyPos, DocIds) ->
        %% Key id is an atom (field name)
        KeyId = element(KeyPos, Tuple),
        case lists:all(fun is_valid_document_id/1, DocIds) of
        true ->
            Bin = xapian_drv:internal_qlc_lookup(
                Server, encoder(KeyId, DocIds), ResNum),
            {Records, <<>>} = xapian_record:decode_list2(Meta, SubDbNames, Bin),
            Records;
        false ->
            erlang:error(bad_docid)
        end
        end.


encoder(KeyId, DocIds) ->
    fun(Bin) -> 
        xapian_common:append_docids(DocIds, 
            xapian_record:append_key_field(KeyId, Bin))
        end.


is_valid_document_id(DocId) -> is_integer(DocId) andalso DocId > 0.


%% Maximum `Len' records can be retrieve for a call.
%% `From' records will be skipped from the beginning of the collection.
traverse_fun(Server, ResNum, Meta, SubDbNames, From, Len, TotalLen) ->
    fun() ->
        Bin = xapian_drv:internal_qlc_get_next_portion(Server, ResNum, 
                                                       From, Len),
        NextFrom = From+Len,
        MoreFun = traverse_fun(Server, ResNum, Meta, SubDbNames, NextFrom, 
                               Len, TotalLen),
        {Records, <<>>} = xapian_record:decode_list(Meta, SubDbNames, Bin),
        if
            NextFrom < TotalLen ->
                lists:reverse(lists:reverse(Records), MoreFun);
            true ->
                Records
        end
    end.
