%% Internal store for metadata of resources.
-module(xapian_register).
-export([
    new/0,
    put/3,
    get/2,
    fetch/2,
    erase/2]).

new() ->
    gb_trees:empty().


put(Store, ClientPid, Elem) ->
    Ref = erlang:monitor(process, ClientPid),
    NewStore = gb_trees:insert(Ref, Elem, Store),
    {ok, {NewStore, Ref}}.


erase(Store, Ref) ->
    erlang:demonitor(Ref),
    case gb_trees:is_defined(Ref, Store) of
    true ->
        Elem = gb_trees:get(Ref, Store),
        NewStore = gb_trees:delete(Ref, Store),
        {ok, {NewStore, Elem}};
    false ->
        {error, elem_not_found}
    end.
        

get(Store, Ref) ->
    case gb_trees:is_defined(Ref, Store) of
    true ->
        Elem = gb_trees:get(Ref, Store),
        {ok, Elem};
    false ->
        {error, elem_not_found}
    end.


%% Try get or throw an exception.
fetch(Store, Ref) ->
    case gb_trees:is_defined(Ref, Store) of
    true ->
        gb_trees:get(Ref, Store);
    false ->
        erlang:error(elem_not_found)
    end.
