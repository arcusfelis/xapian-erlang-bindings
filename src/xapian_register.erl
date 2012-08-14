%% Internal store for metadata of resources.
-module(xapian_register).
-export([
    new/0,
    put/3,
    get/3,
    maybe_get/3,
    fetch/3,
    erase/3,
    delete/3]).

new() ->
    gb_trees:empty().

-record(context, {mon_ref, resources}).

put(Store, ClientPid, Elem) when is_pid(ClientPid) ->
    case gb_trees:lookup(ClientPid, Store) of
        {value, Con=#context{resources = Resources}} ->
            %% Each client process has his resources (Resources)
            %% and one monitor (MonRef).
            %%
            %% Each resource has its owner process and a ref (RefRef), 
            %% which the client gets.
            %%
            %% Elem is a some data.
            ResRef = make_ref(),
            %% Resources(ResRef -> Elem)  map(key -> value)
            NewResources = gb_trees:insert(ResRef, Elem, Resources),
            NewCon = Con#context{resources = NewResources},
            NewStore = gb_trees:update(ClientPid, NewCon, Store),
            {ok, {NewStore, ResRef}};
        none ->
            %% This resource is a first for this process.
            %% Create a monitor.
            MonRef = erlang:monitor(process, ClientPid),
            ResRef = make_ref(),
            NewResources = gb_trees:insert(ResRef, Elem, gb_trees:empty()),
            %% Init context.
            NewCon = #context{mon_ref = MonRef, resources = NewResources},
            NewStore = gb_trees:insert(ClientPid, NewCon, Store),
            {ok, {NewStore, ResRef}}
    end.


%% @doc Delete resource
delete(Store, ClientPid, ResRef)
    when is_pid(ClientPid), is_reference(ResRef) ->
    case gb_trees:lookup(ClientPid, Store) of
        {value, Con=#context{resources = Resources}} ->
            case gb_trees:lookup(ResRef, Resources) of
                {value, Elem} ->
                    %% Delete this resource from the context.
                    NewResources = gb_trees:delete(ResRef, Resources),
                    case gb_trees:is_empty(NewResources) of
                        true ->
                            %% It was a last resource in this context.
                            %% Delete this context.
                            erlang:demonitor(Con#context.mon_ref),
                            NewStore = gb_trees:delete(ClientPid, Store),
                            {ok, {NewStore, Elem}};
                        false ->
                            %% Replace contexts.
                            NewCon = Con#context{resources = NewResources},
                            NewStore = gb_trees:update(ClientPid, NewCon, Store),
                            {ok, {NewStore, Elem}}
                    end;
                none ->
                    {error, elem_not_found}
            end;
        none ->
            %% The resource is registered on other owner or already deleted
            %% (last for this context).
            {error, empty_context}
    end.
        

%% @doc Erase context: all resources, used by `ClientPid'.
%%
%% ClientPid is dead.
erase(Store, ClientPid, MonRef) 
    when is_pid(ClientPid), is_reference(MonRef) ->
    case gb_trees:lookup(ClientPid, Store) of
        {value, #context{mon_ref = MonRef, resources = Resources}} ->
            NewStore = gb_trees:delete(ClientPid, Store),
            %% MonRef is already demonitored.
            {ok, {NewStore, gb_trees:to_list(Resources)}};
        none ->
            {error, empty_context}
    end.


%% @doc Get info about the resource with ResRef, used by ClientPid.
get(Store, ClientPid, ResRef)
    when is_pid(ClientPid), is_reference(ResRef) ->
    case gb_trees:lookup(ClientPid, Store) of
        {value, #context{resources = Resources}} ->
            case gb_trees:lookup(ResRef, Resources) of
                {value, Elem} ->
                    {ok, Elem};
                false ->
                    {error, elem_not_found}
            end;
        none ->
            {error, empty_context}
    end.

maybe_get(_Store, _ClientPid, undefined) ->
    {ok, undefined};
maybe_get(Store, ClientPid, ResRef) ->
    get(Store, ClientPid, ResRef).


%% @doc Try get or throw an exception.
fetch(Store, ClientPid, ResRef) 
    when is_pid(ClientPid), is_reference(ResRef) ->
    #context{resources = Resources} = gb_trees:get(ClientPid, Store),
    gb_trees:get(ResRef, Resources).
