-module(xapian_encode).
-export([ append_stemmer/2
        , append_prefix/2]).
-include_lib("xapian/include/xapian.hrl").
-compile({parse_transform, seqbind}).
-import(xapian_common, [ 
    append_iolist/2,
    append_boolean/2]).

append_stemmer(#x_stemmer{language=Language}, Bin) ->
    append_iolist(maybe_atom(Language), Bin).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(HELP, ?MODULE).

maybe_atom_test_() ->
    [?_assertEqual(maybe_atom("list"), "list")
    ,?_assertEqual(maybe_atom('atom'), "atom")
    ].

-endif.


%% Prefix must be handled with `xapian_check:check_prefix/1'.
%% See `XapianErlangDriver::addPrefix'
append_prefix(#x_prefix_name{name=Name, prefix=Prefix, 
    is_boolean=Bool, is_exclusive=Ex}, Bin@) ->
    Bin@ = append_iolist(prefix_name_to_binary(Name), Bin@),
    Bin@ = append_iolist(Prefix, Bin@),
    Bin@ = append_boolean(Bool, Bin@),
    Bin@ = append_boolean(Ex, Bin@),
    Bin@.


prefix_name_to_binary(A) when is_atom(A) ->
    xapian_common:string_to_binary(atom_to_list(A));

prefix_name_to_binary(X) ->
    X.

maybe_atom(X) when is_atom(X) -> atom_to_list(X);
maybe_atom(X) -> X.

