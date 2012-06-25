%% @doc Usuful miniprograms.
-module(xapian_helper).
-export([stem/3]).

-include_lib("xapian/include/xapian.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(term, {value, positions, wdf}).


stem(Server, Lang, Str) ->
    CD = [ #x_stemmer{language = Lang}, #x_text{value = Str} ],
    RD = xapian_server:document(Server, CD),
    Meta = xapian_term_record:record(term, record_info(fields, term)),
    Table = xapian_term_qlc:document_term_table(Server, RD, Meta),
    Records = qlc:e(qlc:q([X || X <- Table])),
    [ #x_term{value = V, position = P, frequency = F} 
     || #term{value = V, positions = P, wdf = F} <- Records].


