-module(xapian_dsl).
-export([database/2,
         prog_database/3,
         tcp_database/3,
         stemmer/1,
         data/1,
         term/1,
         term/2,
         term/3,
         term/4,
         value/2,
         value/3,
         delta/1,
         text/1,
         prefix_name/2,
         value_name/2,
         x_query/1,
         x_query/2,
         x_query/3]).
-include_lib("xapian/include/xapian.hrl").

database(Name, Path) ->
    #x_database{name=Name, path=Path}.

prog_database(Name, Program, Arguments) ->
    #x_prog_database{name=Name, program=Program, arguments=Arguments}.

tcp_database(Name, Host, Port) ->
    #x_tcp_database{name=Name, host=Host, port=Port}.

stemmer(Language) ->
    #x_stemmer{language=Language}.

data(Value) ->
    #x_data{value=Value}.

term(Value) ->
    #x_term{value=Value}.

term(Value, Position) ->
    #x_term{value=Value, position=Position}.

term(Value, Position, Frequency) ->
    #x_term{value=Value, position=Position, frequency=Frequency}.

term(Value, Position, Frequency, Action) when is_atom(Action) ->
    #x_term{value=Value, position=Position, frequency=Frequency, action=Action}.

value(Slot, Value) ->
    #x_value{slot=Slot, value=Value}.

value(Slot, Value, Action) when is_atom(Action) ->
    #x_value{slot=Slot, value=Value, action=Action}.

delta(Value) when is_integer(Value) ->
    #x_delta{position=Value}.

text(Value) ->
    #x_text{value=Value}.

prefix_name(Name, Prefix) ->
    #x_prefix_name{name=Name, prefix=Prefix}.

value_name(Name, Slot) when is_atom(Name), is_integer(Slot) ->
    #x_value_name{name=Name, slot=Slot}.

x_query(Value) ->
    #x_query{value=Value}.

x_query(Op, Value) when is_atom(Op) ->
    #x_query{op=Op, value=Value}.

x_query(Op, Value, Param) when is_atom(Op) ->
    #x_query{op=Op, value=Value, parameter=Param}.


