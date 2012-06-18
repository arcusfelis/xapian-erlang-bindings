-module(xapian_error).

%% Export for other modules of this application.
-export([bad_slot_value_type_error/4]).

-include_lib("xapian/include/xapian.hrl").
-record(bad_slot_value_error_info, {slot, type, value}).

%% Value in the slot has bad type.
bad_slot_value_type_error(ErrorType, Slot, Value, ExpectedType) ->
    ErrorReason = #bad_slot_value_error_info{slot=Slot, 
                                             type=ExpectedType, value=Value},
    erlang:error(#x_error{type=ErrorType, reason=ErrorReason}).
