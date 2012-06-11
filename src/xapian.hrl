-record(resource, {
    type :: atom(), 
    number :: non_neg_integer()
}).

-record(internal_qlc_info, {
    num_of_objects :: non_neg_integer(),
    resource_number :: non_neg_integer(),
    resource_ref :: reference()
}).
