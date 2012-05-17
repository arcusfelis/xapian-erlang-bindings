-record(internal_qlc_info, {
    num_of_objects :: non_neg_integer(),
    resource_number :: non_neg_integer(),
    resource_ref :: reference()
}).

-record(internal_qlc_mset_parameters, {
    record_info %% see function xapian_record:record
}).

-record(internal_qlc_term_parameters, {
    record_info, %% see function xapian_term_record:record
    user_parameters
}).

-record(resource, {
    type :: atom(), 
    number :: non_neg_integer()
}).
