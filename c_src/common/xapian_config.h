
#define XAPIAN_ERLANG_NS XapianErlang 
#define XAPIAN_ERLANG_NS_BEGIN namespace XAPIAN_ERLANG_NS { 
#define XAPIAN_ERLANG_NS_END   }
#define PR ParamDecoder& params, ResultEncoder& result 
#define CP XapianContext& con, ParamDecoder& params
#define CPR XapianContext& con, ParamDecoder& params, ResultEncoder& result

// Clone of params
#define PCR ParamDecoder params, ResultEncoder& result 
