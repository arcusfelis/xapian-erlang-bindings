
#define XAPIAN_ERLANG_NS XapianErlang 
#define XAPIAN_ERLANG_NS_BEGIN namespace XAPIAN_ERLANG_NS { 
#define XAPIAN_ERLANG_NS_END   }

#define XAPIAN_RESOURCE_NS_BEGIN XAPIAN_ERLANG_NS_BEGIN namespace Resource { 
#define XAPIAN_RESOURCE_NS_END   XAPIAN_ERLANG_NS_END }

#define XAPIAN_RESOURCE_CTRL_NS_BEGIN XAPIAN_RESOURCE_NS_BEGIN namespace Controller { 
#define XAPIAN_RESOURCE_CTRL_NS_END   XAPIAN_RESOURCE_NS_END }

#define XAPIAN_TERM_GEN_NS_BEGIN XAPIAN_ERLANG_NS_BEGIN namespace TermGenerator { 
#define XAPIAN_TERM_GEN_NS_END   XAPIAN_ERLANG_NS_END }

#define XAPIAN_DOC_GEN_NS_BEGIN XAPIAN_TERM_GEN_NS_BEGIN namespace Document { 
#define XAPIAN_DOC_GEN_NS_END   XAPIAN_TERM_GEN_NS_END }

#define XAPIAN_DB_GEN_NS_BEGIN XAPIAN_TERM_GEN_NS_BEGIN namespace Database { 
#define XAPIAN_DB_GEN_NS_END   XAPIAN_TERM_GEN_NS_END }

#define XAPIAN_QP_GEN_NS_BEGIN XAPIAN_TERM_GEN_NS_BEGIN namespace QueryParser { 
#define XAPIAN_QP_GEN_NS_END   XAPIAN_TERM_GEN_NS_END }

#define XAPIAN_VC_SPY_GEN_NS_BEGIN XAPIAN_TERM_GEN_NS_BEGIN namespace ValueCountMatchSpy { 
#define XAPIAN_VC_SPY_GEN_NS_END   XAPIAN_TERM_GEN_NS_END }

#define XAPIAN_EXT_NS_BEGIN XAPIAN_ERLANG_NS_BEGIN namespace Extension { 
#define XAPIAN_EXT_NS_END   XAPIAN_ERLANG_NS_END }

#define PR ParamDecoder& params, ResultEncoder& result 
#define CP Resource::Element& con, ParamDecoder& params
#define CPR Resource::Element& con, ParamDecoder& params, ResultEncoder& result

// Clone of params
#define PCR ParamDecoder params, ResultEncoder& result 
