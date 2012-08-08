#ifndef XAPIAN_QUERY_PARSER_H
#define XAPIAN_QUERY_PARSER_H

#include <stdint.h>
#include <xapian.h>

namespace Xapian
{
class QueryParser;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class QueryParserControllerInternal;

class ParamDecoder;

class QueryParserController 
{
    QueryParserControllerInternal* mp_internal;

    public:
    QueryParserController(Xapian::QueryParser* km);

    /// Copy is allowed
    QueryParserController(const QueryParserController& src);

    ~QueryParserController();

    Xapian::QueryParser* getQueryParser();
};

XAPIAN_ERLANG_NS_END

#endif
