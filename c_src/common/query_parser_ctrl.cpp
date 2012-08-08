#include <xapian.h>
#include "query_parser_ctrl.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class QueryParserControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::QueryParser* mp_km;

    QueryParserControllerInternal(Xapian::QueryParser* km) : mp_km(km)
    {
        m_reference_count = 1;
    }

    ~QueryParserControllerInternal()
    {
        delete mp_km;
    }

    Xapian::QueryParser* getQueryParser()
    {
        return mp_km;
    }

    void decref()
    {
        m_reference_count--;
        if (m_reference_count == 0)
            delete this;
    }

    void incref()
    {
        m_reference_count++;
    }
};


// -------------------------------------------------------------------
// QueryParserController
// -------------------------------------------------------------------

QueryParserController::QueryParserController(Xapian::QueryParser* km)
{
    mp_internal = new QueryParserControllerInternal(km);
}


QueryParserController::QueryParserController(const QueryParserController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


QueryParserController::~QueryParserController()
{
    mp_internal->decref();
}


Xapian::QueryParser* 
QueryParserController::getQueryParser()
{
    return mp_internal->getQueryParser();
}

XAPIAN_ERLANG_NS_END
