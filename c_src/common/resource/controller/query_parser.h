#ifndef QUERY_PARSER_RCTRL_H
#define QUERY_PARSER_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class QueryParser : public Base
{
    Xapian::QueryParser* mp_qp;

    public:
    QueryParser(Xapian::QueryParser* p_qp) : mp_qp(p_qp) {}
    ~QueryParser() { delete mp_qp; }

    virtual operator Xapian::QueryParser&()
    {
        return *mp_qp;
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
