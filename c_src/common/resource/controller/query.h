#ifndef QUERY_RCTRL_H
#define QUERY_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Query : public Base
{
    Xapian::Query* mp_qp;

    public:
    Query(Xapian::Query* p_qp) : mp_qp(p_qp) {}
    ~Query() { delete mp_qp; }

    virtual operator Xapian::Query&()
    {
        return *mp_qp;
    }

    std::string type()
    {
        return "Resource::Query";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
