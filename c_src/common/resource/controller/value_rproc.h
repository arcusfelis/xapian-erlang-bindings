#ifndef VALUE_RANGE_PROCESSOR_RCTRL_H
#define VALUE_RANGE_PROCESSOR_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class ValueRangeProcessor : public Base
{
    Xapian::ValueRangeProcessor* mp_proc;

    public:
    ValueRangeProcessor(Xapian::ValueRangeProcessor* p_proc) : mp_proc(p_proc) {}
    ~ValueRangeProcessor() { delete mp_proc; }

    virtual operator Xapian::ValueRangeProcessor&()
    {
        return *mp_proc;
    }

    std::string type()
    {
        return "Resource::ValueRangeProcessor";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
