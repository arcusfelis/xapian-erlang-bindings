#include <xapian.h>
#include "val_range_ctrl.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class ValueRangeProcessorControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::ValueRangeProcessor* mp_km;

    ValueRangeProcessorControllerInternal(Xapian::ValueRangeProcessor* km) : mp_km(km)
    {
        m_reference_count = 1;
    }

    ~ValueRangeProcessorControllerInternal()
    {
        delete mp_km;
    }

    Xapian::ValueRangeProcessor* getValueRangeProcessor()
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
// ValueRangeProcessorController
// -------------------------------------------------------------------

ValueRangeProcessorController::ValueRangeProcessorController(Xapian::ValueRangeProcessor* km)
{
    mp_internal = new ValueRangeProcessorControllerInternal(km);
}


ValueRangeProcessorController::ValueRangeProcessorController(const ValueRangeProcessorController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


ValueRangeProcessorController::~ValueRangeProcessorController()
{
    mp_internal->decref();
}


Xapian::ValueRangeProcessor* 
ValueRangeProcessorController::getValueRangeProcessor()
{
    return mp_internal->getValueRangeProcessor();
}

XAPIAN_ERLANG_NS_END
