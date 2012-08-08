#include <xapian.h>
#include "stopper_ctrl.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class StopperControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::Stopper* mp_km;

    StopperControllerInternal(Xapian::Stopper* km) : mp_km(km)
    {
        m_reference_count = 1;
    }

    ~StopperControllerInternal()
    {
        delete mp_km;
    }

    Xapian::Stopper* getStopper()
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
// StopperController
// -------------------------------------------------------------------

StopperController::StopperController(Xapian::Stopper* km)
{
    mp_internal = new StopperControllerInternal(km);
}


StopperController::StopperController(const StopperController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


StopperController::~StopperController()
{
    mp_internal->decref();
}


Xapian::Stopper* 
StopperController::getStopper()
{
    return mp_internal->getStopper();
}

XAPIAN_ERLANG_NS_END
