#include <xapian.h>
#include "expand_decider_ctrl.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class ExpandDeciderControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::ExpandDecider* mp_km;

    ExpandDeciderControllerInternal(Xapian::ExpandDecider* km) : mp_km(km)
    {
        m_reference_count = 1;
    }

    ~ExpandDeciderControllerInternal()
    {
        delete mp_km;
    }

    Xapian::ExpandDecider* getExpandDecider()
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
// ExpandDeciderController
// -------------------------------------------------------------------

ExpandDeciderController::ExpandDeciderController(Xapian::ExpandDecider* km)
{
    mp_internal = new ExpandDeciderControllerInternal(km);
}


ExpandDeciderController::ExpandDeciderController(const ExpandDeciderController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


ExpandDeciderController::~ExpandDeciderController()
{
    mp_internal->decref();
}


Xapian::ExpandDecider* 
ExpandDeciderController::getExpandDecider()
{
    return mp_internal->getExpandDecider();
}

XAPIAN_ERLANG_NS_END
