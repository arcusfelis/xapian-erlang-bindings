#include <xapian.h>
#include "stem_ctrl.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class StemControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::Stem* mp_km;

    StemControllerInternal(Xapian::Stem* km) : mp_km(km)
    {
        m_reference_count = 1;
    }

    ~StemControllerInternal()
    {
        delete mp_km;
    }

    Xapian::Stem* getStem()
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
// StemController
// -------------------------------------------------------------------

StemController::StemController(Xapian::Stem* km)
{
    mp_internal = new StemControllerInternal(km);
}


StemController::StemController(const StemController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


StemController::~StemController()
{
    mp_internal->decref();
}


Xapian::Stem* 
StemController::getStem()
{
    return mp_internal->getStem();
}

XAPIAN_ERLANG_NS_END
