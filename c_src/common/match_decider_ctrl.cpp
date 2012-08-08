#include <xapian.h>
#include "match_decider_ctrl.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class MatchDeciderControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::MatchDecider* mp_km;

    MatchDeciderControllerInternal(Xapian::MatchDecider* km) : mp_km(km)
    {
        m_reference_count = 1;
    }

    ~MatchDeciderControllerInternal()
    {
        delete mp_km;
    }

    Xapian::MatchDecider* getMatchDecider()
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
// MatchDeciderController
// -------------------------------------------------------------------

MatchDeciderController::MatchDeciderController(Xapian::MatchDecider* km)
{
    mp_internal = new MatchDeciderControllerInternal(km);
}


MatchDeciderController::MatchDeciderController(const MatchDeciderController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


MatchDeciderController::~MatchDeciderController()
{
    mp_internal->decref();
}


Xapian::MatchDecider* 
MatchDeciderController::getMatchDecider()
{
    return mp_internal->getMatchDecider();
}

XAPIAN_ERLANG_NS_END
