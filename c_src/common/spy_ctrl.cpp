#include <xapian.h>
#include "termiter_gen.h"
#include "termiter_spy_gen.h"
#include "xapian_exception.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class SpyControllerInternal
{
    public:
    uint32_t _refs;
    Xapian::MatchSpy* mp_spy;
    bool m_is_finalized;

    SpyControllerInternal(Xapian::MatchSpy* spy) : mp_spy(spy)
    {
        _refs = 1;
        m_is_finalized = false;
    }

    virtual
    ~SpyControllerInternal()
    {
        delete mp_spy;
    }

    Xapian::MatchSpy* getSpy()
    {
        return mp_spy;
    }

    void finalize()
    {
        m_is_finalized = true;
    }

    bool is_finalized()
    {
        return m_is_finalized;
    }
};


// -------------------------------------------------------------------
// SpyController
// -------------------------------------------------------------------

void 
SpyController::decref()
{
    if (--mp_internal->_refs == 0)
        delete mp_internal;
}


SpyController::SpyController(Xapian::MatchSpy* spy)
{
    mp_internal = new SpyControllerInternal(spy);
}


SpyController::SpyController(const SpyController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->_refs++;
}


SpyController::SpyController(SpyControllerInternal* src)
{
    mp_internal = src;
    mp_internal->_refs++;
}


SpyController::~SpyController()
{
    decref();
}


Xapian::MatchSpy* 
SpyController::getSpy()
{
    return mp_internal->getSpy();
}


TermIteratorGenerator*
SpyController::getIteratorGenerator(ParamDecoder& /*params*/)
{
    throw BadArgumentDriverError();
}


void SpyController::finalize()
{
    mp_internal->finalize();
}

bool SpyController::is_finalized()
{
    return mp_internal->is_finalized();
}

XAPIAN_ERLANG_NS_END
