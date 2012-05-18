#include "termiter_gen.h"
#include "xapian_exception.h"

// hidden
class SpyValueIteratorGenerator : public TermIteratorGenerator
{
    protected:
    SpyController m_controller;
    Xapian::ValueCountMatchSpy* mp_spy;

    public:
    SpyValueIteratorGenerator(SpyController& controller) 
        : m_controller(controller)
    {
        mp_spy = 
        static_cast<Xapian::ValueCountMatchSpy*>(m_controller.getSpy());
    }

    Xapian::TermIterator begin()
    {
        return mp_spy->values_begin();
    }

    Xapian::TermIterator end()
    {
        return mp_spy->values_end();
    }
};


// hidden
class TopSpyValueIteratorGenerator : public SpyValueIteratorGenerator
{
    const uint32_t m_maxvalues;

    public:
    TopSpyValueIteratorGenerator(
        SpyController& controller, uint32_t maxvalues) 
            : SpyValueIteratorGenerator(controller), m_maxvalues(maxvalues)
    {}

    Xapian::TermIterator begin()
    {
        return mp_spy->top_values_begin(m_maxvalues);
    }

    Xapian::TermIterator end()
    {
        return mp_spy->top_values_end(m_maxvalues);
    }
};


// hidden
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
SpyController::getValueIteratorGenerator()
{
    throw BadArgumentDriverError();
}


TermIteratorGenerator*
SpyController::getTopValueIteratorGenerator(uint32_t /*maxvalues*/)
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

// -------------------------------------------------------------------
// ValueCountSpyController
// -------------------------------------------------------------------

ValueCountSpyController::ValueCountSpyController(
    Xapian::ValueCountMatchSpy* spy) 
        : SpyController(spy) {}


TermIteratorGenerator*
ValueCountSpyController::getValueIteratorGenerator()
{
    return new SpyValueIteratorGenerator(*this);
}


TermIteratorGenerator*
ValueCountSpyController::getTopValueIteratorGenerator(uint32_t maxvalues)
{
    return new TopSpyValueIteratorGenerator(*this, maxvalues);
}

