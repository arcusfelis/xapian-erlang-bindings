#ifndef XAPIAN_TERM_SPY_ITER_GEN_H
#define XAPIAN_TERM_SPY_ITER_GEN_H

#include "termiter_gen.h"
#include "spy_ctrl.h"

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

#endif
