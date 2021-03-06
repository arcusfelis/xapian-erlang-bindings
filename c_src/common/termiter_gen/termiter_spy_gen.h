#ifndef XAPIAN_TERM_SPY_ITER_GEN_H
#define XAPIAN_TERM_SPY_ITER_GEN_H

#include "termiter_gen.h"
#include <stdint.h>

#include "xapian_config.h"
XAPIAN_VC_SPY_GEN_NS_BEGIN

class Values : public Iterator
{
    protected:
    Xapian::ValueCountMatchSpy* mp_spy;

    public:
    Values(Xapian::ValueCountMatchSpy& spy) 
    {
        mp_spy = &spy;
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


class TopValues : public Values
{
    const uint32_t m_maxvalues;

    public:
    TopValues(
        Xapian::ValueCountMatchSpy& spy, uint32_t maxvalues) 
            : Values(spy), m_maxvalues(maxvalues)
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

XAPIAN_VC_SPY_GEN_NS_END

#endif
