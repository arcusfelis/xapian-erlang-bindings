#ifndef XAPIAN_TERM_ITER_GEN_H
#define XAPIAN_TERM_ITER_GEN_H

#include <xapian.h>

/**
 * Contains few functions for iterator creation and retrieving information.
 *
 * It provides an interface for term iteration. 
 * Each object contains state with a term set.
 *
 * Used from @ref XapianErlangDriver::termGenerator for QLC iterators.
 */
class TermIteratorGenerator
{
    public:
    virtual Xapian::TermIterator begin() = 0; 
    virtual Xapian::TermIterator end() = 0; 
    virtual ~TermIteratorGenerator() {}

    /** 
     * If @ref size() is unknown, return 0.
     * To check size is really 0, use @ref empty().
     */
    virtual Xapian::termcount size()
    {
        return 0;
    }
    virtual bool empty()
    {
        return begin() == end();
    }
};

#endif
