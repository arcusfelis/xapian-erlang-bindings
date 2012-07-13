#ifndef XAPIAN_TERM_DOC_ITER_GEN_H
#define XAPIAN_TERM_DOC_ITER_GEN_H

#include <xapian.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/**
 * Generate @a TermIterator from `Xapian::Document`.
 */
class DocumentTermIteratorGenerator : public TermIteratorGenerator
{
    Xapian::Document m_doc;

    public:
    DocumentTermIteratorGenerator(Xapian::Document& doc) : m_doc(doc)
    {}

    Xapian::TermIterator begin()
    {
        return m_doc.termlist_begin();
    }

    Xapian::TermIterator end()
    {
        return m_doc.termlist_end();
    }

    Xapian::termcount size()
    {
        // FIXME: it is a great hack (yet another).
        // see http://trac.xapian.org/ticket/423

        // It does the same, but it has a bug.
        //return m_doc.termlist_count();
        Xapian::termcount i = 0;
        for (Xapian::TermIterator e = end(), b = begin(); b != e; b++, i++);
        return i;
    }
};

XAPIAN_ERLANG_NS_END
#endif
