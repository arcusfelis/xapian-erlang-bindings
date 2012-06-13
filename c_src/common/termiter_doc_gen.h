#ifndef XAPIAN_TERM_DOC_ITER_GEN_H
#define XAPIAN_TERM_DOC_ITER_GEN_H

#include <xapian.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

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
        begin();
        return m_doc.termlist_count();
    }
};

XAPIAN_ERLANG_NS_END
#endif
