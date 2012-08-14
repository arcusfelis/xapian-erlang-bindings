#ifndef XAPIAN_TERM_QP_ITER_GEN_H
#define XAPIAN_TERM_QP_ITER_GEN_H

#include "termiter_gen.h"
#include <stdint.h>

#include "xapian_config.h"
XAPIAN_QP_GEN_NS_BEGIN

class QueryParser : public Iterator
{
    protected:
    Xapian::QueryParser* mp_qp;

    public:
    QueryParser(Xapian::QueryParser& qp) 
    {
        mp_qp = &qp;
    }
};


class Unstem : public QueryParser
{
    const std::string m_term;

    public:
    Unstem(
        Xapian::QueryParser& qp, std::string term) 
            : QueryParser(qp), m_term(term) {}

    Xapian::TermIterator begin()
    {
        return mp_qp->unstem_begin(m_term);
    }

    Xapian::TermIterator end()
    {
        return mp_qp->unstem_end(m_term);
    }
};


class StopList : public QueryParser
{
    public:
    StopList(Xapian::QueryParser& qp) 
            : QueryParser(qp) {}

    Xapian::TermIterator begin()
    {
        return mp_qp->stoplist_begin();
    }

    Xapian::TermIterator end()
    {
        return mp_qp->stoplist_end();
    }
};

XAPIAN_QP_GEN_NS_END

#endif
