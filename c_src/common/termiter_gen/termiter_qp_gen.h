#ifndef XAPIAN_TERM_QP_ITER_GEN_H
#define XAPIAN_TERM_QP_ITER_GEN_H

#include "termiter_gen.h"
#include <stdint.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class QueryParserIteratorGenerator : public TermIteratorGenerator
{
    protected:
    Xapian::QueryParser* mp_qp;

    public:
    QueryParserIteratorGenerator(Xapian::QueryParser& qp) 
    {
        mp_qp = &qp;
    }
};


class UnstemQueryParserIteratorGenerator : public QueryParserIteratorGenerator
{
    const std::string m_term;

    public:
    UnstemQueryParserIteratorGenerator(
        Xapian::QueryParser& qp, std::string term) 
            : QueryParserIteratorGenerator(qp), m_term(term) {}

    Xapian::TermIterator begin()
    {
        return mp_qp->unstem_begin(m_term);
    }

    Xapian::TermIterator end()
    {
        return mp_qp->unstem_end(m_term);
    }
};


class StopListQueryParserIteratorGenerator : public QueryParserIteratorGenerator
{
    public:
    StopListQueryParserIteratorGenerator(Xapian::QueryParser& qp) 
            : QueryParserIteratorGenerator(qp) {}

    Xapian::TermIterator begin()
    {
        return mp_qp->stoplist_begin();
    }

    Xapian::TermIterator end()
    {
        return mp_qp->stoplist_end();
    }
};

XAPIAN_ERLANG_NS_END

#endif
