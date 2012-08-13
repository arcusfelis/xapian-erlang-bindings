#ifndef TERM_GENERATOR_FACTORY_H
#define TERM_GENERATOR_FACTORY_H

#include <xapian.h>
#include <vector>
#include <string>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN


/**
 * It is here, because TermGenerator cannot be cloned.
 * The same factory for TermGenerator.
 */
class TermGeneratorFactory
{
    Xapian::WritableDatabase m_wdb;
    Xapian::Stem m_stemmer;

    public:
    void set_stemmer (const Xapian::Stem &stemmer)
    {
        m_stemmer = stemmer;
    }

    void set_database (const Xapian::WritableDatabase &wdb)
    {
        m_wdb = wdb;
    }

    operator Xapian::TermGenerator()
    {
        Xapian::TermGenerator tg;
        tg.set_database(m_wdb);
        tg.set_stemmer(m_stemmer);
        return tg;
    }
};

XAPIAN_ERLANG_NS_END

#endif
