#ifndef XAPIAN_TERM_DB_ITER_GEN_H
#define XAPIAN_TERM_DB_ITER_GEN_H

#include "termiter_gen.h"
#include <stdint.h>

#include "xapian_config.h"
XAPIAN_DB_GEN_NS_BEGIN

class Database : public Iterator
{
    protected:
    Xapian::Database* mp_db;

    public:
    Database(Xapian::Database& db) 
    {
        mp_db = &db;
    }
};


class Spellings : public Database
{
    public:
    Spellings(Xapian::Database& db) 
            : Database(db)
    {}

    Xapian::TermIterator begin()
    {
        return mp_db->spellings_begin();
    }

    Xapian::TermIterator end()
    {
        return mp_db->spellings_end();
    }
};

class Synonyms : public Database
{
    const std::string m_term;
    public:
    Synonyms(Xapian::Database& db, const std::string& term) 
            : Database(db), m_term(term)
    {}

    Xapian::TermIterator begin()
    {
        return mp_db->synonyms_begin(m_term);
    }

    Xapian::TermIterator end()
    {
        return mp_db->synonyms_end(m_term);
    }
};


XAPIAN_DB_GEN_NS_END

#endif
