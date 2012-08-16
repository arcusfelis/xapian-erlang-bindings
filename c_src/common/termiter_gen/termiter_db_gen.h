#ifndef XAPIAN_TERM_DB_ITER_GEN_H
#define XAPIAN_TERM_DB_ITER_GEN_H

#include "termiter_gen.h"
#include <stdint.h>

#include "xapian_config.h"
XAPIAN_DB_GEN_NS_BEGIN

class Database : public Iterator
{
    protected:
    Xapian::Database m_db;

    public:
    Database(Xapian::Database& db) : m_db(db)
    {}
};


class Spellings : public Database
{
    public:
    Spellings(Xapian::Database& db) 
            : Database(db)
    {}

    Xapian::TermIterator begin()
    {
        return m_db.spellings_begin();
    }

    Xapian::TermIterator end()
    {
        return m_db.spellings_end();
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
        return m_db.synonyms_begin(m_term);
    }

    Xapian::TermIterator end()
    {
        return m_db.synonyms_end(m_term);
    }
};


class SynonymKeys : public Database
{
    const std::string m_prefix;
    public:
    SynonymKeys(Xapian::Database& db, const std::string& prefix) 
            : Database(db), m_prefix(prefix)
    {}

    Xapian::TermIterator begin()
    {
        return m_db.synonym_keys_begin(m_prefix);
    }

    Xapian::TermIterator end()
    {
        return m_db.synonym_keys_end(m_prefix);
    }
};


class MetadataKeys : public Database
{
    const std::string m_prefix;
    public:
    MetadataKeys(Xapian::Database& db, const std::string& prefix) 
            : Database(db), m_prefix(prefix)
    {}

    Xapian::TermIterator begin()
    {
        return m_db.metadata_keys_begin(m_prefix);
    }

    Xapian::TermIterator end()
    {
        return m_db.metadata_keys_end(m_prefix);
    }
};


class AllTerms : public Database
{
    const std::string m_prefix;
    public:
    AllTerms(Xapian::Database& db, const std::string& prefix) 
            : Database(db), m_prefix(prefix)
    {}

    Xapian::TermIterator begin()
    {
        return m_db.allterms_begin(m_prefix);
    }

    Xapian::TermIterator end()
    {
        return m_db.allterms_end(m_prefix);
    }
};


XAPIAN_DB_GEN_NS_END

#endif
