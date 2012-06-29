#ifndef QUERY_PARSER_FACTORY_H
#define QUERY_PARSER_FACTORY_H

#include <xapian.h>
#include <vector>
#include <string>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class PrefixInternal
{
    std::string m_field; 
    std::string m_prefix;
    bool m_is_boolean;
    bool m_is_exclusive;

    public:

    PrefixInternal(
            const std::string& field, 
            const std::string& prefix) 
        : m_field(field), m_prefix(prefix), 
          m_is_boolean(false), m_is_exclusive(true) // exclusive is unknown.
    {}

    PrefixInternal(
            const std::string& field, 
            const std::string& prefix,
            bool is_exclusive) 
        : m_field(field), m_prefix(prefix), 
        m_is_boolean(true), m_is_exclusive(is_exclusive)
    {}

    void fill(Xapian::QueryParser& qp)
    {
        if (m_is_boolean)
            qp.add_boolean_prefix(m_field, m_prefix, m_is_exclusive);
        else
            qp.add_prefix(m_field, m_prefix);
    }
};

class QueryParserFactory
{
    Xapian::Database m_db;
    Xapian::Stem m_stemmer;
    std::vector< PrefixInternal > m_prefixes;

    public:
    void set_stemmer (const Xapian::Stem &stemmer)
    {
        m_stemmer = stemmer;
    }

    void set_database (const Xapian::Database &db)
    {
        m_db = db;
    }

    void add_prefix (const std::string &field, const std::string &prefix)
    {
        PrefixInternal p(field, prefix);
        m_prefixes.push_back(p);
    }

    void add_boolean_prefix (const std::string &field, 
            const std::string &prefix, bool exclusive)
    {
        PrefixInternal p(field, prefix, exclusive);
        m_prefixes.push_back(p);
    }

    operator Xapian::QueryParser()
    {
        Xapian::QueryParser qp;
        qp.set_database(m_db);
        qp.set_stemmer(m_stemmer);

        std::vector< PrefixInternal >::iterator 
            piter = m_prefixes.begin(),
            pend  = m_prefixes.end();
        while (piter != pend)
        {
            piter->fill(qp);
            piter++;
        }
        return qp;
    }
};

XAPIAN_ERLANG_NS_END

#endif
