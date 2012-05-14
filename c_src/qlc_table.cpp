#include "qlc_table.h"
#include "xapian_core.h"
#include <assert.h>
#include <vector>

// ------------------------------------------------------------------
// QlcTable
// ------------------------------------------------------------------

QlcTable::QlcTable(XapianErlangDriver& driver) 
    : m_driver(driver) {}


QlcTable::~QlcTable()
{}


// ------------------------------------------------------------------
// MSetQlcTable
// ------------------------------------------------------------------

MSetQlcTable::MSetQlcTable(XapianErlangDriver& driver, 
    Xapian::MSet& mset, const ParamDecoderController& controller) 
    : QlcTable(driver), m_mset(mset), m_controller(controller)
{}


uint32_t 
MSetQlcTable::numOfObjects()
{
    return static_cast<uint32_t>(m_mset.size());
}


/**
 * Skip "skip" documents. 
 * Read not more then "count" documents.
 */
void
MSetQlcTable::getPage(const uint32_t skip, const uint32_t count)
{
    uint32_t size = m_mset.size();
    assert(skip <= size);

    /* Tail size */
    size -= skip;

    // Do while: 
    // * the current element is not the last element;
    // * and not more then wanted count of documents were extracted.
    const uint32_t left = std::min(size, count);

    // Put count of elements
    m_driver.m_result << left;

    Xapian::MSetIterator iter = m_mset[skip];
    Xapian::MSetIterator last = (count < size) ? m_mset[skip+left+1] : m_mset.end();

    for (; iter != last; iter++)
    {
        ParamDecoder params = m_controller;
        Xapian::Document doc = iter.get_document();
        m_driver.retrieveDocument(params, doc, &iter);
    }
}


void
MSetQlcTable::lookup(ParamDecoder& driver_params)
{
    std::vector< Xapian::MSetIterator > matched;

    while (const Xapian::docid docid = driver_params)
    for (Xapian::MSetIterator iter = m_mset.begin(); iter != m_mset.end(); iter++)
    {
        Xapian::docid current_docid = *iter;
        if (current_docid == docid)
        {
            matched.push_back(iter);
            break;
        }
    }

    const uint32_t size = matched.size();

    // Put count of elements
    m_driver.m_result << size;

    for (std::vector< Xapian::MSetIterator >::iterator 
        match_iter = matched.begin(); 
        match_iter != matched.end(); 
        match_iter++)
    {
        Xapian::MSetIterator& iter = *match_iter;
        ParamDecoder params = m_controller;
        Xapian::Document doc = iter.get_document();
        m_driver.retrieveDocument(params, doc, &iter);
    }
}





// ------------------------------------------------------------------
// TermTable
// ------------------------------------------------------------------


TermQlcTable::TermQlcTable(XapianErlangDriver& driver, 
        Xapian::Document& doc, const ParamDecoderController& controller) 
    : QlcTable(driver), m_doc(doc), m_controller(controller)
{
    m_iter = m_doc.termlist_begin();
    m_current_pos = 0;
}


uint32_t 
TermQlcTable::numOfObjects()
{
    return static_cast<uint32_t>(m_doc.termlist_count());
}


/**
 * Skip "skip" documents. 
 * Read not more then "count" documents.
 */
void
TermQlcTable::getPage(const uint32_t skip, const uint32_t count)
{
    uint32_t size = m_doc.termlist_count();
    assert(skip <= size);

    /* Tail size */
    size -= skip;

    // Do while: 
    // * the current element is not the last element;
    // * and not more then wanted count of documents were extracted.
    const uint32_t left = std::min(size, count);

    // Put count of elements
    m_driver.m_result << left;

    // Skip first elements
    if (skip != m_current_pos)
    {
        m_iter = m_doc.termlist_begin();
        for (uint32_t i = left; i; i--)
            m_iter++;
    }

    // While left > 0 and term is not last.
    for (uint32_t i = left; i && (m_iter != m_doc.termlist_end()); m_iter++, i--)
    {
        ParamDecoder params = m_controller;
        m_driver.retrieveTerm(params, m_iter);
    }

    // Save cur pos of an iterator
    m_current_pos = skip + left;
}

void
TermQlcTable::lookup(ParamDecoder& driver_params)
{
    std::vector< Xapian::TermIterator > matched;

    Xapian::TermIterator iter = m_doc.termlist_begin();
    Xapian::TermIterator end  = m_doc.termlist_end();
    for (; ;)
    {
        const std::string& term = driver_params;
        if (term.empty()) break; else
        {
            iter.skip_to(term);
            bool found = (iter != end) && (*iter == term);
            if (found)
                matched.push_back(iter);
        }
    };

    const uint32_t size = matched.size();

    // Put count of elements
    m_driver.m_result << size;

    for (std::vector< Xapian::TermIterator >::iterator 
        match_iter = matched.begin(); 
        match_iter != matched.end(); 
        match_iter++)
    {
        Xapian::TermIterator& iter = *match_iter;
        ParamDecoder params = m_controller;
        m_driver.retrieveTerm(params, iter);
    }
}

