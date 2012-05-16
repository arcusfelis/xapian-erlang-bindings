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
    // Flags, that signal about end of list.
    const uint8_t more = 1, stop = 0;

    while (const Xapian::docid docid = driver_params)
    for (Xapian::MSetIterator iter = m_mset.begin(); iter != m_mset.end(); iter++)
    {
        Xapian::docid current_docid = *iter;
        if (current_docid == docid)
        {
            m_driver.m_result << more;

            ParamDecoder params = m_controller;
            Xapian::Document doc = iter.get_document();
            m_driver.retrieveDocument(params, doc, &iter);
            break;
        }
    }
    m_driver.m_result << stop;
}





// ------------------------------------------------------------------
// TermTable
// ------------------------------------------------------------------


TermQlcTable::TermQlcTable(XapianErlangDriver& driver, 
        Xapian::Document& doc, const ParamDecoderController& controller) 
    : QlcTable(driver), m_controller(controller)
{
    // Call doc.termlist_begin() twice.
    m_begin = doc.termlist_begin();
    m_iter = doc.termlist_begin();
    m_end = doc.termlist_end();

    // Do it after doc.term_list_begin()
    m_size = static_cast<uint32_t>(doc.termlist_count());
    if (!m_size)
        throw EmptySetDriverError();

    assert(m_begin != m_end);

    m_current_pos = 0;
    m_first_tname = *m_begin;   
}


uint32_t 
TermQlcTable::numOfObjects()
{
    return m_size;
}


/**
 * Skip "skip" documents. 
 * Read not more then "count" documents.
 */
void
TermQlcTable::getPage(const uint32_t skip, const uint32_t count)
{
    uint32_t size = m_size;
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
        // Go to the beginning
        // It is the same as a call of m_iter = doc.termlist_begin();
        assert(m_iter != m_end);
        m_iter.skip_to(m_first_tname);
        assert(*m_iter == m_first_tname);
        for (uint32_t i = skip; i; i--)
            m_iter++;
    }
//  std::cout << " [" m_size << " " << size << " " << skip  << " " << left << "] ";

    // While left > 0 and term is not last.
    for (uint32_t i = left; i; m_iter++, i--)
    {
        assert(m_iter != m_end);
        ParamDecoder params = m_controller;
        m_driver.retrieveTerm(params, m_iter);
    }

    // Save cur pos of an iterator
    m_current_pos = skip + left;
}

void
TermQlcTable::lookup(ParamDecoder& driver_params)
{
    ParamDecoder schema_params = m_controller;
    XapianErlangDriver::qlcTermIteratorLookup(
        driver_params, schema_params, m_driver.m_result,
        m_begin, m_end);
}


