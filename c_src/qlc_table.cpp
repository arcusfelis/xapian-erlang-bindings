#include "qlc_table.h"
#include "xapian_core.h"
#include <assert.h>
#include <vector>
#include <set>

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
    std::set<Xapian::docid> docids;
    while (const Xapian::docid docid = driver_params)
        docids.insert(docid);
    for (Xapian::MSetIterator iter = m_mset.begin(); iter != m_mset.end(); iter++)
    {
        Xapian::docid current_docid = *iter;
        if (docids.find(current_docid) != docids.end())
        {
            m_driver.m_result << MORE;

            ParamDecoder params = m_controller;
            Xapian::Document doc = iter.get_document();
            m_driver.retrieveDocument(params, doc, &iter);
            break;
        }
    }
    m_driver.m_result << STOP;
}





// ------------------------------------------------------------------
// TermTable
// ------------------------------------------------------------------


TermQlcTable::TermQlcTable(XapianErlangDriver& driver, 
        TermIteratorGenerator* gen, 
        const ParamDecoderController& controller) 
    : QlcTable(driver), mp_gen(gen), m_controller(controller)
{
    m_iter = mp_gen->begin();
    m_end = mp_gen->end();

    assert(!mp_gen->empty() || (m_iter == m_end));
    if (mp_gen->empty())
        throw EmptySetDriverError();

    m_size = static_cast<uint32_t>(mp_gen->size());
    m_current_pos = 0;
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
    if (m_size)
        getPageKnownSize(skip, count);
    else
        getPageUnknownSize(skip, count);
}

void
TermQlcTable::lookup(ParamDecoder& driver_params)
{
    ParamDecoder schema_params = m_controller;

    XapianErlangDriver::qlcTermIteratorLookup(
        driver_params, schema_params, m_driver.m_result,
        mp_gen->begin(), m_end);
}



// ------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------
        
void
TermQlcTable::getPageUnknownSize(const uint32_t skip, const uint32_t count)
{
    assert(!m_size);

    const uint32_t left = count;

    m_driver.m_result << UNKNOWN_SIZE;

    // Skip first elements
    if (skip != m_current_pos)
    {
        goToAndCheckBorder(skip);
    }

    uint32_t passed = 0;
    // While left > 0 and term is not last.
    for (uint32_t i = left; i && (m_iter != m_end); m_iter++, i--, passed++)
    {
        assert(m_iter != m_end);
        m_driver.m_result << MORE;
        ParamDecoder params = m_controller;
        m_driver.retrieveTerm(params, m_iter);
    }
    m_driver.m_result << STOP;

    // Save cur pos of an iterator
    m_current_pos = skip + passed;

    // Total size is known.
    if (m_iter == m_end)
        m_size = m_current_pos;
}


void
TermQlcTable::getPageKnownSize(const uint32_t skip, const uint32_t count)
{
    uint32_t size = m_size;
    assert(skip <= size);
    

    /* Tail size */
    size -= skip;

    // Do while: 
    // * the current element is not the last element;
    // * and not more then wanted count of documents were extracted.
    const uint32_t left = std::min(size, count);

    m_driver.m_result << KNOWN_SIZE;
    // Put count of elements
    m_driver.m_result << left;

    // Skip first elements
    if (skip != m_current_pos)
    {
        goTo(skip);
    }

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
TermQlcTable::goTo(const uint32_t wanted_pos)
{
    uint32_t skip_from_cur;
    if (wanted_pos < m_current_pos)
    {
        // Go backward

        m_iter = mp_gen->begin();
        assert(m_iter != m_end);
        skip_from_cur = wanted_pos;
    } else 
        // Go forward
        skip_from_cur = wanted_pos - m_current_pos;

    for (uint32_t i = skip_from_cur; i; i--)
        m_iter++;

    m_current_pos = wanted_pos;
}


/// Checks out of border
void
TermQlcTable::goToAndCheckBorder(const uint32_t wanted_pos)
{
    if (wanted_pos < m_current_pos)
    {
        // Go backward is save
        goTo(wanted_pos);
    } else {
        // Go forward requires to check a border
        uint32_t skip_from_cur = wanted_pos - m_current_pos;

        for (uint32_t i = skip_from_cur; i && (m_iter != m_end); i--)
        {
            m_iter++; 
            m_current_pos++;
        }
    }
}
