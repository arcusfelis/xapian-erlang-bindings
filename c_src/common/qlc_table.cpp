#include "qlc_table.h"
#include "xapian_core.h"
#include <assert.h>
#include <vector>
#include <set>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

// ------------------------------------------------------------------
// QlcTable
// ------------------------------------------------------------------

QlcTable::QlcTable(Driver& driver) 
    : m_driver(driver) {}


QlcTable::~QlcTable()
{}


// ------------------------------------------------------------------
// MSetQlcTable
// ------------------------------------------------------------------

MSetQlcTable::MSetQlcTable(Driver& driver, 
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
MSetQlcTable::getPage(
        ResultEncoder& result, const uint32_t skip, const uint32_t count)
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
    result << left;

    Xapian::MSetIterator iter = m_mset[skip];
    Xapian::MSetIterator last = (count < size) 
                              ? m_mset[skip+left] 
                              : m_mset.end();

    ParamDecoder params = m_controller;
    m_driver.retrieveDocuments(params, result, iter, last);
}


void
MSetQlcTable::lookup(ParamDecoder& driver_params, ResultEncoder& result)
{
    std::set<Xapian::docid> docids;
    uint8_t key = driver_params;

    while (const Xapian::docid docid = driver_params)
        docids.insert(docid);

    bool unique = true;
    switch(key)
    {
        case Driver::GET_DOCID:
            if (m_driver.m_number_of_databases != 1)
                unique = false;
            break;

        case Driver::GET_MULTI_DOCID:
            break;

        default:
            throw BadCommandDriverError(key);
    }

    Xapian::MSetIterator miter = m_mset.begin(), mend = m_mset.end();
    if (unique)
    for (; miter != mend; miter++)
    {
        Xapian::docid current_docid = *miter;
            
        std::set<Xapian::docid>::iterator doc_iter = docids.find(current_docid);
        if (doc_iter != docids.end())
        {
            docids.erase(doc_iter);
            result << MORE;

            ParamDecoder params = m_controller;
            m_driver.selectEncoderAndRetrieveDocument(params, result, miter);
            if (docids.empty())
                break;
        }
    }
    else
    for (; miter != mend; miter++)
    {
        Xapian::docid current_docid = m_driver.docid_sub(*miter);
            
        if (docids.find(current_docid) != docids.end())
        {
            result << MORE;

            ParamDecoder params = m_controller;
            m_driver.selectEncoderAndRetrieveDocument(params, result, miter);
        }
    }

    result << STOP;
}





// ------------------------------------------------------------------
// TermTable
// ------------------------------------------------------------------

TermQlcTable::TermQlcTable(Driver& driver, 
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
TermQlcTable::getPage(
        ResultEncoder& result, const uint32_t skip, const uint32_t count)
{
    if (m_size)
        getPageKnownSize(result, skip, count);
    else
        getPageUnknownSize(result, skip, count);
}

void
TermQlcTable::lookup(ParamDecoder& driver_params, ResultEncoder& result)
{
    ParamDecoder schema_params = m_controller;

    Driver::qlcTermIteratorLookup(
        driver_params, schema_params, result,
        mp_gen->begin(), m_end);
}



// ------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------
        
void
TermQlcTable::getPageUnknownSize(
        ResultEncoder& result, const uint32_t skip, const uint32_t count)
{
    assert(!m_size);

    const uint32_t left = count;

    result << UNKNOWN_SIZE;

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
        result << MORE;
        ParamDecoder params = m_controller;
        m_driver.retrieveTerm(params, result, m_iter);
    }
    result << STOP;

    // Save cur pos of an iterator
    m_current_pos = skip + passed;

    // Total size is known.
    if (m_iter == m_end)
        m_size = m_current_pos;
}


void
TermQlcTable::getPageKnownSize(
        ResultEncoder& result, const uint32_t skip, const uint32_t count)
{
    uint32_t size = m_size;
    assert(skip <= size);
    

    /* Tail size */
    size -= skip;

    // Do while: 
    // * the current element is not the last element;
    // * and not more then wanted count of documents were extracted.
    const uint32_t left = std::min(size, count);

    result << KNOWN_SIZE;
    // Put count of elements
    result << left;

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
        m_driver.retrieveTerm(params, result, m_iter);
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

XAPIAN_ERLANG_NS_END
