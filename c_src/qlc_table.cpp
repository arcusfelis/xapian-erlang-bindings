#include "qlc_table.h"
#include "xapian_driver.h"
#include <assert.h>
#include <vector>

QlcTable::QlcTable(XapianErlangDriver& driver) 
    : m_driver(driver) {}


QlcTable::~QlcTable()
{}


MSetQlcTable::MSetQlcTable(XapianErlangDriver& driver, 
    Xapian::MSet& mset, const ParamDecoderController& controller) 
    : QlcTable(driver), m_mset(mset), m_controller(controller)
{
}

uint32_t 
MSetQlcTable::numOfObjects() const
{
    return static_cast<uint32_t>(m_mset.size());
}

/**
 * Skip "skip" documents. 
 * Read not more then "count" documents.
 */
void
MSetQlcTable::getPage(const uint32_t skip, const uint32_t count) const
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
MSetQlcTable::lookup(ParamDecoder& driver_params) const
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
        Xapian::MSetIterator iter = * match_iter;
        ParamDecoder params = m_controller;
        Xapian::Document doc = iter.get_document();
        m_driver.retrieveDocument(params, doc, &iter);
    }
}
