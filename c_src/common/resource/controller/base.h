#ifndef RESOURCE_CONTROLLER_H
#define RESOURCE_CONTROLLER_H

#include "xapian_exception.h"
#include "resource/element.h"
#include <assert.h>
#include <vector>
#include <string>

#include "xapian_config.h"

XAPIAN_ERLANG_NS_BEGIN
class QlcTable;
XAPIAN_ERLANG_NS_END

namespace Xapian
{
    class MSet;
    class KeyMaker;
    class Enquire;
    class Weight;
    class Query;
    class MatchDecider;
    class ExpandDecider;
    class ValueRangeProcessor;
    class QueryParser;
    class Stem;
    class Stopper;
    class MatchSpy;
    class ValueCountMatchSpy;
    class Document;
}

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Base
{
    unsigned m_counter;
    std::vector<Element> m_children;
    bool mb_attached;

    public:
    Base() : m_counter(0) {}
    virtual 
    ~Base() {}

    void incref() { m_counter++; }
    void decref() 
    {
        assert(m_counter>0); 
        m_counter--; 
        if (!m_counter)
            delete this;
    }

    void
    attach(Element& child)
    {
        assert(!mb_attached);

        // Create a copy of "the reference" 
        // (i.e. Element).
        m_children.push_back(child);
        // Lock the list of children.
        child.get().mb_attached = true;
    }

    virtual std::string type()
    {
        return "Resource::Controller";
    }

    virtual operator Xapian::MSet&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::MSet");
    }

    virtual operator Xapian::KeyMaker&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::KeyMaker");
    }

    virtual operator Xapian::Enquire&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::Enquire");
    }

    virtual operator QlcTable&()
    {
        throw ResourceTypeMismatchDriverError(type(), "QlcTable");
    }

    virtual operator Xapian::Weight&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::Weight");
    }

    virtual operator Xapian::Query&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::Query");
    }

    virtual operator Xapian::MatchDecider&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::MatchDecider");
    }

    virtual operator Xapian::ExpandDecider&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::ExpandDecider");
    }

    virtual operator Xapian::ValueRangeProcessor&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::ValueRangeProcessor");
    }

    virtual operator Xapian::QueryParser&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::QueryParser");
    }

    virtual operator Xapian::Stopper&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::Stopper");
    }

    virtual operator Xapian::Stem&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::Stem");
    }

    virtual operator Xapian::ValueCountMatchSpy&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::ValueCountMatchSpy");
    }

    virtual operator Xapian::MatchSpy&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::MatchSpy");
    }

    virtual operator Xapian::Document&()
    {
        throw ResourceTypeMismatchDriverError(type(), "Xapian::Document");
    }

    virtual void finalize()
    {
        throw AbstractMethodDriverError(type(), "finalize");
    }

    virtual bool is_finalized()
    {
        throw AbstractMethodDriverError(type(), "is_finalized");
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
