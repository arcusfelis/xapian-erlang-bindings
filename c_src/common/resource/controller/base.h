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
    class TermGenerator;
    class Stem;
    class Stopper;
    class MatchSpy;
    class ValueCountMatchSpy;
    class Document;
}

XAPIAN_EXT_NS_BEGIN
    class ValueCountMatchSpy;
XAPIAN_EXT_NS_END

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Base
{
    unsigned m_counter;
    std::vector<Element> m_children;
    bool mb_attached;

    /// Assignment operator.
    /// Assignment is not allowed.
    Base & operator= (const Base & /*source*/) { assert(false); return *this; }

    /// Copy constructor.
    /// Copy is not allowed.
    Base(const Base & /*source*/) { assert(false); }

    public:
    Base() : m_counter(0), mb_attached(false) {}
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
        if (mb_attached)
            throw AlreadyAttachedDriverError(POS, type(), child.get().type());
        assert(!mb_attached);

        // Create a copy of "the reference" 
        // (i.e. Element).
        m_children.push_back(child);
        // Lock the list of children.
        child.get().mb_attached = true;
    }

    void
    attachContext(Element& context)
    {
        Base& con_base = context.get();
        if (con_base.m_children.empty())
            attach(context);
        else
            con_base.mb_attached = true;
    }

    virtual std::string type()
    {
        return "Resource::Controller";
    }

    virtual operator Xapian::MSet&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::MSet");
    }

    virtual operator Xapian::KeyMaker&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::KeyMaker");
    }

    virtual operator Xapian::Enquire&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::Enquire");
    }

    virtual operator QlcTable&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "QlcTable");
    }

    virtual operator Xapian::Weight&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::Weight");
    }

    virtual operator Xapian::Query&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::Query");
    }

    virtual operator Xapian::MatchDecider&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::MatchDecider");
    }

    virtual operator Xapian::ExpandDecider&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::ExpandDecider");
    }

    virtual operator Xapian::ValueRangeProcessor&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::ValueRangeProcessor");
    }

    virtual operator Xapian::QueryParser&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::QueryParser");
    }

    virtual operator Xapian::TermGenerator&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::TermGenerator");
    }

    virtual operator Xapian::Stopper&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::Stopper");
    }

    virtual operator Xapian::Stem&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::Stem");
    }

    virtual operator Xapian::ValueCountMatchSpy&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::ValueCountMatchSpy");
    }

    virtual operator Xapian::MatchSpy&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::MatchSpy");
    }

    virtual operator Xapian::Document&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), "Xapian::Document");
    }

    // Extensions
    virtual operator Extension::ValueCountMatchSpy&()
    {
        throw ResourceTypeMismatchDriverError(POS, type(), 
                "Extension::ValueCountMatchSpy");
    }

    virtual void finalize()
    {
        throw AbstractMethodDriverError(POS, type(), "finalize");
    }

    virtual bool is_finalized()
    {
        throw AbstractMethodDriverError(POS, type(), "is_finalized");
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
