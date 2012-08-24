#ifndef RESOURCE_ELEMENT_H
#define RESOURCE_ELEMENT_H

#include <stdint.h>

#include "xapian_config.h"

namespace Xapian
{
    class Weight;
    class ValueRangeProcessor;
    class ValueCountMatchSpy;
    class MSet;
    class KeyMaker;
    class Enquire;
    class Query;
    class MatchDecider;
    class ExpandDecider;
    class QueryParser;
    class TermGenerator;
    class Stopper;
    class Stem;
    class MatchSpy;
    class MSet;
    class Document;
}

XAPIAN_ERLANG_NS_BEGIN
    class QlcTable;
XAPIAN_ERLANG_NS_END

XAPIAN_EXT_NS_BEGIN
    class ValueCountMatchSpy;
XAPIAN_EXT_NS_END

XAPIAN_RESOURCE_NS_BEGIN

namespace Controller
{
    class Base;
}

class Element 
{
    Controller::Base* mp_controller;

    public:
    Element(Controller::Base& controller);
    Element(Controller::Base* p_controller);
    Element&
    operator = (const Element& source);

    Element() : mp_controller(0) {}
    /// Copy is allowed
    Element(const Element& src);

    ~Element();
    
    Controller::Base& get() { return *mp_controller; }

    /**
     * Use this object as a parent of the child object.
     * Attached (child) objects cannot be used as a parent again.
     */
    void attach(Element& child);

    /**
     * Attach children of the @a context.
     *
     * If the context has no children, then do nothing.
     */
    void attachContext(Element& context);

    static Element wrap(Xapian::Weight* p_weight);
    static Element wrap(Xapian::ValueRangeProcessor* p_proc);
    static Element wrap(Xapian::KeyMaker* p_key_maker);
    static Element wrap(Xapian::Enquire* p_enquire);
    static Element wrap(Xapian::QueryParser* p_query_parser);
    static Element wrap(Xapian::TermGenerator* p_term_gen);
    static Element wrap(Xapian::Document* p_document);
    static Element wrap(Xapian::Query* p_query);
    static Element wrap(Xapian::MSet* p_mset);
    static Element wrap(Xapian::Stopper* p_stopper);
    static Element wrap(Xapian::Stem* p_stemmer);
    static Element wrap(QlcTable* p_table);
    static Element wrap(uint32_t slot, 
            Xapian::ValueCountMatchSpy* p_spy);
    /**
     * Create a new context.
     * Context is a object, that's goal is aggregating other Elements.
     */
    static Element createContext();
    operator Xapian::MSet&();
    operator Xapian::KeyMaker&();
    operator Xapian::Enquire&();
    operator QlcTable&();
    operator Xapian::Weight&();
    operator Xapian::Query&();
    operator Xapian::MatchDecider&();
    operator Xapian::ExpandDecider&();
    operator Xapian::ValueRangeProcessor&();
    operator Xapian::QueryParser&();
    operator Xapian::TermGenerator&();
    operator Xapian::Stopper&();
    operator Xapian::Stem&();
    operator Xapian::ValueCountMatchSpy&();
    operator Xapian::MatchSpy&();
    operator Xapian::Document&();

    // Extensions
    operator Extension::ValueCountMatchSpy&();

    void finalize();
    bool is_finalized();
};

XAPIAN_RESOURCE_NS_END
#endif
