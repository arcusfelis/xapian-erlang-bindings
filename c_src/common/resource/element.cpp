#include "resource/element.h"
#include "xapian_exception.h"

#include "resource/controller/weight.h"
#include "resource/controller/value_count_mspy.h"
#include "resource/controller/value_rproc.h"
#include "resource/controller/key_maker.h"
#include "resource/controller/enquire.h"
#include "resource/controller/query_parser.h"
#include "resource/controller/document.h"
#include "resource/controller/match_set.h"
#include "resource/controller/qlc_table.h"

#include <xapian.h>

XAPIAN_RESOURCE_NS_BEGIN


Element::
Element(Controller::Base& controller)
{
    mp_controller = &controller;
    mp_controller->incref();
}

Element::
Element(Controller::Base* p_controller)
{
    assert(p_controller);
    mp_controller = p_controller;
    mp_controller->incref();
}

/// Copy constructor.
Element::
Element(const Element& source)
{
    mp_controller = source.mp_controller;

    if (mp_controller)
        mp_controller->incref();
}

/// Assignment operator.
Element&
Element::operator = (const Element& source)
{

    // Clean the old data
    if (mp_controller)
        mp_controller->decref();

    // Fill the new data
    mp_controller = source.mp_controller;
    if (mp_controller)
        mp_controller->incref();

    // by convention, always return *this
    return *this;
}

Element::
~Element()
{
    if (mp_controller)
        mp_controller->decref();
}


void 
Element::
attach(Element& child)
{
    assert(mp_controller);
    mp_controller->attach(child);
}

void 
Element::
attachContext(Element& context)
{
    assert(mp_controller);
    mp_controller->attachContext(context);
}

Element::
operator Xapian::MSet&()
{ return *mp_controller; }

Element::
operator Xapian::KeyMaker&()
{ return *mp_controller; }

Element::
operator Xapian::Enquire&()
{ return *mp_controller; }

Element::
operator QlcTable&()
{ return *mp_controller; }

Element::
operator Xapian::Weight&()
{ return *mp_controller; }

Element::
operator Xapian::Query&()
{ return *mp_controller; }

Element::
operator Xapian::MatchDecider&()
{ return *mp_controller; }

Element::
operator Xapian::ExpandDecider&()
{ return *mp_controller; }

Element::
operator Xapian::ValueRangeProcessor&()
{ return *mp_controller; }

Element::
operator Xapian::QueryParser&()
{ return *mp_controller; }

Element::
operator Xapian::Stopper&()
{ return *mp_controller; }

Element::
operator Xapian::Stem&()
{ return *mp_controller; }

Element::
operator Xapian::ValueCountMatchSpy&()
{ return *mp_controller; }

Element::
operator Xapian::MatchSpy&()
{ return *mp_controller; }

Element::
operator Xapian::Document&()
{ return *mp_controller; }

Element::
operator Extension::ValueCountMatchSpy&()
{ return *mp_controller; }

void 
Element::
finalize() 
{ return mp_controller->finalize(); }

bool 
Element::
is_finalized() 
{ return mp_controller->is_finalized(); }


// Static functions

Element
Element::
createContext()
{
    return Element(new Controller::Base());
}

Element 
Element::
wrap(Xapian::Weight* p_weight)
{
    return Element(new Controller::Weight(p_weight));
}

Element
Element::
wrap(uint32_t slot, Xapian::ValueCountMatchSpy* p_spy)
{
    return Element(new Controller::ValueCountMatchSpy(slot, p_spy));
}

Element
Element::
wrap(Xapian::ValueRangeProcessor* p_proc)
{
    return Element(new Controller::ValueRangeProcessor(p_proc));
}

Element
Element::
wrap(Xapian::KeyMaker* p_key_maker)
{
    return Element(new Controller::KeyMaker(p_key_maker));
}

Element
Element::
wrap(Xapian::MSet* p_mset)
{
    return Element(new Controller::MSet(p_mset));
}

Element
Element::
wrap(QlcTable* p_table)
{
    return Element(new Controller::QlcTable(p_table));
}

Element
Element::
wrap(Xapian::Enquire* p_enquire)
{
    return Element(new Controller::Enquire(p_enquire));
}

Element
Element::
wrap(Xapian::QueryParser* p_query_parser)
{
    return Element(new Controller::QueryParser(p_query_parser));
}

Element
Element::
wrap(Xapian::Document* p_document)
{
    return Element(new Controller::Document(p_document));
}

XAPIAN_RESOURCE_NS_END
