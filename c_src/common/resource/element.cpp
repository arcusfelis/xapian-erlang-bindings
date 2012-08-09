#include "resource/element.h"
#include "xapian_exception.h"

#include "resource/controller/weight.h"
#include "resource/controller/value_count_mspy.h"
#include "resource/controller/value_rproc.h"
#include "resource/controller/key_maker.h"

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
    mp_controller = p_controller;
    mp_controller->incref();
}

Element::
~Element()
{
    mp_controller->decref();
}

void 
Element::
attach(Element& child)
{
    mp_controller->attach(child);
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

XAPIAN_RESOURCE_NS_END
