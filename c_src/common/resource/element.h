#ifndef RESOURCE_ELEMENT_H
#define RESOURCE_ELEMENT_H

#include <stdint.h>

#include "xapian_config.h"

namespace Xapian
{
    class Weight;
    class ValueRangeProcessor;
    class ValueCountMatchSpy;
    class KeyMaker;
};

XAPIAN_RESOURCE_NS_BEGIN

namespace Controller
{
    class Base;
};

class Element 
{
    Controller::Base* mp_controller;

    public:
    Element(Controller::Base& controller);
    Element(Controller::Base* p_controller);

    /// Copy is allowed
    Element(const Element& src);

    ~Element();
    
    Controller::Base& get() { return *mp_controller; }

    /**
     * Use this object as a parent of the child object.
     * Attached (child) objects cannot be used as a parent again.
     */
    void attach(Element& child);

    static Element wrap(Xapian::Weight* p_weight);
    static Element wrap(Xapian::ValueRangeProcessor* p_proc);
    static Element wrap(Xapian::KeyMaker* p_key_maker);
    static Element wrap(uint32_t slot, Xapian::ValueCountMatchSpy* p_spy);
};

XAPIAN_RESOURCE_NS_END
#endif
