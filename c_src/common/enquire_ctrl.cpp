#include "key_maker_ctrl.h"
#include "enquire_ctrl.h"
#include <xapian.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class EnquireControllerInternal
{
    uint32_t m_reference_count;
    Xapian::Enquire m_enquire;
    std::vector<KeyMakerController> m_key_maker_controllers;

    public:
    EnquireControllerInternal(Xapian::Enquire& enquire) : m_enquire(enquire)
    {
        m_reference_count = 1;
    }

    Xapian::Enquire& getEnquire()
    {
        return m_enquire;
    }

    void addKeyMakerController(const KeyMakerController& mkc)
    {
        m_key_maker_controllers.push_back(mkc);
    }

    void decref()
    {
        m_reference_count--;
        if (m_reference_count == 0)
            delete this;
    }

    void incref()
    {
        m_reference_count++;
    }

    ~EnquireControllerInternal(){}
};


// -------------------------------------------------------------------
// EnquireController
// -------------------------------------------------------------------

EnquireController::EnquireController(Xapian::Enquire& enquire)
{
    mp_internal = new EnquireControllerInternal(enquire);
}


EnquireController::EnquireController(const EnquireController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


EnquireController::~EnquireController()
{
    mp_internal->decref();
}


Xapian::Enquire& 
EnquireController::getEnquire()
{
    return mp_internal->getEnquire();
}

void 
EnquireController::addKeyMakerController(KeyMakerController& mkc)
{
    mp_internal->addKeyMakerController(mkc);
}

XAPIAN_ERLANG_NS_END
