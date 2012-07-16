#include "key_maker_ctrl.h"
#include "enquire_ctrl.h"
#include <xapian.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class EnquireControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::Enquire m_enquire;
    std::vector<KeyMakerController> m_key_maker_controllers;

    EnquireControllerInternal(Xapian::Enquire& enquire) : m_enquire(enquire)
    {
        m_reference_count = 1;
    }

    Xapian::Enquire& getEnquire()
    {
        return m_enquire;
    }

    void addKeyMakerController(KeyMakerController mkc)
    {
        m_key_maker_controllers.push_back(mkc);
    }
};


// -------------------------------------------------------------------
// EnquireController
// -------------------------------------------------------------------

void 
EnquireController::decref()
{
    if (--mp_internal->m_reference_count == 0)
        delete mp_internal;
}


EnquireController::EnquireController(Xapian::Enquire& enquire)
{
    mp_internal = new EnquireControllerInternal(enquire);
}


EnquireController::EnquireController(const EnquireController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->m_reference_count++;
}


EnquireController::EnquireController(EnquireControllerInternal* src)
{
    mp_internal = src;
    mp_internal->m_reference_count++;
}


EnquireController::~EnquireController()
{
    decref();
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
