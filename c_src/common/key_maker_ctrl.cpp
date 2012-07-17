#include <xapian.h>
#include "key_maker_ctrl.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class KeyMakerControllerInternal
{
    public:
    uint32_t m_reference_count;
    Xapian::KeyMaker* mp_km;

    KeyMakerControllerInternal(Xapian::KeyMaker* km) : mp_km(km)
    {
        m_reference_count = 1;
    }

    ~KeyMakerControllerInternal()
    {
        delete mp_km;
    }

    Xapian::KeyMaker* getKeyMaker()
    {
        return mp_km;
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
};


// -------------------------------------------------------------------
// KeyMakerController
// -------------------------------------------------------------------

KeyMakerController::KeyMakerController(Xapian::KeyMaker* km)
{
    mp_internal = new KeyMakerControllerInternal(km);
}


KeyMakerController::KeyMakerController(const KeyMakerController& src)
{
    mp_internal = src.mp_internal;
    mp_internal->incref();
}


KeyMakerController::KeyMakerController(KeyMakerControllerInternal* src)
{
    mp_internal = src;
    mp_internal->incref();
}


KeyMakerController::~KeyMakerController()
{
    mp_internal->decref();
}


Xapian::KeyMaker* 
KeyMakerController::getKeyMaker()
{
    return mp_internal->getKeyMaker();
}

XAPIAN_ERLANG_NS_END
