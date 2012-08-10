#ifndef KEY_MAKER_RCTRL_H
#define KEY_MAKER_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class KeyMaker : public Base
{
    Xapian::KeyMaker* mp_km;

    public:
    KeyMaker(Xapian::KeyMaker* p_km) : mp_km(p_km) {}
    ~KeyMaker() { delete mp_km; }

    virtual operator Xapian::KeyMaker&()
    {
        return *mp_km;
    }

    std::string type()
    {
        return "Resource::KeyMaker";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
