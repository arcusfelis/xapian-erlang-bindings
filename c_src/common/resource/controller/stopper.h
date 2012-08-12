#ifndef STOPPER_RCTRL_H
#define STOPPER_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Stopper : public Base
{
    Xapian::Stopper* mp_stopper;

    public:
    Stopper(Xapian::Stopper* p_stopper) : mp_stopper(p_stopper) {}
    ~Stopper() { delete mp_stopper; }

    virtual operator Xapian::Stopper&()
    {
        return *mp_stopper;
    }

    std::string type()
    {
        return "Resource::Stopper";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
