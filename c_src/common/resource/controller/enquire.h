#ifndef ENQUIRE_RCTRL_H
#define ENQUIRE_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Enquire : public Base
{
    Xapian::Enquire* mp_enquire;

    public:
    Enquire(Xapian::Enquire* p_enquire) : mp_enquire(p_enquire) {}
    ~Enquire() { delete mp_enquire; }

    virtual operator Xapian::Enquire&()
    {
        return *mp_enquire;
    }

    std::string type()
    {
        return "Resource::Enquire";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
