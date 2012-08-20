#ifndef STEM_RCTRL_H
#define STEM_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Stem : public Base
{
    Xapian::Stem* mp_stopper;

    public:
    Stem(Xapian::Stem* p_stopper) : mp_stopper(p_stopper) {}
    ~Stem() { delete mp_stopper; }

    virtual operator Xapian::Stem&()
    {
        return *mp_stopper;
    }

    std::string type()
    {
        return "Resource::Stem";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
