#ifndef WEIGHT_RCTRL_H
#define WEIGHT_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Weight : public Base
{
    Xapian::Weight* mp_weight;

    public:
    Weight(Xapian::Weight* p_weight) : mp_weight(p_weight) {}
    ~Weight() { delete mp_weight; }

    virtual operator Xapian::Weight&()
    {
        return *mp_weight;
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
