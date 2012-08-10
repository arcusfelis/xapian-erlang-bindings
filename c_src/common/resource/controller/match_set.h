#ifndef MATCH_SET_RCTRL_H
#define MATCH_SET_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class MSet : public Base
{
    Xapian::MSet* mp_mset;

    public:
    MSet(Xapian::MSet* p_mset) : mp_mset(p_mset) {}
    ~MSet() { delete mp_mset; }

    virtual operator Xapian::MSet&()
    {
        return *mp_mset;
    }

    std::string type()
    {
        return "Resource::MatchSet";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
