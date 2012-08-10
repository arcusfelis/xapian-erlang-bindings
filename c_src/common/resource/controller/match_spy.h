#ifndef MSPY_RCTRL_H
#define MSPY_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class MatchSpy : public Base
{
    Xapian::MatchSpy* mp_spy;
    bool mb_finalized;

    public:
    MatchSpy(Xapian::MatchSpy* p_spy) : mp_spy(p_spy), mb_finalized(false) {}
    ~MatchSpy() { delete mp_spy; }

    virtual operator Xapian::MatchSpy&()
    {
        return *mp_spy;
    }

    std::string type()
    {
        return "Resource::MatchSpy";
    }

    void finalize()
    {
        mb_finalized = true;
    }

    bool is_finalized()
    {
        return mb_finalized;
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
