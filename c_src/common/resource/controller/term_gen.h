#ifndef TERM_GEN_RCTRL_H
#define TERM_GEN_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class TermGenerator : public Base
{
    Xapian::TermGenerator* mp_tg;

    public:
    TermGenerator(Xapian::TermGenerator* p_tg) : mp_tg(p_tg) {}
    ~TermGenerator() { delete mp_tg; }

    virtual operator Xapian::TermGenerator&()
    {
        return *mp_tg;
    }

    std::string type()
    {
        return "Resource::TermGenerator";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
