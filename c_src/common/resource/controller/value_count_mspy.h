#ifndef VC_MSPY_RCTRL_H
#define VC_MSPY_RCTRL_H

#include "resource/controller/match_spy.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class ValueCountMatchSpy : public MatchSpy
{
    uint32_t m_slot;

    public:
    ValueCountMatchSpy(uint32_t slot, Xapian::ValueCountMatchSpy* p_spy) 
        : MatchSpy(p_spy), m_slot(slot) {}
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
