#ifndef VC_MSPY_RCTRL_H
#define VC_MSPY_RCTRL_H

#include "resource/controller/match_spy.h"
#include "extension/value_count_mspy.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class ValueCountMatchSpy : public MatchSpy
{
    uint32_t m_slot;
    Extension::ValueCountMatchSpy m_ext;
    // It is the same pointer, as in MatchSpy.
    // The object will be deleted by MatchSpy.
    ValueCountMatchSpy* m_vc_spy;

    public:
    ValueCountMatchSpy(uint32_t slot, Xapian::ValueCountMatchSpy* p_spy) 
        : MatchSpy(p_spy), m_ext(slot) {}

    operator Xapian::ValueCountMatchSpy&()
    {
        return *m_vc_spy;
    }

    operator Extension::ValueCountMatchSpy&()
    {
        return m_ext;
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
