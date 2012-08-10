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
    // It is the same pointer, as in MatchSpy.
    // The object will be deleted by MatchSpy.
    Xapian::ValueCountMatchSpy* mp_vc_spy;
    Extension::ValueCountMatchSpy m_ext;

    public:
    ValueCountMatchSpy(uint32_t slot, Xapian::ValueCountMatchSpy* p_spy) 
        : MatchSpy(p_spy), mp_vc_spy(p_spy), m_ext(slot) {}

    operator Xapian::ValueCountMatchSpy&()
    {
        return *mp_vc_spy;
    }

    operator Extension::ValueCountMatchSpy&()
    {
        return m_ext;
    }

    std::string type()
    {
        return "Resource::ValueCountMatchSpy";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
