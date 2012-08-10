#ifndef VC_MSPY_EXT_H
#define VC_MSPY_EXT_H

#include "xapian_config.h"
#include <stdint.h>

XAPIAN_EXT_NS_BEGIN

class ValueCountMatchSpy
{
    uint32_t m_slot;

    public:
    ValueCountMatchSpy(uint32_t slot) 
        : m_slot(slot) {}

    uint32_t getSlot() { return m_slot; }
};

XAPIAN_EXT_NS_END
#endif
