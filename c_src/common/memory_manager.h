#ifndef MEMORY_MANAGER_H
#define MEMORY_MANAGER_H

// Internal imports
#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/**
 * This class uses memory-management functions from C stdlib.
 */
class MemoryManager
{
    public:
    /**
     * Because this class have other virtual functions.
     */
    virtual ~MemoryManager() {};
    virtual void* alloc(size_t size);
    /**
     * @a pos is a pointer on a memory block, allocated with @ref alloc.
     */
    virtual void free(void* pos);
};

XAPIAN_ERLANG_NS_END

#endif
