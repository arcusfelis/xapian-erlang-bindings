#ifndef MEMORY_MANAGER_H
#define MEMORY_MANAGER_H

// Internal imports
#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class MemoryManager
{
    public:
    virtual ~MemoryManager() {};
    virtual void* alloc(size_t size);
    virtual void free(void* pos);
};

XAPIAN_ERLANG_NS_END

#endif
