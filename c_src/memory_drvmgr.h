#ifndef DRIVER_MEMORY_MANAGER_H
#define DRIVER_MEMORY_MANAGER_H

#include "xapian_config.h"
#include "memory_manager.h"
XAPIAN_ERLANG_NS_BEGIN

class DriverMemoryManager: public MemoryManager
{
    public:
    void* alloc(size_t size);
    void free(void* pos);
};

XAPIAN_ERLANG_NS_END

#endif
