// External imports
#include <stdlib.h>

// Internal imports
#include "memory_manager.h"
XAPIAN_ERLANG_NS_BEGIN

void* MemoryManager::alloc(size_t size)
{
    void* pos = malloc(size);
    return pos;
}

void MemoryManager::free(void* pos)
{
    /* call free() from C stdlib */
    ::free(pos);
}

XAPIAN_ERLANG_NS_END

