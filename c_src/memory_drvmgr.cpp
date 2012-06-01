// External imports
#include "erl_driver.h"

// Internal imports
#include "memory_drvmgr.h"
#include "xapian_exception.h"

XAPIAN_ERLANG_NS_BEGIN

void* DriverMemoryManager::alloc(size_t size)
{
    void* buf = driver_alloc(size);
    if (buf == NULL)
        throw MemoryAllocationDriverError(size);
//  std::cout << "Alloc: " << buf << ' ' << size << '\n';
//  std::cout.flush();
    return buf;
}

void DriverMemoryManager::free(void* buf)
{
//  std::cout << "Free: " << buf << '\n';
//  std::cout.flush();
    driver_free(buf);
}

XAPIAN_ERLANG_NS_END

