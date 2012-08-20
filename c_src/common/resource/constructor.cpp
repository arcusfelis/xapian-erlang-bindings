#include "resource/constructor.h"

XAPIAN_RESOURCE_NS_BEGIN

Constructor*
Constructor::create(std::string name, CreatePureResourceFn p_fun)
{
    return new PureConstructor(name, p_fun);
}

Constructor*
Constructor::create(Driver& driver, std::string name, CreateDriverResourceFn p_fun)
{
    return new DriverConstructor(driver, name, p_fun);
}

XAPIAN_RESOURCE_NS_END

