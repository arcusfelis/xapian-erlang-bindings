#include "resource/generator.h"
#include "resource/constructor.h"
#include "xapian_exception.h"

XAPIAN_RESOURCE_NS_BEGIN

Element
Generator::
create(Register& manager, ParamDecoder& params)
{
    uint32_t constructor_num = params;
    // Does exists?
    if (!isExists(constructor_num))
        throw ElementNotFoundDriverError(POS, constructor_num);
    Constructor& constructor = * m_constructors.at(constructor_num);
    // Create a new element.
    return constructor.call(manager, params);
}

XAPIAN_RESOURCE_NS_END
