#ifndef RESOURCE_FACTORY_H
#define RESOURCE_FACTORY_H

#include <stdint.h>
#include "resource/generator.h"
#include "resource/register.h"

#include "xapian_config.h"

XAPIAN_ERLANG_NS_BEGIN
class ParamDecoder;
class ResultEncoder;
class Driver;
XAPIAN_ERLANG_NS_END

XAPIAN_RESOURCE_NS_BEGIN

class Factory
{
    enum ResourceSchemaType
    {
        // These numbers are "interesting".
        // It allows to get BadCommandDriverError exception, 
        // if something was encoded illegally.
        SCHEMA_TYPE_REFERENCE    = 56,
        SCHEMA_TYPE_CONSTRUCTOR  = 97,
        SCHEMA_UNDEFINED         = 61
    };

    Generator m_generator;
    Register  m_register;

    public:
    Factory(Driver& driver);

    Element
    extract(ParamDecoder& params);

    void
    skip(ParamDecoder& params);

    Element
    extract(Element& context, ParamDecoder& params);

    void
    save(Element& element, ResultEncoder& result);

    Element
    create(ParamDecoder& params);

    void
    release(ParamDecoder& params);

    void
    multiRelease(ParamDecoder& params);

    void
    getResourceConstructors(ResultEncoder& result);
};

XAPIAN_RESOURCE_NS_END
#endif
