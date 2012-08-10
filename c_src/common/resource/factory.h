#ifndef RESOURCE_FACTORY_H
#define RESOURCE_FACTORY_H

#include <stdint.h>
#include "resource/generator.h"
#include "resource/register.h"

#include "xapian_config.h"

XAPIAN_ERLANG_NS_BEGIN
class ParamDecoder;
class ResultEncoder;
XAPIAN_ERLANG_NS_END

XAPIAN_RESOURCE_NS_BEGIN

class Factory
{
    enum ResourceSchemaType
    {
        SCHEMA_TYPE_REFERENCE    = 1,
        SCHEMA_TYPE_CONSTRUCTOR  = 2
    };

    Generator m_generator;
    Register  m_register;

    public:
    Factory();

    Element
    extract(ParamDecoder& params);

    Element
    extract(Element& context, ParamDecoder& params);

    void
    save(Element& element, ResultEncoder& result);

    void
    release(ParamDecoder& params);

    void
    getResourceConstructors(ResultEncoder& result);
};

XAPIAN_RESOURCE_NS_END
#endif
