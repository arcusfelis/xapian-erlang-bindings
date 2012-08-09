#include "resource/factory.h"
#include "xapian_exception.h"
#include "param_decoder.h"
#include "result_encoder.h"

XAPIAN_RESOURCE_NS_BEGIN

Factory::Factory()
{
    m_generator.registerCallbacks();
}

void
Factory::
save(Element& element, ResultEncoder& result)
{
    uint32_t element_num = m_register.put(element);
    result << element_num;
}

void
Factory::
release(ParamDecoder& params)
{
    uint32_t element_num = params;
    m_register.remove(element_num);
}

Element
Factory::
extract(Element& context, ParamDecoder& params)
{
    Element elem = extract(params);
    context.attach(elem);
    return elem;
}

Element
Factory::
extract(ParamDecoder& params)
{
    switch(uint8_t schema_type = params)
    {
        case SCHEMA_TYPE_REFERENCE:
        {
            uint32_t element_num = params;
            return m_register.get(element_num);
        }

        case SCHEMA_TYPE_CONSTRUCTOR:
        {
            return m_generator.create(m_register, params);
        }

        default:
            throw BadCommandDriverError(schema_type);
    }
}

XAPIAN_RESOURCE_NS_END
