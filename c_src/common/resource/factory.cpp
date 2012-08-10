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
create(ParamDecoder& params)
{
    return m_generator.create(m_register, params);
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
            break;
        }

        case SCHEMA_TYPE_CONSTRUCTOR:
        {
            return m_generator.create(m_register, params);
            break;
        }

        default:
            throw BadCommandDriverError(schema_type);
    }
}

/**
 * Get a metadata about constructors.
 * It is called from `xapian_server:init/2`.
 */
void
Factory::
getResourceConstructors(ResultEncoder& result)
{
    Generator::constructor_iterator i, e;
    i = m_generator.constructor_begin();
    e = m_generator.constructor_end();
    // n = 0 and nothing else, because the vector is used inside Generator.
    for (uint32_t n = 0; i != e; i++, n++)
    {
        Constructor& constructor = **i;
        std::string name = constructor.name();
        // see xapian_server:decode_resource_constructor_info_cycle/2
        result << n << name;
    }
}

XAPIAN_RESOURCE_NS_END
