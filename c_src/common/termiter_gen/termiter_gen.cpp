#include <stdint.h>
#include <xapian.h> 

#include "termiter_spy_gen.h"
#include "xapian_exception.h"
#include "param_decoder.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

enum ObjectType           
{                         
    VALUES           = 0, 
    TOP_VALUES       = 1  
};                        

TermIteratorGenerator*
TermIteratorGenerator::create(
        ParamDecoder& params,
        Xapian::ValueCountMatchSpy& spy)
{
    switch (uint8_t type = params)
    {
        case VALUES:
            return new SpyValueIteratorGenerator(spy);

        case TOP_VALUES:
        {
            const uint32_t max_values = params;
            if (!max_values)
                throw BadArgumentDriverError();
            return new TopSpyValueIteratorGenerator(spy, max_values);
        }

        default:
            throw BadCommandDriverError(type);
    }
}


TermIteratorGenerator*
TermIteratorGenerator::create(Xapian::Document& doc)
{
    return new DocumentTermIteratorGenerator(doc);
}

XAPIAN_ERLANG_NS_END
