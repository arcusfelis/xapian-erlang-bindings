#include "termiter_spy_gen.h"
#include "spy_value_ctrl.h"
#include "xapian_exception.h"

#include "termiter_gen.h"
#include "param_decoder.h"
#include <xapian.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

// -------------------------------------------------------------------
// ValueCountSpyController
// -------------------------------------------------------------------

ValueCountSpyController::ValueCountSpyController(
    Xapian::ValueCountMatchSpy* spy) 
        : SpyController(spy) {}


TermIteratorGenerator*
ValueCountSpyController::getIteratorGenerator(ParamDecoder& params)
{
    switch (uint8_t type = params)
    { 
        case VALUES:
            return new SpyValueIteratorGenerator(*this);

        case TOP_VALUES:
        {
            const uint32_t max_values = params;
            if (!max_values)
                throw BadArgumentDriverError();
            return new TopSpyValueIteratorGenerator(*this, max_values);
        }

        default:
            throw BadCommandDriverError(type);
    }
}

XAPIAN_ERLANG_NS_END
