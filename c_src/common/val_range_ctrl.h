#ifndef XAPIAN_VAL_RANGE_H
#define XAPIAN_VAL_RANGE_H

#include <stdint.h>
#include <xapian.h>

namespace Xapian
{
class ValueRangeProcessor;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class ValueRangeProcessorControllerInternal;

class ParamDecoder;

class ValueRangeProcessorController 
{
    ValueRangeProcessorControllerInternal* mp_internal;

    public:
    ValueRangeProcessorController(Xapian::ValueRangeProcessor* km);

    /// Copy is allowed
    ValueRangeProcessorController(const ValueRangeProcessorController& src);

    ~ValueRangeProcessorController();

    Xapian::ValueRangeProcessor* getValueRangeProcessor();
};

XAPIAN_ERLANG_NS_END

#endif
