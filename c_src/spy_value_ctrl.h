#ifndef XAPIAN_SPY_VALUE_CTRL_H
#define XAPIAN_SPY_VALUE_CTRL_H

#include <stdint.h>
#include "spy_ctrl.h"

namespace Xapian
{
class ValueCountMatchSpy;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class ValueCountSpyController : public SpyController
{
    enum ObjectType
    {
        VALUES           = 0,
        TOP_VALUES       = 1
    };

    public:
    /// Passed spy will be deallocated by system
    ValueCountSpyController(Xapian::ValueCountMatchSpy* spy);

    TermIteratorGenerator*
    getIteratorGenerator(ParamDecoder& params);
};

XAPIAN_ERLANG_NS_END
#endif
