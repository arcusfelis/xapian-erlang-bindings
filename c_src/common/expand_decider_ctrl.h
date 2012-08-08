#ifndef XAPIAN_EXPAND_DECIDER_H
#define XAPIAN_EXPAND_DECIDER_H

#include <stdint.h>
#include <xapian.h>

namespace Xapian
{
class ExpandDecider;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class ExpandDeciderControllerInternal;

class ParamDecoder;

class ExpandDeciderController 
{
    ExpandDeciderControllerInternal* mp_internal;

    public:
    ExpandDeciderController(Xapian::ExpandDecider* km);

    /// Copy is allowed
    ExpandDeciderController(const ExpandDeciderController& src);

    ~ExpandDeciderController();

    Xapian::ExpandDecider* getExpandDecider();
};

XAPIAN_ERLANG_NS_END

#endif
