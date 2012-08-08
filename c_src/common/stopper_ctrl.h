#ifndef XAPIAN_STOPPER_H
#define XAPIAN_STOPPER_H

#include <stdint.h>
#include <xapian.h>

namespace Xapian
{
class Stopper;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class StopperControllerInternal;

class ParamDecoder;

class StopperController 
{
    StopperControllerInternal* mp_internal;
    
    public:
    StopperController(Xapian::Stopper* km);

    /// Copy is allowed
    StopperController(const StopperController& src);

    ~StopperController();

    Xapian::Stopper* getStopper();
};

XAPIAN_ERLANG_NS_END

#endif
