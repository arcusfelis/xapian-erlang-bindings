#ifndef XAPIAN_STEM_H
#define XAPIAN_STEM_H

#include <stdint.h>
#include <xapian.h>

namespace Xapian
{
class Stem;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class StemControllerInternal;

class ParamDecoder;

class StemController 
{
    StemControllerInternal* mp_internal;

    public:
    StemController(Xapian::Stem* km);

    /// Copy is allowed
    StemController(const StemController& src);

    ~StemController();

    Xapian::Stem* getStem();
};

XAPIAN_ERLANG_NS_END

#endif
