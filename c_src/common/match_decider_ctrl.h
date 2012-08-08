#ifndef XAPIAN_MATCH_DECIDER_H
#define XAPIAN_MATCH_DECIDER_H

#include <stdint.h>
#include <xapian.h>

namespace Xapian
{
class MatchDecider;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class MatchDeciderControllerInternal;

class ParamDecoder;

class MatchDeciderController 
{
    MatchDeciderControllerInternal* mp_internal;

    public:
    MatchDeciderController(Xapian::MatchDecider* km);

    /// Copy is allowed
    MatchDeciderController(const MatchDeciderController& src);

    ~MatchDeciderController();

    Xapian::MatchDecider* getMatchDecider();
};

XAPIAN_ERLANG_NS_END

#endif
