#ifndef XAPIAN_ENQUIRE_H
#define XAPIAN_ENQUIRE_H

#include <stdint.h>

namespace Xapian
{
class Enquire;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class EnquireControllerInternal;
class KeyMakerController;

class EnquireController 
{
    EnquireControllerInternal* mp_internal;

    /**
     * Decrease the couter inside @ref mp_internal.
     */

    public:
    EnquireController(Xapian::Enquire& km);

    /// Copy is allowed
    EnquireController(const EnquireController& src);

    ~EnquireController();

    Xapian::Enquire& getEnquire();

    void addKeyMakerController(KeyMakerController& mkc);
};

XAPIAN_ERLANG_NS_END

#endif
