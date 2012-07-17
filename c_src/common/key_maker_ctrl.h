#ifndef XAPIAN_KEY_MAKER_H
#define XAPIAN_KEY_MAKER_H

#include <stdint.h>
#include <xapian.h>

namespace Xapian
{
class KeyMaker;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class KeyMakerControllerInternal;

class ParamDecoder;

class KeyMakerController 
{
    KeyMakerControllerInternal* mp_internal;

    public:
    KeyMakerController(Xapian::KeyMaker* km);

    /// Copy is allowed
    KeyMakerController(const KeyMakerController& src);

    KeyMakerController(KeyMakerControllerInternal* src);

    ~KeyMakerController();

    Xapian::KeyMaker* getKeyMaker();
};

XAPIAN_ERLANG_NS_END

#endif
