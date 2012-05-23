#ifndef XAPIAN_SPY_CTRL_H
#define XAPIAN_SPY_CTRL_H

#include <stdint.h>

namespace Xapian
{
class MatchSpy;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class SpyControllerInternal;
class TermIteratorGenerator;

class ParamDecoder;

/**
 * @brief Controller of `Xapian::MatchSet`.
 *
 * This class do a lot of stuff.
 * User can extend it for working other variants of `MatchSpy`.
 *
 * @ref getIteratorGenerator allows to create an object of type 
 * @ref TermIteratorGenerator,
 * which can be used inside @ref TermQlcTable.
 *
 * If the `MatchSet` cannot be used as TermGenerator, do not overwrite 
 * these methods. If the Erlang user will try to use this `MatchSpy` as
 * a term iterator, an exception will be thrown.
 *
 * `MatchSpy` can filled only once, after that it will be @ref finalize.
 *
 * Objects of this type are reference-counted.
 * `MatchSpy` will be deleted in the destructor.
 */
class SpyController 
{
    SpyControllerInternal* mp_internal;

    void decref();

    public:
    SpyController(Xapian::MatchSpy* spy);

    /// Copy is allowed
    SpyController(const SpyController& src);

    SpyController(SpyControllerInternal* src);

    virtual ~SpyController();

    Xapian::MatchSpy* getSpy();

    /**
     * Create an object.
     * This method can be redifined by an user.
     * It is reference-counted.
     * Throw badarg error, if you can not use this `MatchSet`.
     */
    virtual TermIteratorGenerator*
    getIteratorGenerator(ParamDecoder& param);

    /**
     * Call it after using with `add_matchspy`.
     */
    void finalize();

    bool is_finalized();
};

XAPIAN_ERLANG_NS_END

#endif
