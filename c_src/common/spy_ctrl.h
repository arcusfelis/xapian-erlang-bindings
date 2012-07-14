#ifndef XAPIAN_SPY_CTRL_H
#define XAPIAN_SPY_CTRL_H

#include <stdint.h>
#include <xapian.h>

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
 * Manage of the `Xapian::MatchSpy` object.
 * It is here, because the current version of Xapian cannot manage 
 * user-defined objects automatically.
 * It transforms passed into a constructor `Xapian::MatchSpy` object
 * into the reference-counted object.
 * Reference on the @ref SpyController, but not on `Xapian::MatchSpy`
 * will be counted.
 *
 * MatchSpy will be shared beetween clones of the @ref SpyController object.
 * When last clone will be deleted, MatchSpy will be deleted too 
 * with the delete operator.
 *
 * After using of the controlled spy in the enquire, the object will be finalized.
 * Finalized object cannot be used with a new enquire.
 * If an Erlang user try to use the finalized object, then an error will occure.
 *
 * The client C++ code checks if the object finalized or not.
 *
 * Controllers can be grouped inside the @ref ObjectRegister.
 */
class SpyController 
{
    SpyControllerInternal* mp_internal;

    /**
     * Decrease the couter inside @ref mp_internal.
     */
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

    /**
     * Returns true, if the MatchSpy was already filled.
     */
    bool is_finalized();

    /**
     * ValueCountMatchSpy uses this function.
     * Other subclasses might change the default behaviour.
     */
    virtual Xapian::valueno getSlot();
};

XAPIAN_ERLANG_NS_END

#endif
