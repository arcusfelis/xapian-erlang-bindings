#ifndef OBJECT_REGISTER_H
#define OBJECT_REGISTER_H

#include <stdint.h>


// Google dense map can be used as a store for ObjectBaseRegister.
#ifndef GOOGLE_HASH_MAP
/* Use google hash map */
#include <google/dense_hash_map>
/* dirty code begin */
#include HASH_FUN_H
#define HASH_TPL SPARSEHASH_HASH
#define HASH_MAP google::dense_hash_map
/* dirty code end */

#else

/* Use xapian wrapper for hash map */
#include "unordered_map.h"
#define HASH_TPL std::hash
#define HASH_MAP std::unordered_map
#endif


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/**
 * It is used for storing pointers on objects of a special type (elements).
 * This class is abstract. It defines an interface.
 */
class ObjectBaseRegister
{
    protected:
    typedef uint32_t Counter;

    public:
    virtual ~ObjectBaseRegister() {};

    /**
     * Remove the element by id from the register.
     * The element will be deleted with the `delete` operator.
     */
    virtual void
    remove(Counter) = 0;


    /**
     * Return the pointer of the element by its id.
     * Returned object will be still in the set.
     */
    virtual void*
    getVoidPointer(Counter) = 0;

    /**
     * Add a new element to the set.
     * Return id, which can be used with @ref remove and @ref getVoidPointer.
     */
    virtual Counter 
    putVoidPointer(void* obj) = 0;

    /**
     * Return the pointer of the element by its id.
     * Returned object must be deallocated by its user.
     * Returned element will be no more in the set.
     * The position of this element will be reused for the new object.
     * No one element will be deleted by calling this method.
     */
    virtual void* 
    replaceWithoutCleaning(Counter num, void* new_obj) = 0;
};


/**
 * Store objects of the type @ref Child inside a register.
 * Methods from @ref ObjectBaseRegister can be used for work with a register.
 */
template <class Child>
class ObjectRegister : public ObjectBaseRegister
{
    public:

    /**
     * It is the mapping from id (of the type @ref Counter) to the 
     * reference on the object.
     *
     * Id will be generated automatically.
     */
    typedef 
    HASH_MAP< Counter, Child*, HASH_TPL<uint32_t> > Hash;

    private:
    /* Contains a number of the latest added object */
    Counter m_counter;

    /* Stores a map. */
    Hash m_elements;

    public:

    ObjectRegister();

    Child*
    get(Counter num);

    Counter 
    put(Child* obj);

    void*
    getVoidPointer(Counter num);

    Counter 
    putVoidPointer(void* obj);

    void
    remove(Counter num);

    void* 
    replaceWithoutCleaning(uint32_t num, void* new_obj);

    Hash&
    getElements()
    {
        return m_elements;
    }

    ~ObjectRegister();
};


XAPIAN_ERLANG_NS_END
#include "object_register.hpp"

#endif
