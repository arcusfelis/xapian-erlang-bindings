#ifndef OBJECT_REGISTER_H
#define OBJECT_REGISTER_H

#include <stdint.h>
#include <vector>


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

#include "destructor_callback.h"
#include "xapian_context.h"

#include "xapian_config.h"
#include <stdio.h>

/* FOR THE SECOND PART */
/* For min */
#include <algorithm>
#include "xapian_exception.h"
XAPIAN_ERLANG_NS_BEGIN

// ------------------------------------------------------------------
// Helper
// ------------------------------------------------------------------


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
    virtual void clear() = 0;

    /**
     * Remove the element by id from the register.
     * The element will be deleted with the `delete` operator.
     */
    virtual void
    remove(Counter) = 0;

    virtual void
    attach(Counter parent_id, 
           ObjectBaseRegister& child_register, Counter child_id) = 0;


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
};

template <class Child>
class RegisterElement
{
    typedef std::vector<DestructorCallback*> DestructorCallbacks;
    DestructorCallbacks m_dcbs;
    Child* m_child;

    public:
    RegisterElement(Child* child)
    {
        m_child = child;
    }

    ~RegisterElement()
    {
        // Erase the resource
        delete m_child;

        typename DestructorCallbacks::iterator i, e, b;
        b = m_dcbs.begin();
        e = m_dcbs.end();
        for(i = b; i != e; i++)
        {
            DestructorCallback* cb = *i;
            // Call callback
            cb->call();
            // Delete its object
            delete cb;
        }
    }

    Child* getChild()
    {
        return m_child;
    }

    void attachDestructorCallback(DestructorCallback* cb)
    {
        m_dcbs.push_back(cb);
    }

    /**
     * Copy data from the passed context to this element.
     */
    void attachContext(XapianContext& context)
    {
         context.moveAttached(m_dcbs);
    }
};


/**
 * Store objects of the type @ref Child inside a register.
 * Methods from @ref ObjectBaseRegister can be used for work with a register.
 */
template <class Child>
class ObjectRegister : public ObjectBaseRegister
{
    /**
     * It is the mapping from id (of the type @ref Counter) to the 
     * reference on the object.
     *
     * Id will be generated automatically.
     */
    typedef 
    HASH_MAP< Counter, RegisterElement<Child>*, HASH_TPL<uint32_t> > Hash;

    private:
    /* Contains a number of the latest added object */
    Counter m_counter;

    /* Stores a map. */
    Hash m_elements;

    public:
    
    typedef 
    HASH_MAP< Counter, Child*, HASH_TPL<uint32_t> > PublicHash;

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

    void
    attach(Counter parent_id, 
           ObjectBaseRegister& child_register, Counter child_id);

    void
    attachContext(Counter parent_id, XapianContext& context);

    PublicHash
    getElements()
    {
        PublicHash elems;
        elems.set_empty_key(0);
        elems.set_deleted_key(1);

        typename Hash::iterator i, e, b;
        b = m_elements.begin();
        e = m_elements.end();

        // Copy elements (pointers) without meta information
        for(i = b; i != e; i++)
            elems[i->first] = i->second->getChild();

        return elems;
    }

    ~ObjectRegister();
    void clear();
};




template <class Child>
ObjectRegister<Child>::ObjectRegister()
{
    m_elements.set_empty_key(0);
    m_elements.set_deleted_key(1);
    m_counter = 1;
}

template <class Child>
typename ObjectRegister<Child>::Counter 
ObjectRegister<Child>::put(Child* obj)
{
    m_counter++;
    m_elements[m_counter] = new RegisterElement<Child>(obj);

    return m_counter;
}

template <class Child>
void
ObjectRegister<Child>::remove(Counter num)
{
    typename Hash::iterator i; 
    i = m_elements.find(num);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(num);

    delete i->second;
    m_elements.erase(i);
}


/**
 * Tell, that if the resource with id `parent_id` was be deleted from this
 * register, than the object with id `child_id` from register `child_register`.
 */
template <class Child>
void
ObjectRegister<Child>::attach(
   Counter parent_id, 
   ObjectBaseRegister& child_register, 
   Counter child_id)
{
    typename Hash::iterator i; 
    i = m_elements.find(parent_id);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(parent_id);

    DestructorCallback* p_cb = 
        new RemoveResourceDestructorCallback(child_id, child_register);
    i->second->attachDestructorCallback(p_cb);
}

/**
 * Move data from the context to the element with the `parent_id` id.
 */
template <class Child>
void
ObjectRegister<Child>::attachContext(Counter parent_id, XapianContext& context)
{
    typename Hash::iterator i; 
    i = m_elements.find(parent_id);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(parent_id);

    i->second->attachContext(context);
}

template <class Child>
Child*
ObjectRegister<Child>::get(Counter num) 
{
    typename Hash::iterator i; 
    i = m_elements.find(num);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(num);

    return i->second->getChild();
}


template <class Child>
void*
ObjectRegister<Child>::getVoidPointer(Counter num)
{
    return (void*) get(num);
}


template <class Child>
typename ObjectRegister<Child>::Counter 
ObjectRegister<Child>::putVoidPointer(void* obj)
{
    return put(static_cast<Child*>(obj));
}


template <class Child>
ObjectRegister<Child>::~ObjectRegister()
{
    clear();
}


/**
 * This method is called from m_stores.clear, that is called from driver.clear.
 * It is called for all ObjectRegisters twice: the first tile, it is called 
 * while all other registers alive (directly), the second time, when they
 * are alive partically (from ~ObjectRegister).
 */
template <class Child>
void
ObjectRegister<Child>::clear()
{
    typename Hash::iterator i, e, b;
    b = m_elements.begin();
    e = m_elements.end();
    for(i = b; i != e; i++)
    {
        delete i->second;
    }
    m_elements.clear();
}

XAPIAN_ERLANG_NS_END
#endif
