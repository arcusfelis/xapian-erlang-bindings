#ifndef OBJECT_REGISTER_H
#define OBJECT_REGISTER_H

#include <stdint.h>

#include <google/dense_hash_map>
/* dirty code begin */
#include HASH_FUN_H
#define HASH_TPL SPARSEHASH_HASH
/* dirty code end */


class ObjectBaseRegister
{
    protected:
    typedef uint32_t Counter;

    public:
    virtual void
    remove(Counter) = 0;

    virtual void*
    getVoidPointer(Counter) = 0;

    virtual Counter 
    putVoidPointer(void* obj) = 0;
};


template <class Child>
class ObjectRegister : public ObjectBaseRegister
{
    public:
    typedef 
    google::dense_hash_map< uint32_t, Child*, HASH_TPL<uint32_t> > Hash;

    private:
    /* Contains a number of the latest added object */
    Counter m_counter;
    Hash m_elements;

    public:

    ObjectRegister();

    Child*
    get(Counter num);

    Counter 
    put(Child* obj);

    void*
    getVoidPointer(Counter num);

    virtual Counter 
    putVoidPointer(void* obj);

    void
    remove(Counter num);

    Hash&
    getElements()
    {
        return m_elements;
    }

    ~ObjectRegister();
};

#include "object_register.hpp"

#endif
