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
};


template <class Child>
class ObjectRegister : public ObjectBaseRegister
{
    typedef 
    google::dense_hash_map< uint32_t, Child*, HASH_TPL<uint32_t> > Hash;

    /* Contains a number of the latest added object */
    Counter m_counter;
    Hash m_elements;
    
    public:
    ObjectRegister();

    Counter 
    put(Child* obj);

    void
    remove(Counter num);

    Child*
    get(Counter num);

    ~ObjectRegister();
};

#include "object_register.hpp"

#endif
