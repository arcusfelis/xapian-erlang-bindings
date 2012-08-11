#ifndef RESOURCE_REGISTER_H
#define RESOURCE_REGISTER_H

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

#include "resource/element.h"

#include "xapian_config.h"

XAPIAN_RESOURCE_NS_BEGIN

class Register
{
    typedef uint32_t Counter;
    /**
     * It is the mapping from id (of the type @ref Counter) to the 
     * reference on the object.
     *
     * Id will be generated automatically.
     */
    typedef 
    HASH_MAP< Counter, Element, HASH_TPL<uint32_t> > Hash;

    private:
    /* Contains a number of the last added object */
    Counter m_counter;

    /* Stores a map. */
    Hash m_elements;

    public:

    Register();

    Element
    get(Counter num);

    Counter 
    put(Element);

    void
    remove(Counter num);

    void
    removeAny(Counter num);

    ~Register();
    void clear();
};

XAPIAN_RESOURCE_NS_END
#endif
