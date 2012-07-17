// looks just like a cpp file, but it isn't

/* For min */
#include <algorithm>
#include "xapian_exception.h"


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN


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
    m_elements[m_counter] = obj;

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


template <class Child>
Child*
ObjectRegister<Child>::get(Counter num) 
{
    typename Hash::iterator i; 
    i = m_elements.find(num);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(num);

    return i->second;
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
    typename Hash::iterator i, e, b;
    b = m_elements.begin();
    e = m_elements.end();
    for(i = b; i != e; i++)
    {
        delete i->second;
    }
}

template <class Child> 
void*
ObjectRegister<Child>::replaceWithoutCleaning(uint32_t num, void* new_obj)
{
    typename Hash::iterator i; 
    i = m_elements.find(num);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(num);

    void* old_obj = (void*) i->second;

    // A type iterator can be used to modify the value of an element.
    // http://msdn.microsoft.com/en-us/library/exbyc388%28v=vs.80%29.aspx
    i->second = static_cast<Child*>(new_obj);

    return old_obj;
}

XAPIAN_ERLANG_NS_END
