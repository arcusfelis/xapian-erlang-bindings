// looks just like a cpp file, but it isn't

/* For min */
#include <algorithm>
#include "xapian_exception.h"


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

    m_elements.erase(i);
    delete i->second;
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
