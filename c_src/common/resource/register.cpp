#include "resource/register.h"
#include "xapian_exception.h"

XAPIAN_RESOURCE_NS_BEGIN

Register::
Register()
{
    m_elements.set_empty_key(0);
    m_elements.set_deleted_key(1);
    m_counter = 1;
}

Register::Counter 
Register::
put(Element elem)
{
    m_counter++;
    m_elements[m_counter] = elem;

    return m_counter;
}

Element 
Register::
get(Register::Counter num)
{
    typename Hash::iterator i; 
    i = m_elements.find(num);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(num);

    return i->second;
}

void
Register::
remove(Register::Counter num)
{
    typename Hash::iterator i; 
    i = m_elements.find(num);

    if (i == m_elements.end())
        throw ElementNotFoundDriverError(num);

    m_elements.erase(i);
}


Register::
~Register()
{
    clear();
}


/**
 * This method is called from m_stores.clear, that is called from driver.clear.
 * It is called for all ObjectRegisters twice: the first tile, it is called 
 * while all other registers alive (directly), the second time, when they
 * are alive partically (from ~ObjectRegister).
 */
void
Register::
clear()
{
    m_elements.clear();
}

XAPIAN_RESOURCE_NS_END
