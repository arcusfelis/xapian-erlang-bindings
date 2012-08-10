#ifndef RESOURCE_GENERATOR_H
#define RESOURCE_GENERATOR_H

#include <stdint.h>
#include <vector>
#include "resource/constructor.h"

#include "xapian_config.h"

XAPIAN_ERLANG_NS_BEGIN
class ParamDecoder;
XAPIAN_ERLANG_NS_END

XAPIAN_RESOURCE_NS_BEGIN

class Generator;

class Element;
class Register;
class Generator 
{
    typedef std::vector<Constructor*> Constructors;
    /// Registered constructors
    Constructors m_constructors;

    /// Assignment operator.
    /// Assignment is not allowed.
    Generator & operator= (const Generator & /*source*/) { assert(false); return *this; }

    /// Copy constructor.
    /// Copy is not allowed.
    Generator(const Generator & /*source*/) { assert(false); }

    bool
    isExists(uint32_t constructor_num)
    {
        return (constructor_num < m_constructors.size());
    }

    public:

    Generator()
    {}

    void
    add(Constructor* p_constructor)
    {
        m_constructors.push_back(p_constructor);
    }

    Element
    create(Register& manager, ParamDecoder& params);

    ~Generator()
    {
        Constructors::iterator e, i;
        Constructor* p_constructor;

        i = m_constructors.begin();
        e = m_constructors.end();
        for (; i != e; i++)
        {
            p_constructor = *i;
            delete p_constructor;
        }
    }

    void
    registerCallbacks();

    typedef std::vector<Constructor*>::iterator constructor_iterator;
    typedef std::vector<Constructor*>::const_iterator const_constructor_iterator;

    constructor_iterator 
    constructor_begin() 
    { return m_constructors.begin(); }

    const_constructor_iterator 
    constructor_begin() const 
    { return m_constructors.begin(); }

    constructor_iterator 
    constructor_end() 
    { return m_constructors.end(); }

    const_constructor_iterator 
    constructor_end() const 
    { return m_constructors.end(); }

};

XAPIAN_RESOURCE_NS_END
#endif
