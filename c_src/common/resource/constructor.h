#ifndef RESOURCE_CONSTRUCTOR_H
#define RESOURCE_CONSTRUCTOR_H

#include <stdint.h>
#include <string.h>
#include "resource/element.h"
#include "resource/register.h"
#include "param_decoder.h"

#include "xapian_config.h"

XAPIAN_RESOURCE_NS_BEGIN

class Constructor 
{
    /**                                                                             
     *  CreateUserResourceFn is a function for calling new Constructor of
     *  a resource.
     */                                                                             
    typedef Element (*CreateResourceFn)                                 
            (Register& manager, ParamDecoder& params);                           

    CreateResourceFn mp_fun;
    std::string m_name;

    public:
    Constructor(std::string name, CreateResourceFn p_fun) 
        : m_name(name), mp_fun(p_fun) { }

    Element call(Register& manager, ParamDecoder& params)
    {
        return mp_fun(manager, params);
    }

    std::string name() { return m_name; }
};

XAPIAN_RESOURCE_NS_END
#endif
