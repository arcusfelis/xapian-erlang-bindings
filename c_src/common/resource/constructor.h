#ifndef RESOURCE_CONSTRUCTOR_H
#define RESOURCE_CONSTRUCTOR_H

#include <stdint.h>
#include <string.h>
#include "resource/element.h"
#include "resource/register.h"
#include "param_decoder.h"

#include "xapian_config.h"

XAPIAN_ERLANG_NS_BEGIN
class Driver;   
XAPIAN_ERLANG_NS_END  


XAPIAN_RESOURCE_NS_BEGIN

class Constructor 
{
    public:
    /**                                                                             
     *  CreatePureResourceFn is a function for calling new Constructor of
     *  a resource.
     */                                                                             
    typedef Element (*CreatePureResourceFn)                                 
            (Register& manager, ParamDecoder& params);                           

    /**                                                                             
     *  CreateDriverResourceFn is a function for calling new Constructor of
     *  a resource.
     */                                                                             
    typedef Element (*CreateDriverResourceFn)                                 
            (Driver&, Register& manager, ParamDecoder& params);                           

    virtual
    Element call(Register& manager, ParamDecoder& params) = 0;

    virtual
    std::string name() = 0;

    virtual
    ~Constructor() {}

    /**
     * Returned value must be deleted by client.
     */
    static Constructor*
    create(std::string name, CreatePureResourceFn p_fun);

    static Constructor*
    create(Driver& driver, std::string name, CreateDriverResourceFn p_fun);
};

class PureConstructor : public Constructor
{
    CreatePureResourceFn mp_fun;
    std::string m_name;

    public:
    PureConstructor(std::string name, CreatePureResourceFn p_fun) 
        : mp_fun(p_fun), m_name(name) { }

    Element call(Register& manager, ParamDecoder& params)
    {
        return mp_fun(manager, params);
    }

    std::string name() { return m_name; }

    ~PureConstructor() {}
};

class DriverConstructor : public Constructor
{
    CreateDriverResourceFn mp_fun;
    std::string m_name;
    Driver& m_driver;

    public:
    DriverConstructor(Driver& driver, std::string name, 
                      CreateDriverResourceFn p_fun) 
        : mp_fun(p_fun), m_name(name), m_driver(driver) { }

    Element call(Register& manager, ParamDecoder& params)
    {
        return mp_fun(m_driver, manager, params);
    }

    std::string name() { return m_name; }

    ~DriverConstructor() {}
};

XAPIAN_RESOURCE_NS_END
#endif
