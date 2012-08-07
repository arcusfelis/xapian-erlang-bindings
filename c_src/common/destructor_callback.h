#ifndef XAPIAN_DESTR_CB_H
#define XAPIAN_DESTR_CB_H

#include <stdint.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class DestructorCallback
{
    public:
    virtual void call() = 0;
    virtual ~DestructorCallback() {}
};

class ObjectBaseRegister;

class RemoveResourceDestructorCallback : public DestructorCallback
{
    typedef uint32_t Counter;
    Counter m_child_id; 
    ObjectBaseRegister* mp_child_register;

    public:
    RemoveResourceDestructorCallback(Counter child_id, ObjectBaseRegister& child_register) 
    {
        m_child_id = child_id;
        mp_child_register = &child_register;
    }
   
    ~RemoveResourceDestructorCallback() {}
    void call();
};

XAPIAN_ERLANG_NS_END
#endif
