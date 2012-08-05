#ifndef XAPIAN_CONTEXT_H
#define XAPIAN_CONTEXT_H

#include <stdint.h>
#include <xapian.h>

#include "xapian_config.h"
#include "destructor_callback.h"
XAPIAN_ERLANG_NS_BEGIN

class XapianContext
{
    typedef std::vector<DestructorCallback*> DestructorCallbacks;
    typedef uint32_t Counter;
    DestructorCallbacks m_dcbs;

    public:
    ~XapianContext()
    {
        typename DestructorCallbacks::iterator i, e, b;
        b = m_dcbs.begin();
        e = m_dcbs.end();
        for(i = b; i != e; i++)
        {
            DestructorCallback* cb = *i;
            // Call callback
            cb->call();
            // Delete its object
            delete cb;
        }
    }

    void attachDestructorCallback(DestructorCallback* cb)
    {
        m_dcbs.push_back(cb);
    }

    /**
     * @a child_id will be deleted from @a child_register after destruction 
     * of this object.
     */
    void attach(ObjectBaseRegister& child_register, Counter child_id)
    {
        DestructorCallback* p_cb = 
            new RemoveResourceDestructorCallback(child_id, child_register);
        attachDestructorCallback(p_cb);
    }

    /**
     * Move collected callbacks to passed vector.
     */
    void moveAttached(DestructorCallbacks& to)
    {
        typename DestructorCallbacks::iterator i, e, b;
        b = m_dcbs.begin();
        e = m_dcbs.end();
        for(i = b; i != e; i++)
        {
            DestructorCallback* cb = *i;
            to.push_back(cb);
        }
        // Erase callbacks (without calling!)
        m_dcbs.clear();
    }
};

XAPIAN_ERLANG_NS_END

#endif

