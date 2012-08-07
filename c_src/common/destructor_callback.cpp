#include "destructor_callback.h"
#include "object_register.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

void
RemoveResourceDestructorCallback::call()
{
    try
    {
        mp_child_register->remove(m_child_id);
    } catch(ElementNotFoundDriverError e)
    {}
}

XAPIAN_ERLANG_NS_END
