#include "destructor_callback.h"
#include "object_register.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

void
RemoveResourceDestructorCallback::call()
{
    m_child_register->remove(m_child_id);
}

XAPIAN_ERLANG_NS_END
