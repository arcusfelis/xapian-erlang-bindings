#ifndef QLC_TABLE_RCTRL_H
#define QLC_TABLE_RCTRL_H

#include "resource/controller/base.h"
#include "qlc.h"

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class QlcTable : public Base
{
    XAPIAN_ERLANG_NS::QlcTable* mp_table;

    public:
    QlcTable(XAPIAN_ERLANG_NS::QlcTable* p_table) : mp_table(p_table) {}
    ~QlcTable() { delete mp_table; }

    operator XAPIAN_ERLANG_NS::QlcTable&()
    {
        return *mp_table;
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
