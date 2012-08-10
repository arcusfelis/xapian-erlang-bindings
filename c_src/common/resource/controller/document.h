#ifndef DOCUMENT_RCTRL_H
#define DOCUMENT_RCTRL_H

#include "resource/controller/base.h"
#include <xapian.h>

#include "xapian_config.h"

XAPIAN_RESOURCE_CTRL_NS_BEGIN

class Document : public Base
{
    Xapian::Document* mp_km;

    public:
    Document(Xapian::Document* p_km) : mp_km(p_km) {}
    ~Document() { delete mp_km; }

    virtual operator Xapian::Document&()
    {
        return *mp_km;
    }

    std::string type()
    {
        return "Resource::Document";
    }
};

XAPIAN_RESOURCE_CTRL_NS_END
#endif
