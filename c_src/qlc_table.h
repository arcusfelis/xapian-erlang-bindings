#ifndef QLC_TABLE_H
#define QLC_TABLE_H

#include "param_decoder.h"
#include "param_decoder_controller.h"

#include <xapian.h>
#include <string>
#include <cstring>

class XapianErlangDriver;

class QlcTable
{
    protected:
    XapianErlangDriver& m_driver;

    public:
    QlcTable(XapianErlangDriver& driver);

    virtual
    ~QlcTable();
    
    virtual uint32_t
    numOfObjects() const = 0;

    virtual void
    getPage(uint32_t from, uint32_t count) const = 0;

    virtual void
    lookup(ParamDecoder& driver_params) const = 0;
};


class MSetQlcTable : public QlcTable
{
    Xapian::MSet m_mset;
    const ParamDecoderController m_controller;

    public:
    MSetQlcTable(XapianErlangDriver& driver, 
        Xapian::MSet& mset, const ParamDecoderController& controller);

    uint32_t numOfObjects() const;

    void getPage(uint32_t from, uint32_t count) const;

    void lookup(ParamDecoder& driver_params) const;
};


#endif
