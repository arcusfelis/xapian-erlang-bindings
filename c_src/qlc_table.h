#ifndef QLC_TABLE_H
#define QLC_TABLE_H

#include "param_decoder.h"
#include "param_decoder_controller.h"

#include <xapian.h>
#include <string>
#include <cstring>

class XapianErlangDriver;

class QlcType
{
public:
    enum QlcValidObjectType
    {
        MSET            = 0,
        TERMS           = 1
    };
};

class QlcTable
{
    protected:
    XapianErlangDriver& m_driver;

    public:
    QlcTable(XapianErlangDriver& driver);

    virtual
    ~QlcTable();
    
    virtual uint32_t
    numOfObjects() = 0;

    virtual void
    getPage(uint32_t from, uint32_t count) = 0;

    virtual void
    lookup(ParamDecoder& driver_params) = 0;
};


class MSetQlcTable : public QlcTable
{
    Xapian::MSet m_mset;
    const ParamDecoderController m_controller;

    public:
    MSetQlcTable(XapianErlangDriver& driver, 
        Xapian::MSet& mset, const ParamDecoderController& controller);

    uint32_t numOfObjects();

    void getPage(uint32_t from, uint32_t count);

    void lookup(ParamDecoder& driver_params);
};


class TermQlcTable : public QlcTable
{
    Xapian::Document m_doc;
    Xapian::TermIterator m_iter;
    uint32_t m_current_pos;
    
    const ParamDecoderController m_controller;

    public:
    TermQlcTable(XapianErlangDriver& driver, 
        Xapian::Document& doc, const ParamDecoderController& controller);

    uint32_t numOfObjects();

    void getPage(uint32_t from, uint32_t count);

    void lookup(ParamDecoder& driver_params);
};


#endif
