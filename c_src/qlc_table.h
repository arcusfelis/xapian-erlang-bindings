#ifndef QLC_TABLE_H
#define QLC_TABLE_H

#include "param_decoder.h"
#include "param_decoder_controller.h"

#include <xapian.h>
#include <string>
#include <cstring>
#include "termiter_gen.h"

class XapianErlangDriver;

class QlcType
{
public:
    enum QlcValidObjectType
    {
        MSET            = 0,
        TERMS           = 1,
        SPY_TERMS       = 2,
        TOP_SPY_TERMS   = 3
    };
};

class QlcTable
{

    protected:
    XapianErlangDriver& m_driver;

    public:
    // Flags, that signal about end of list.
    static const uint8_t MORE = 1, STOP = 0;
    static const uint8_t UNKNOWN_SIZE = 0, KNOWN_SIZE = 1;

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
    // Don't change m_end value
    Xapian::TermIterator m_iter, m_end;
    TermIteratorGenerator* mp_gen;
    uint32_t m_current_pos, m_size;
    
    const ParamDecoderController m_controller;
                     
    public:

    /**
     * gen variable will be deallocated by system (not a programmer!).
     */
    TermQlcTable(XapianErlangDriver& driver, 
        TermIteratorGenerator* gen, 
        const ParamDecoderController& controller);

    
    ~TermQlcTable() 
    {
        delete mp_gen;
    }

    /**
     * Return zero if unknown.
     */
    uint32_t numOfObjects();

    void getPage(uint32_t from, uint32_t count);

    void lookup(ParamDecoder& driver_params);

    // Helpers
    private:
    void goToAndCheckBorder(const uint32_t skip);
    void goTo(const uint32_t skip);
    void getPageKnownSize(const uint32_t skip, const uint32_t count);
    void getPageUnknownSize(const uint32_t skip, const uint32_t count);
};


#endif
