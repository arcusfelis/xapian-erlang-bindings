#ifndef QLC_TABLE_H
#define QLC_TABLE_H

#include "param_decoder.h"
#include "param_decoder_controller.h"

#include <xapian.h>
#include <string>
#include <cstring>
#include "termiter_gen.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class Driver;
class ResultEncoder;

/**
 * This class is used as the namespace for enum @ref QlcValidObjectType.
 * So, we can use QlcType::MSET.
 */
class QlcType
{
public:
    enum QlcValidObjectType
    {
        MSET            = 0,
        TERMS           = 1,
        SPY_TERMS       = 2
    };
};

/**
 * QLCTables provide data for QLC.
 * QLC is an Erlang interface for iteration trough a set of objects.
 */
class QlcTable
{

    protected:
    Driver& m_driver;

    public:
    /**
     * Flags, that signal about the end of the list.
     */
    static const uint8_t MORE = 1, STOP = 0;

    /**
     * Objects can use 2 encoding schemas:
     *
     * - With known size:
     *      Objects are coded as `Size(3) Obj1 Obj2 Obj3`;
     * - With unknown size:
     *      Objects are coded as `Obj1 MORE Obj2 MORE Obj3 STOP`.
     */
    static const uint8_t UNKNOWN_SIZE = 0, KNOWN_SIZE = 1;

    QlcTable(Driver& driver);

    /**
     * It is virtual, because other methods are virtual.
     */
    virtual
    ~QlcTable();
    
    /**
     * It counts the total count of objects in the set (in the table).
     */
    virtual uint32_t
    size() = 0;

    /**
     * It allows paganation.
     */
    virtual void
    getPage(ResultEncoder&, uint32_t from, uint32_t count) = 0;

    /**
     * Select objects by a key (by an index).
     */
    virtual void
    lookup(PR) = 0;
};


/**
 * This table is based on @ref Xapian::Mset.
 */
class MSetQlcTable : public QlcTable
{
    Xapian::MSet m_mset;
    const ParamDecoderController m_controller;

    public:
    MSetQlcTable(Driver& driver, 
        Xapian::MSet& mset, const ParamDecoderController& controller);

    uint32_t size();

    void getPage(ResultEncoder&, uint32_t from, uint32_t count);

    void lookup(PR);
};


/**
 * This table is base on @ref TermIteratorGenerator.
 * This table is used for iterating terms in a document or for 
 * iterating values in MatchSpy.
 * @ref TermIteratorGenerator knows how to init 
 * @ref Xapian::TermIterator iterators.
 */
class TermQlcTable : public QlcTable
{
    // Don't change m_end value
    Xapian::TermIterator m_iter, m_end;
    TermIteratorGenerator* mp_gen;
    uint32_t m_current_pos, m_size;
    
    const ParamDecoderController m_controller;
                     
    public:

    /**
     * @a gen variable will be deallocated by the system (not by a programmer!).
     * @a gen and the new object have the same live-time.
     */
    TermQlcTable(Driver& driver, 
        TermIteratorGenerator* gen, 
        const ParamDecoderController& controller);

    
    ~TermQlcTable();

    /**
     * Calculate, how many objects in the set.
     * Return zero if unknown.
     */
    uint32_t size();

    void getPage(ResultEncoder&, uint32_t from, uint32_t count);

    void lookup(PR);

    // Helpers
    private:
    /**
     * Skip @a skip objects from the beginning.
     * It is used, when the total size of objects is not yet known.
     */
    void goToAndCheckBorder(const uint32_t skip);

    /**
     * Skip @a skip objects from the beginning.
     * It is used, when the total size of objects is known.
     */
    void goTo(const uint32_t skip);

    /**
     * Skip @a skip objects from the beginning and return objects.
     * Maximum @a count object will be returned.
     * It is used, when the total size of objects is not yet known.
     */
    void getPageKnownSize(
            ResultEncoder&, const uint32_t skip, const uint32_t count);

    /**
     * Skip @a skip objects from the beginning and return objects.
     * Maximum @a count object will be returned.
     * It is used, when the total size of objects is known.
     */
    void getPageUnknownSize(
            ResultEncoder&, const uint32_t skip, const uint32_t count);
};

XAPIAN_ERLANG_NS_END
#endif
