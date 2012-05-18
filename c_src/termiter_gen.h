#ifndef XAPIAN_TERM_ITER_GEN_H
#define XAPIAN_TERM_ITER_GEN_H

#include <xapian.h>
#include <stdint.h>

class TermIteratorGenerator
{
    public:
    virtual Xapian::TermIterator begin() = 0; 
    virtual Xapian::TermIterator end() = 0; 
    virtual ~TermIteratorGenerator() {}

    // If size() is unknown, return 0.
    // To check size is really 0, use empty().
    virtual Xapian::termcount size()
    {
        return 0;
    }
    virtual bool empty()
    {
        return begin() == end();
    }
};


class DocumentTermIteratorGenerator : public TermIteratorGenerator
{
    Xapian::Document m_doc;

    public:
    DocumentTermIteratorGenerator(Xapian::Document& doc) : m_doc(doc)
    {}

    Xapian::TermIterator begin()
    {
        return m_doc.termlist_begin();
    }

    Xapian::TermIterator end()
    {
        return m_doc.termlist_end();
    }

    Xapian::termcount size()
    {
        begin();
        return m_doc.termlist_count();
    }
};


class SpyControllerInternal;

/**
 * This class do a lot of stuff.
 * User can extend it.
 *
 * Methods are called from inside of the program (in qlcInit):
 * * getValueIteratorGenerator
 * * getTopValueIteratorGenerator
 *
 * They allows to create an object of type TermIteratorGenerator,
 * which can be used inside TermQlcTable.
 *
 * MatchSpy can filled only once, after that it will be finalize.
 * 
 */
class SpyController 
{
    SpyControllerInternal* mp_internal;

    void decref();

    public:
    SpyController(Xapian::MatchSpy* spy);

    /// Copy is allowed
    SpyController(const SpyController& src);

    SpyController(SpyControllerInternal* src);

    virtual ~SpyController();

    Xapian::MatchSpy* getSpy();

    /**
     * Create an object.
     * This method can be redifined by an user.
     * It is reference-counted.
     * Throw badarg error, if you can not use this MatchSet.
     */
    virtual TermIteratorGenerator*
    getValueIteratorGenerator();

    virtual TermIteratorGenerator*
    getTopValueIteratorGenerator(uint32_t maxvalues);

    /**
     * Call it after using with add_matchspy.
     */
    void finalize();

    bool is_finalized();
};



class ValueCountSpyController : public SpyController
{
    public:
    /// Passed spy will be deallocated by system
    ValueCountSpyController(Xapian::ValueCountMatchSpy* spy);

    TermIteratorGenerator* getValueIteratorGenerator();

    TermIteratorGenerator*
    getTopValueIteratorGenerator(uint32_t maxvalues);
};

#endif
