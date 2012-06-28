#ifndef RESULT_ENCODER_H
#define RESULT_ENCODER_H

// External imports
#include <string>
#include <stdint.h>

// Internal imports
#include "xapian_config.h"
#include "memory_manager.h"
XAPIAN_ERLANG_NS_BEGIN

typedef struct DataSegment DataSegment;
struct DataSegment;

// -------------------------------------------------------------------
// Result encoder: appends variables to the buffer
// -------------------------------------------------------------------

/**
 * If the driver wants to return data, it should return it in rbuf. 
 * When control is called, *rbuf points to a default buffer of rlen 
 * bytes, which can be used to return data. Data is returned different 
 * depending on the port control flags (those that are set with 
 * set_port_control_flags). 
 */
class ResultEncoder
{
    /* It knows, how to allocate/deallocate memory. */
    MemoryManager& m_mm;

    /* Where to write now */
    char*   m_current_buf;

    /* Current size of filled data in bytes */
    size_t  m_current_len;

    /* Preallocated */
    char*   m_default_buf;
    /* Len of buffer, allocated by a port */
    size_t  m_default_len;

    /* How many bytes were allocated, but not used. */
    size_t  m_left_len;

    /* Next segment */
    /* NULL, if no memory was allocated. */
    DataSegment* m_first_segment;

    /* Can has undefined value, if no memory was allocated. */
    DataSegment* m_last_segment;


    /**
     * Allocate a new data segment and return a pointer on it. 
     */
    DataSegment* 
    alloc(size_t size);

    /**
     * Deallocate the data segment. 
     */
    void free(DataSegment*& pos);

    public:
    ResultEncoder(MemoryManager& mm) 
        : m_mm(mm), m_current_buf(NULL)
    {}

    /**
     * Set the internal pointer on the client-managed block of data. 
     */
    void
    setBuffer(char* rbuf, const size_t rlen);

    ResultEncoder(MemoryManager& mm, char* rbuf, const size_t rlen) 
        : m_mm(mm)
    {
        setBuffer(rbuf, rlen);
    }

    /*! \name Appends variables to the buffer. */
    /*! \{ */
    ResultEncoder& 
    operator<<(const std::string str);

    ResultEncoder& 
    operator<<(const char * str);

    ResultEncoder& 
    operator<<(uint32_t value);

    ResultEncoder& 
    operator<<(uint8_t value);

    ResultEncoder& 
    operator<<(double value);
    /*! \} */

    bool maybe(bool is_exists);

    /**
     * It is used by `<<` operators.
     * Appends a block of data of size @a term_len to the result.
     */
    void put(const char* term, const size_t term_len);

    void finalize(char*);
    size_t finalSize() const;

    /** 
     * Returns true, if the existed buffer is too small.
     */
    bool isExtended() const;

    void clear();
    void reset();
};

XAPIAN_ERLANG_NS_END
#endif
