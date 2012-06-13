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
    MemoryManager& m_mm;

    /* Where to write now */
    char*   m_current_buf;

    /* Current size of filled data in bytes */
    size_t  m_current_len;

    /* Preallocated */
    char*   m_default_buf;
    /* Len of buffer, allocated by a port */
    size_t  m_default_len;

    size_t  m_left_len;

    /* Next segment */
    DataSegment* m_first_segment;
    DataSegment* m_last_segment;

    public:
    ResultEncoder(MemoryManager& mm) 
        : m_mm(mm), m_current_buf(NULL)
    {}

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

    DataSegment* 
    alloc(size_t size);

    void free(DataSegment*& pos);

    void put(const char* term, const size_t term_len);

    void finalize(char*);
    size_t finalSize() const;
    bool isExtended() const;

    void clear();
    void reset();
};

XAPIAN_ERLANG_NS_END
#endif
