#ifndef RESULT_ENCODER_H
#define RESULT_ENCODER_H

#include <string>
#include <stdint.h>

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
    /* Here will be a pointer on data after all operations */
    char**  m_result_buf;
    size_t  m_result_len;

    /* Preallocated */
    char*   m_default_buf;

    /* Where to write now */
    char*   m_current_buf;

    /* Len of buffer, allocated by a port */
    size_t  m_default_len;
    size_t  m_left_len;

    /* Next segment */
    DataSegment* m_first_segment;
    DataSegment* m_last_segment;

    public:

    void
    setBuffer(char** rbuf, const size_t rlen);

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

    DataSegment* 
    alloc(size_t size);

    void put(const char* term, const size_t term_len);

    operator size_t();

    void clear();
};

#endif
