/* vim: set filetype=cpp shiftwidth=4 tabstop=4 expandtab tw=80: */

/**
 * Prefix m_ (member) for properties means that property is private.
 */

// -------------------------------------------------------------------
// Includes
// -------------------------------------------------------------------

#include "result_encoder.h"
#include "xapian_exception.h"
#include <assert.h>
#include <cstdlib>
#include <cstring>


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

// -------------------------------------------------------------------
// Defines
// -------------------------------------------------------------------

/*
 * The length of free space in bytes, 
 * which will be allocatied new memory for storing result. 
 */
#define RESERVED_LEN 100

/* Helper used by ResultEncoder. */
#define SIZE_OF_SEGMENT(LEN) (sizeof(DataSegment) + (LEN) - 1)

/* Helper used by ResultEncoder. */
//#define PUT_VALUE(X) (put((char*) &(X), sizeof(X)))
#define PUT_VALUE(X) (put(reinterpret_cast<char*>( &(X) ), sizeof(X)))



/**
 * @brief A structure with a variable length.
 */
struct DataSegment
{
    /// a pointer on a next segment or NULL, if it is a last segment.
    DataSegment* next;
    /// the real size of @ref data
    size_t size;  
    /// struct-hack, flexible-array member
    char data[1]; 
};


void
ResultEncoder::setBuffer(char* rbuf, const size_t rlen) 
{
    m_default_buf = m_current_buf = rbuf;
    m_default_len = m_left_len = rlen;
    m_current_len = 0;
    m_first_segment = NULL;
}


ResultEncoder& 
ResultEncoder::operator<<(const std::string str)
{
    uint32_t len = static_cast<uint32_t>( str.length() );
    PUT_VALUE(len);
    put(str.data(), len);
    return *this;
}


ResultEncoder& 
ResultEncoder::operator<<(const char * str)
{
    uint32_t len = static_cast<uint32_t>( strlen(str) );
    PUT_VALUE(len);
    put(str, len);
    return *this;
}


ResultEncoder& 
ResultEncoder::operator<<(uint32_t value)
{
    PUT_VALUE(value);
    return *this;
}


ResultEncoder& 
ResultEncoder::operator<<(uint8_t value)
{
    PUT_VALUE(value);
    return *this;
}


ResultEncoder& 
ResultEncoder::operator<<(double value)
{
    PUT_VALUE(value);
    return *this;
}


DataSegment* 
ResultEncoder::alloc(size_t size)
{
    size = SIZE_OF_SEGMENT(size);
    void* pos = m_mm.alloc(size);
    return reinterpret_cast<DataSegment*>( pos );
}



void
ResultEncoder::free(DataSegment*& pos)
{
    m_mm.free((void*) pos);
    pos = NULL;
}



/** 
 * Copy termLen bytes from term. 
 */
void 
ResultEncoder::put(const char* term, const size_t term_len)
{
    assert(m_current_buf != NULL);

    /* Len of the whole buffer after coping */
    const size_t new_len = term_len + m_current_len;
    const bool allocated = m_first_segment != NULL;
    const bool large = allocated 
        || m_default_buf == NULL 
        || new_len > m_default_len;
    const bool no_alloc = allocated && term_len <= m_left_len;

    if (!large || no_alloc)
    {
        /* Whole term can be stored in the preallocated area. */
        char* dest = m_current_buf;
        const char* src = term;
        memcpy(dest, src, term_len);
        m_current_buf += term_len;
        m_left_len -= term_len;
    }
    else 
    {
        /* part1 will stay in a default buffer. */
        const size_t part1_len = m_left_len;
        const size_t part2_len = term_len - part1_len;
        const size_t new_segment_len = part2_len + RESERVED_LEN;

        /* Create new data segment. */
        DataSegment* 
        new_segment = alloc(new_segment_len);
        new_segment->size = new_segment_len;
        new_segment->next = NULL;

        const char* part1_src = term;
        const char* part2_src = term + part1_len;

        char* part1_dest = m_current_buf;
        char* part2_dest = new_segment->data;

        /* Copy data */
        memcpy(part1_dest, part1_src, part1_len);
        memcpy(part2_dest, part2_src, part2_len);

        if (m_first_segment == NULL) {
            /* Set as a first segment. */
            m_first_segment = new_segment;
        } else {
            /* Link to a last segment. */
            m_last_segment->next = new_segment;
        }

        /* Save state */
        m_last_segment = new_segment;
        m_current_buf = part2_dest + part2_len;
        m_left_len = RESERVED_LEN;
    }
    
    m_current_len = new_len;
}


/**
 * Clear the state of the object.
 */
void 
ResultEncoder::clear() {
    if (m_current_len > m_default_len)
    {
        do 
        {
            assert(m_first_segment != NULL);

            /* No infinite cycles. */
            assert(m_first_segment->next != m_first_segment);

            /* Use last_segment as a temp variable. */
            m_last_segment = m_first_segment;

            /* Move a pointer on the next segment. */
            m_first_segment = m_first_segment->next;

            /* Delete copied segment. */
            free(m_last_segment);
        }
        while(m_first_segment != NULL);
    }

    m_first_segment = NULL;
    m_current_buf = NULL;
    m_current_len = 0;
}


/**
 * Reset to the state before the @ref setBuffer call.
 */
void ResultEncoder::reset()
{
    clear();
    setBuffer(m_default_buf, m_default_len);
}


/**
 * Return true, if this class allocated memory.
 */
bool ResultEncoder::isExtended() const {
    return (m_current_len > m_default_len);
}


/**
 * Merge all segments together.
 * Data will be reseted.
 * @a buf is used to copy data.
 */
void ResultEncoder::finalize(char* dest) {
    assert(isExtended());
    // TODO: can be skipped
    // memset(dest, 0, m_current_len);
    char* src = m_default_buf;
    memcpy(dest, src, m_default_len);
    dest += m_default_len;

    /* Shrink data size to copy. */
    m_last_segment->size -= m_left_len;
    do 
    {
        assert(m_first_segment != NULL);

        /* No infinite cycles. */
        assert(m_first_segment->next != m_first_segment);

        const size_t segment_size = m_first_segment->size;
        assert(segment_size > 0);

        /* Real buffer length is not greater, then allocated buffer length. */
        src = m_first_segment->data;
        memcpy(dest, src, segment_size);
        dest += segment_size;

        /* Use last_segment as a temp variable. */
        m_last_segment = m_first_segment;

        /* Move a pointer on the next segment. */
        m_first_segment = m_first_segment->next;

        /* Delete copied segment. */
        assert(m_last_segment != NULL);
        free(m_last_segment);
    }
    while(m_first_segment != NULL);

    m_first_segment = NULL;
    m_current_buf = NULL;
    m_current_len = 0;
}

/**
 * Returns result
 */
size_t ResultEncoder::finalSize() const {
    return m_current_len;
}

XAPIAN_ERLANG_NS_END
