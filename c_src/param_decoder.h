#ifndef PARAM_DECODER_H
#define PARAM_DECODER_H

#include <stdint.h>
#include <string>


namespace Xapian
{
class Stem;
}

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/**
 * Decoder of the parameters from erlang's calls
 */
class ParamDecoder
{
    /**
     * A current position of the pointer.
     * Every move forward will increase this variable.
     */
    char *m_buf; 

    /**
     * A length in bytes of the remained buffer.
     * Every move forward will decrease this variable.
     */
    size_t m_len;

    public:
    ParamDecoder(char *buf, size_t len);

    /**
     * Skip the pointer on @ref size_t bytes.
     */
    char* move(const size_t size);

    /*! \name Retrieves a variable from the buffer and moves the pointer on the next variable. */
    /*! \{ */
    operator const std::string();
    operator int8_t();
    operator uint8_t();
    operator bool();
    operator int32_t(); 
    operator uint32_t();
    operator double(); 
    operator const Xapian::Stem();
    /*! \} */

    /**
     * Return a pointer on the current pointer position.
     */
    char* currentPosition();
};

XAPIAN_ERLANG_NS_END
#endif
