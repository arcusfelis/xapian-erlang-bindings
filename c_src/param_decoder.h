#ifndef PARAM_DECODER_H
#define PARAM_DECODER_H

#include <stdint.h>
#include <string>

namespace Xapian
{
class Stem;
}

// -------------------------------------------------------------------
// Decoder of the parameters from erlang's calls
// -------------------------------------------------------------------

class ParamDecoder
{
    char *m_buf; 
    size_t m_len;

    public:
    ParamDecoder(char *buf, size_t len);

    char* 
    move(const size_t size);

    operator const std::string();
    operator int8_t();
    operator uint8_t();
    operator bool();
    operator int32_t(); 
    operator uint32_t();
    operator double(); 
    operator const Xapian::Stem();
    char* currentPosition();
};

#endif
