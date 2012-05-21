#include <xapian.h>
#include <stdint.h>
#include <cstring>
#include <string>
#include <stdexcept>

#include "param_decoder.h"
#include "xapian_exception.h"

/* Helper used by ParamDecoder. */
#define READ_TYPE(T) (*(reinterpret_cast<T*>( move(sizeof(T)) )))


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/**
 * Constructor
 */
ParamDecoder::ParamDecoder(char *buf, size_t len) :
    m_buf( buf ),
    m_len( len ) {}


char* 
ParamDecoder::move(const size_t size)
{
    if (size > m_len)
        throw OverflowDriverError();
    m_len -= size;
    char* old_buf = m_buf;
    m_buf += size;
    return old_buf;
}

/**
 * Decodes <<StringLen:32/native-signed-integer, StringBin/binary>>.
 */
ParamDecoder::operator const std::string() {
    const int32_t str_len = READ_TYPE(int32_t);
    const char* str_bin = m_buf;
    move(str_len);
    const std::string str(str_bin, static_cast<size_t>(str_len));
    return str;
}

/**
 * Decodes <<Int8t:8/native-signed-integer>>.
 */
ParamDecoder::operator int8_t() {
    return READ_TYPE(int8_t);
}

/**
 * Decodes <<Int8t:8/native-unsigned-integer>>.
 */
ParamDecoder::operator uint8_t() {
    return READ_TYPE(uint8_t);
}

ParamDecoder::operator bool() {
    return READ_TYPE(uint8_t);
}

/**
 * Decodes <<Num:32/native-signed-integer>>.
 */
ParamDecoder::operator int32_t() {
    return READ_TYPE(int32_t);
}

/**
 * Decodes <<Num:32/native-unsigned-integer>>.
 * Results are Xapian::valueno or Xapian::termcount.
 */
ParamDecoder::operator uint32_t() {
    return READ_TYPE(uint32_t);
}

ParamDecoder::operator double() {
    return READ_TYPE(double);
}

ParamDecoder::operator const Xapian::Stem()
{
    const std::string&   language = *this;
    return Xapian::Stem(language);
}

char* ParamDecoder::currentPosition()
{
    return m_buf;
}

XAPIAN_ERLANG_NS_END
