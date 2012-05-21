#include "param_decoder_controller.h"
#include "erl_driver.h"
#include "xapian_exception.h"


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

void 
ParamDecoderController::init(const char *buf, const size_t len)
{
    m_len = len;
    m_buf = static_cast<char*>( driver_alloc(len) );
    if (m_buf == NULL)
        throw MemoryAllocationDriverError(len);

    // Copy data into a buffer 
    // From buf into m_buf
    memcpy(m_buf, buf, len);
}

/**
 * Constructor
 */
ParamDecoderController::ParamDecoderController(const char *buf, const size_t len)
{
    init(buf, len);
}

ParamDecoderController::ParamDecoderController(const ParamDecoderController& prototype)
{
    init(prototype.m_buf, prototype.m_len);
}

ParamDecoderController::~ParamDecoderController()
{
    driver_free(m_buf);
}

ParamDecoderController::operator ParamDecoder() const {
    return ParamDecoder(m_buf, m_len);
}

XAPIAN_ERLANG_NS_END
