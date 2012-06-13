#include "param_decoder_controller.h"
#include "xapian_exception.h"


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

void 
ParamDecoderController::init(const char *buf, const size_t len)
{
    m_len = len;
    m_buf = reinterpret_cast<char*>(m_mm.alloc(len));

    // Copy data into a buffer 
    // From buf into m_buf
    memcpy(m_buf, buf, len);
}

/**
 * Constructor
 */
ParamDecoderController::ParamDecoderController(MemoryManager& mm, 
        const char *buf, const size_t len) 
    : m_mm(mm)
{
    init(buf, len);
}

ParamDecoderController::ParamDecoderController(
        const ParamDecoderController& prototype) 
    : m_mm(getMemoryManager(prototype))
{
    init(prototype.m_buf, prototype.m_len);
}

ParamDecoderController::~ParamDecoderController()
{
    m_mm.free(m_buf);
}

ParamDecoderController::operator ParamDecoder() const {
    return ParamDecoder(m_buf, m_len);
}

XAPIAN_ERLANG_NS_END
