#ifndef PARAM_DECODER_CONTROLLER_H
#define PARAM_DECODER_CONTROLLER_H

// External imports
#include <cstring>

// Internal imports
#include "param_decoder.h"
#include "memory_manager.h"
#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/**
 * This object is for storing a paramDecoder buffer as a resource
 */    
class ParamDecoderController
{
    MemoryManager& m_mm;
    char *m_buf; 
    size_t m_len;

    static MemoryManager& 
    getMemoryManager(const ParamDecoderController& ctrl)
    {
        return ctrl.m_mm;
    }

    void init(const char *buf, const size_t len);

    public:
    /**
     * Constructor
     */
    ParamDecoderController(MemoryManager& mm, const char *buf, const size_t len);

    ParamDecoderController(const ParamDecoderController& prototype);

    ~ParamDecoderController();

    operator ParamDecoder() const;
};

XAPIAN_ERLANG_NS_END

#endif
