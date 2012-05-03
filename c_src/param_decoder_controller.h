#ifndef PARAM_DECODER_CONTROLLER_H
#define PARAM_DECODER_CONTROLLER_H

#include <cstring>
#include "param_decoder.h"

/**
 * This object is for storing a paramDecoder buffer as a resource
 */    
class ParamDecoderController
{
    char *m_buf; 
    size_t m_len;

    void init(const char *buf, const size_t len);

    public:
    /**
     * Constructor
     */
    ParamDecoderController(const char *buf, const size_t len);

    ParamDecoderController(const ParamDecoderController& prototype);

    ~ParamDecoderController();

    operator ParamDecoder() const;
};

#endif
