#include "xapian_drvctrl.h"
#include "xapian_exception.h"
#include "xapian_core.h"
#include "memory_drvmgr.h"

#include "param_decoder.h"
#include "result_encoder.h"
#include "user_resources.h"

#include <assert.h>

// -------------------------------------------------------------------
// Globals
// -------------------------------------------------------------------

XAPIAN_ERLANG_NS_BEGIN

ResourceGenerator* gp_generator = NULL;
MemoryManager* gp_driverMemoryManager = NULL;

/**
 * Create global variables
 */
int 
DriverController::init()
{
    if (gp_generator == NULL)
    {
        gp_generator = new ResourceGenerator();
        registerUserCallbacks(*gp_generator);
    }
    if (gp_driverMemoryManager == NULL)
    {
        gp_driverMemoryManager = new DriverMemoryManager();
    }
    return 0;
}


/**
 * Delete global variables
 */
void 
DriverController::finish()
{
    if (gp_generator != NULL)
        delete gp_generator;

    if (gp_driverMemoryManager != NULL)
        delete gp_driverMemoryManager;

    gp_driverMemoryManager = NULL;
    gp_generator = NULL;
}


ErlDrvData 
DriverController::start(
    ErlDrvPort port, 
    char* /* buf */)
{
    /* If the flag is set to PORT_CONTROL_FLAG_BINARY, 
       a binary will be returned. */       
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY); 
    assert(gp_generator != NULL);
    Driver* drv_data = new Driver(*gp_driverMemoryManager, *gp_generator);
    return reinterpret_cast<ErlDrvData>( drv_data );
}


void 
DriverController::stop(
    ErlDrvData drv_data) 
{
    Driver* 
    drv = reinterpret_cast<Driver*>( drv_data );

    drv->clear();

    if (drv != NULL)
        delete drv;
} 



ErlDrvSSizeT 
DriverController::control(
    ErlDrvData drv_data, 
    const unsigned int  command, 
    char*         buf, 
    ErlDrvSizeT   e_len, 
    char**        rbuf, 
    ErlDrvSizeT   e_rlen)
{
    const size_t len  = static_cast<int>(e_len);
    const size_t rlen = static_cast<int>(e_rlen);

    Driver& drv = * reinterpret_cast<Driver*>( drv_data );

    ParamDecoder params(buf, len); 
    ResultEncoder result(*gp_driverMemoryManager, *rbuf, rlen);
    drv.handleCommand(params, result, command);

    ErlDrvSSizeT result_len = result.finalSize();

    /* It is too long for rbuf, create new binary. */
    if (result.isExtended())
    {
        ErlDrvBinary* bin = driver_alloc_binary(result_len);
        if (bin == NULL)
            throw MemoryAllocationDriverError(result_len);
        result.finalize(bin->orig_bytes);
        *rbuf = (char*) bin;
    }
    return result_len;
}

XAPIAN_ERLANG_NS_END
