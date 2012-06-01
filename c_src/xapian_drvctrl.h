#ifndef XAPIAN_DRIVER_CONTROLLER_H
#define XAPIAN_DRIVER_CONTROLLER_H
#include "erl_driver.h"

/* Hack to handle R15 driver used with pre R15 driver */
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN
class DriverController
{
    public:
    /**
     * The driver is loaded.
     * This is called only once.
     */
    static int 
    init();

    /**
     * The driver is unloaded.
     * This is called only once.
     */
    static void
    finish();

    /**
     * Here we do some initialization, start is called from `open_port'. 
     * The drv_data will be passed to control and stop.
     * This is called multiple times.
     */
    static ErlDrvData 
    start(ErlDrvPort port, char* /* buf */);

    /**
     * This is called multiple times.
     */
    static void 
    stop(ErlDrvData drv_data);

    static ErlDrvSSizeT control(
        ErlDrvData      drv_data, 
        unsigned int    command, 
        char*           buf, 
        ErlDrvSizeT     len, 
        char**          rbuf, 
        ErlDrvSizeT     rlen);
};
XAPIAN_ERLANG_NS_END
#endif
