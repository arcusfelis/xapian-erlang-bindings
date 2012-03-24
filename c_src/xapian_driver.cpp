#include "erl_driver.h"

#include <xapian.h>

#include <cstdlib>
#include <vector>
#include <string>
#include <cstring>

/* For int32_t */
#include <stdint.h>


using namespace std;


/* Name of the so or dll library */
#define DRIVER_NAME xapian_drv

/* Helps transform an argument into a string */
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)


class XapianErlangDriver 
{
    private:
    Xapian::Database *db;

    public:
    // Commands
    static const int OPEN = 0;

    // Modes for opening of a db
    static const int8_t READ_OPEN                 = 0;
    static const int8_t WRITE_CREATE_OR_OPEN      = 1;
    static const int8_t WRITE_CREATE              = 2;
    static const int8_t WRITE_CREATE_OR_OVERWRITE = 3;
    static const int8_t WRITE_OPEN                = 4;

    XapianErlangDriver()
    {
        db = NULL;
    }

    /**
     * Here we do some initialization, start is called from open_port. 
     * The drv_data will be passed to control and stop.
     */
    static ErlDrvData start(
        ErlDrvPort port, 
        char *buf)
    {
        /* If the flag is set to PORT_CONTROL_FLAG_BINARY, 
           a binary will be returned. */       
        set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY); 
        XapianErlangDriver* drv_data = new XapianErlangDriver();
        return (ErlDrvData) drv_data;
    }


    static void stop(
        ErlDrvData drv_data) 
    {
        XapianErlangDriver* drv = (XapianErlangDriver*) drv_data;
        delete drv;
    }   


    static int control(
        ErlDrvData drv_data, 
        unsigned int command, 
        char *buf, 
        int len, 
        char **rbuf, 
        int rlen)
    {
        XapianErlangDriver* drv = (XapianErlangDriver*) drv_data;
        switch(command) {
        case OPEN:
            return drv->open(buf, len, rbuf, rlen);
            
        default:
            return -1;
        }
    }



    int open(char *buf, int len, char **rbuf, int rlen)
    {
        // parse params
        const int32_t path_len = *((int32_t*) buf);
        buf += sizeof(int32_t);
        const char * path_bin = buf;
        buf += path_len;
        const int8_t mode = *((int8_t*) buf);
        

        // string ( const char * s, size_t n );
        const string dbpath(path_bin, (size_t) path_len);

        // Is already opened?
        if (db != NULL)
            return -1;

        switch(mode) {
            // Open readOnly db
            case READ_OPEN:
                db = new Xapian::Database(dbpath);
                break;

            // open for read/write; create if no db exists
            case WRITE_CREATE_OR_OPEN:
                db = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_CREATE_OR_OPEN);
                break;

            // create new database; fail if db exists
            case WRITE_CREATE:
                db = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_CREATE);
                break;

            // overwrite existing db; create if none exists
            case WRITE_CREATE_OR_OVERWRITE:
                db = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_CREATE_OR_OVERWRITE);
                break;

            // open for read/write; fail if no db exists
            case WRITE_OPEN:
                db = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_OPEN);
                break;

            default:
                return -1;
        }
        return 0;
    }

};


ErlDrvEntry xapian_driver_entry = {
    /* F_PTR init, 
       called at system start up for statically linked drivers, and after 
       loading for dynamically loaded drivers.
      
       int (*init)(void);
     */
    NULL,               

    /* L_PTR start, called when port is opened.
       called when open_port/2 is invoked.
       return value -1 means failure.

       ErlDrvData (*start)(ErlDrvPort port, char *command);
     */
    XapianErlangDriver::start,    

    /* F_PTR stop, called when port is closed.
       called when port is closed, and when the emulator is halted. 

       void (*stop)(ErlDrvData drv_data);
     */
    XapianErlangDriver::stop,

    /* F_PTR output, called when erlang has sent.
        called when we have output from erlang to the port.

       void (*output)(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
     */
    NULL,

    /* F_PTR ready_input, called when input descriptor ready.
       called when we have input from one of the driver's handles.

       void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event)
     */
    NULL, 

    /* F_PTR ready_output, called when output descriptor ready.
       called when output is possible to one of the driver's handles.

       void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);
     */
    NULL,

    /* char *driver_name, the argument to open_port.
       name supplied as command in open_port XXX ?

       char *driver_name;
     */
    (char*) STR(DRIVER_NAME), /* it is a macro */

    /* F_PTR finish, called when unloaded.
       called before unloading the driver - DYNAMIC DRIVERS ONLY.

       void (*finish)(void);
     */
    NULL,

    /* Reserved -- Used by emulator internally. */
    NULL,

    /* F_PTR control, port_command callback. 
       "ioctl" for drivers - invoked by port_control/3

       ErlDrvSSizeT (*control)(ErlDrvData drv_data, 
                unsigned int command,
                char *buf, ErlDrvSizeT len,
                char **rbuf, ErlDrvSizeT rlen);
     */
    XapianErlangDriver::control,  

    /* F_PTR timeout, reserved. 
       Handling of timeout in driver.

       void (*timeout)(ErlDrvData drv_data);
     */
    NULL,

    /* F_PTR outputv, reserved. 
       called when we have output from erlang to the port.

       void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev);
     */
    NULL,

    /* F_PTR ready_async. 

       void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data);
     */
    NULL,

    /* F_PTR flush.
       called when the port is about to be closed, and there is data in the 
       driver queue that needs to be flushed before 'stop' can be called.

       void (*flush)(ErlDrvData drv_data);
     */
    NULL,

    /* F_PTR call. 
       Works mostly like 'control', a synchronous call into the driver.

       ErlDrvSSizeT (*call)(ErlDrvData drv_data, 
             unsigned int command,
             char *buf, ErlDrvSizeT len,
             char **rbuf, ErlDrvSizeT rlen, unsigned int *flags);
     */
    NULL,

    /* F_PTR event.
       Called when an event selected by driver_event() has occurred.

       void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
                  ErlDrvEventData event_data); 
      */
    NULL, 
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,

    /* ERL_DRV_FLAGs */
    ERL_DRV_FLAG_USE_PORT_LOCKING,

    /* Reserved -- Used by emulator internally */
    NULL,

    /* F_PTR process_exit.
       Called when a process monitor fires. 

       void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
     */
    NULL, 

    /* F_PTR stop_select.
       Called to close an event object.

       void (*stop_select)(ErlDrvEvent event, void* reserved);
     */
    NULL, 
};


extern "C"
{
/**
 * An Erlang driver only exports one function: the driver entry function. 
 * This is defined with a macro, DRIVER_INIT, and returns a pointer to a C 
 * struct containing the entry points that are called from the emulator. 
 * The struct defines the entries that the emulator calls to call the driver, 
 * with a NULL pointer for entries that are not defined and used by the driver.
 */
DRIVER_INIT(DRIVER_NAME) /* must match name in driver_entry */
{
        return &xapian_driver_entry;
}
}
