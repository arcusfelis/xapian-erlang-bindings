#include "erl_driver.h"

#include <xapian.h>

#include <cstdlib>
#include <vector>
#include <string>
#include <cstring>
#include <stdexcept>
#include <sstream>

/* For int32_t */
#include <stdint.h>

using namespace std;


class DriverRuntimeError: public runtime_error
{
    public:
    DriverRuntimeError(const string& str):
        std::runtime_error(str) {}
};

class MemoryAllocationDriverError: public DriverRuntimeError
{
    public:
    MemoryAllocationDriverError(int size) : 
        DriverRuntimeError(buildString(size)) {}

    static const string buildString(int size)
    {
        std::stringstream ss;
        ss << "Cannot allocate " << size << "bytes.";
        return ss.str();
    }
};

class BadCommandDriverError: public DriverRuntimeError
{
    public:
    BadCommandDriverError(int8_t command_id) : 
        DriverRuntimeError(buildString(command_id)) {}

    static const string buildString(int8_t command_id)
    {
        std::stringstream ss;
        ss << "Unknown command with id = " << command_id;
        return ss.str();
    }
};



class ParamDecoder
{
    char *buf; 
    int len;

    public:
    ParamDecoder(char *buf, int len)
    {
        this->buf = buf;
        this->len = len;
    }

    /**
     * Decodes <<StringLen:32/native-signed-integer, StringBin/binary>>
     */
    operator const string() {
        const int32_t str_len = *((int32_t*) buf);
        buf += sizeof(int32_t);
        const char * str_bin = buf;
        const string str(str_bin, (size_t) str_len);
        buf += str_len;
        return str;
    }

    /**
     * Decodes <<Int8t:8/native-signed-integer>>
     */
    operator int8_t() {
        int8_t result = *((int8_t*) buf);
        buf += sizeof(int8_t);
        return result;
    }

    /**
     * Decodes <<Num:32/native-signed-integer>>
     */
    operator int32_t() {
        int32_t result = *((int32_t*) buf);
        buf += sizeof(int32_t);
        return result;
    }

    /**
     * Decodes <<Num:32/native-unsigned-integer>>
     * Results are Xapian::valueno or Xapian::termcount
     */
    operator unsigned int32_t() {
        int32_t result = *((unsigned int32_t*) buf);
        buf += sizeof(unsigned int32_t);
        return result;
    }
};



/**
 * If the driver wants to return data, it should return it in rbuf. 
 * When control is called, *rbuf points to a default buffer of rlen 
 * bytes, which can be used to return data. Data is returned different 
 * depending on the port control flags (those that are set with 
 * set_port_control_flags). 
 */
class ResultEncoder
{
    char **rbuf;
    int rlen;
    int len;
    bool allocated;
    public:

    void
    setBuffer(char **rbuf, int rlen)
    {
        this->rbuf = rbuf;
        this->rlen = rlen;
        len = 0;
        allocated = false;
    }

    void put(Xapian::docid docid)
    {
        put(&docid, sizeof(docid));
    }

    void put(void* term, int termLen)
    {
        int newLen = termLen + len;
        // Check allocated memory
        if (*rbuf == NULL || newLen > rlen) 
        {

            char* newBuf = (char*) driver_alloc_binary(newLen);
            if(*rbuf == NULL) 
                throw MemoryAllocationDriverError(newLen);

            // void *memcpy(void *dest, const void *src, size_t n);
            memcpy(newBuf, *rbuf, len);

            if (allocated)
                driver_free_binary((ErlDrvBinary*) *rbuf);

            *rbuf = newBuf;
            allocated = true;
        }
        // Append term to buf
        // void *memcpy(void *dest, const void *src, size_t n);
        memcpy((*rbuf + len), term, termLen);
        len += termLen;
    }

    /**
     * Get return result
     */
    operator int() {
        return len;
    }
};



/* Name of the so or dll library */
#define DRIVER_NAME xapian_drv

/* Helps transform an argument into a string */
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)


class XapianErlangDriver 
{
    private:
    Xapian::Database *db;
    Xapian::WritableDatabase *wdb;
    ResultEncoder result;

    public:
    ResultEncoder* getResultEncoder()
    {
        return &result;
    }

    XapianErlangDriver()
    {
        db = NULL;
        wdb = NULL;
    }

    ~XapianErlangDriver()
    {
        if (db != NULL) 
            delete db;
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
        if (drv != NULL)
            delete drv;
    }   


    // Commands
    static const int OPEN         = 0;
    static const int LAST_DOC_ID  = 1;
    static const int ADD_DOCUMENT = 2;

    static int control(
        ErlDrvData drv_data, 
        unsigned int command, 
        char *buf, 
        int len, 
        char **rbuf, 
        int rlen)
    {
        XapianErlangDriver* drv = (XapianErlangDriver*) drv_data;
        ParamDecoder params(buf, len); 
        drv->getResultEncoder()->setBuffer(rbuf, rlen);

        switch(command) {
        case OPEN: 
            {
            const string dbpath = params;
            int8_t mode = params;
            return drv->open(dbpath, mode);
            }

        case LAST_DOC_ID:
            return drv->getLastDocId();

        case ADD_DOCUMENT:
            return drv->addDocument(params);

        default:
            return -1;
        }
    }


    // Modes for opening of a db
    static const int8_t READ_OPEN                 = 0;
    static const int8_t WRITE_CREATE_OR_OPEN      = 1;
    static const int8_t WRITE_CREATE              = 2;
    static const int8_t WRITE_CREATE_OR_OVERWRITE = 3;
    static const int8_t WRITE_OPEN                = 4;

    int open(const string& dbpath, int8_t mode)
    {
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
                db = wdb = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_CREATE_OR_OPEN);
                break;

            // create new database; fail if db exists
            case WRITE_CREATE:
                db = wdb = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_CREATE);
                break;

            // overwrite existing db; create if none exists
            case WRITE_CREATE_OR_OVERWRITE:
                db = wdb = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_CREATE_OR_OVERWRITE);
                break;

            // open for read/write; fail if no db exists
            case WRITE_OPEN:
                db = wdb = new Xapian::WritableDatabase(dbpath, 
                    Xapian::DB_OPEN);
                break;

            default:
                return -1;
        }
        return 0;
    }

    int getLastDocId()
    {
        //Xapian::docid get_lastdocid() const
        Xapian::docid 
        docid = db->get_lastdocid();
        result.put(docid);
        return result;
    }


    int addDocument(ParamDecoder& params)
    {
        Xapian::Document doc;
        applyDocument(params, doc);
        Xapian::docid
        docid = wdb->add_document(doc);
        result.put(docid);
        return result;
    }

    
    static const int8_t STEMMER             = 1;
    static const int8_t DATA                = 2;
    static const int8_t VALUE               = 3;
    static const int8_t DELTA               = 4;
    static const int8_t TEXT                = 5;
    static const int8_t TERM                = 6;
    static const int8_t POSTING             = 7;

    /**
     * Read commands, encoded by xapian_document:encode.
     * Used in update, replace, add document functions
     */
    void applyDocument(ParamDecoder& params, Xapian::Document& doc)
    {
        Xapian::TermGenerator   termgenerator;
        termgenerator.set_document(doc);

        while (int8_t command = params)
        /* Do, while command != stop != 0 */
        {
            switch (command)
            {
                case STEMMER:
                {
                    // see xapian_document:append_stemmer
                    const string   language = params;
                    termgenerator.set_stemmer(Xapian::Stem(language));
                    break;
                }

                case DATA:
                {
                    // see xapian_document:append_data
                    const string   data = params;
                    doc.set_data(data);
                    break;
                }

                case VALUE:
                {
                    // see xapian_document:append_value
                    Xapian::valueno   slot  = params;
                    const string      value = params;
                    doc.add_value(slot, value); 
                    break;
                }

                case DELTA:
                {
                    // see xapian_document:append_delta
                    Xapian::termcount   delta = params;
                    termgenerator.increase_termpos(delta);
                    break;
                }

                case TEXT:
                {
                    // see xapian_document:append_delta
                    const string      text    = params; // value
                    Xapian::termcount wdf_inc = params; // pos
                    const string      prefix  = params;
                    termgenerator.index_text(text, wdf_inc, prefix); 
                    break;
                }

                case TERM:
                {
                    // see xapian_document:append_term
                    const string      tname   = params; // value
                    Xapian::termcount wdf_inc = params; 
                    // Pos = undefined
                    doc.add_term(tname, wdf_inc);
                    break;
                }

                case POSTING:
                {
                    // see xapian_document:append_term
                    const string      tname   = params; // value
                    Xapian::termpos   tpos    = params;
                    Xapian::termcount wdf_inc = params;
                    doc.add_posting(tname, tpos, wdf_inc);
                    break;
                }

                default:
                    throw BadCommandDriverError(command);
            }
        }
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
