#include "erl_driver.h"

#include <xapian.h>

#include <cstdlib>
#include <vector>
#include <string>
#include <cstring>
#include <stdexcept>
#include <sstream>

//#define NDEBUG
#include <assert.h>

/* For int32_t */
#include <stdint.h>


/* Name of the so or dll library */
#define DRIVER_NAME xapian_drv

/* Helps transform an argument into a string */
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)

using namespace std;


class DriverRuntimeError: public runtime_error
{
    const char * __type;

    public:
    DriverRuntimeError(const char * type, const string& str):
        std::runtime_error(str) { __type = type; }

    const char * get_type() const
    {
       return __type;
    }

};

class MemoryAllocationDriverError: public DriverRuntimeError
{
    static const char TYPE[];
    public:

    MemoryAllocationDriverError(int size) : 
        DriverRuntimeError(TYPE, buildString(size)) {}

    static const string buildString(int size)
    {
        std::stringstream ss;
        ss << "Cannot allocate " << size << " bytes.";
        return ss.str();
    }
};

class BadCommandDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    BadCommandDriverError(int8_t command_id) : 
        DriverRuntimeError(TYPE, buildString(command_id)) {}

    static const string buildString(int8_t command_id)
    {
        std::stringstream ss;
        ss << "Unknown command with id = " << command_id << ".";
        return ss.str();
    }
};

class OverflowDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    OverflowDriverError() : 
        DriverRuntimeError(TYPE, "Too short binary.") {}
};

class NotWritableDatabaseError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    NotWritableDatabaseError() : 
        DriverRuntimeError(TYPE, "The database is open as read only.") {}
};


#define REG_TYPE(CLASS) const char CLASS::TYPE[] = STR(CLASS);
REG_TYPE(MemoryAllocationDriverError)
REG_TYPE(BadCommandDriverError)
REG_TYPE(OverflowDriverError)
REG_TYPE(NotWritableDatabaseError)


#define READ_TYPE(T) (*((T*) move(sizeof(T))))
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

    char* move(int size)
    {
        len -= size;
        if (len < 0)
            throw OverflowDriverError();
        char* old_buf = buf;
        buf += size;
        return old_buf;
    }

    /**
     * Decodes <<StringLen:32/native-signed-integer, StringBin/binary>>
     */
    operator const string() {
        const int32_t str_len = READ_TYPE(int32_t);
        const char* str_bin = buf;
        move(str_len);
        const string str(str_bin, (size_t) str_len);
        return str;
    }

    /**
     * Decodes <<Int8t:8/native-signed-integer>>
     */
    operator int8_t() {
        int8_t result = READ_TYPE(int8_t);
        return result;
    }

    /**
     * Decodes <<Num:32/native-signed-integer>>
     */
    operator int32_t() {
        int32_t result = READ_TYPE(int32_t);
        return result;
    }

    /**
     * Decodes <<Num:32/native-unsigned-integer>>
     * Results are Xapian::valueno or Xapian::termcount
     */
    operator uint32_t() {
        uint32_t result = READ_TYPE(uint32_t);
        return result;
    }

};

typedef struct DataSegment DataSegment;
struct DataSegment
{
    DataSegment* next;
    int size;
    char data[];
};

#define RESERVED_LEN 100
#define SIZE_OF_SEGMENT(LEN) (sizeof(DataSegment) + LEN)
#define PUT_VALUE(X) (put((char*) &(X), sizeof(X)))


/**
 * If the driver wants to return data, it should return it in rbuf. 
 * When control is called, *rbuf points to a default buffer of rlen 
 * bytes, which can be used to return data. Data is returned different 
 * depending on the port control flags (those that are set with 
 * set_port_control_flags). 
 */
class ResultEncoder
{
    /* Here will be a pointer on data after all operations */
    char **result_buf;
    int result_len;

    /* Preallocated */
    char *default_buf;

    /* Where to write now */
    char *current_buf;

    /* Next segment */
    DataSegment* first_segment;
    DataSegment* last_segment;

    /* Len of buffer, allocated by a port */
    int default_len;
    int left_len;
    public:

    void
    setBuffer(char **rbuf, int rlen) 
    {
        result_buf = rbuf;
        current_buf = default_buf = *rbuf;
        default_len = rlen;
        result_len = 0;
        first_segment = NULL;
        left_len = rlen;
    }

    ResultEncoder & operator<<(Xapian::docid docid)
    {
        PUT_VALUE(docid);
        return *this;
    }

    ResultEncoder & operator<<(const string str)
    {
        uint32_t len = (uint32_t) str.length();
        PUT_VALUE(len);
        put(str.data(), len);
        return *this;
    }

    ResultEncoder & operator<<(const char * str)
    {
        uint32_t len = (uint32_t) strlen(str);
        PUT_VALUE(len);
        put(str, len);
        return *this;
    }

    ResultEncoder & operator<<(uint8_t value)
    {
        PUT_VALUE(value);
        return *this;
    }


    DataSegment* alloc(int size)
    {
        size = SIZE_OF_SEGMENT(size);
        DataSegment* ds = (DataSegment*) driver_alloc(size);
        if (ds == NULL)
            throw MemoryAllocationDriverError(size);
        return ds;
    }

    /* Copy termLen bytes from term. */
    void put(const char* term, const int term_len)
    {
        /* Len of the whole buffer after coping */
        const int new_len = term_len + result_len;
        const bool allocated = first_segment != NULL;
        const bool large = allocated || default_buf == NULL || new_len > default_len;
        const bool no_alloc = allocated && term_len <= left_len;
        if (!large || no_alloc)
        {
            /* Whole term can be stored in the preallocated area. */
            char* dest = current_buf;
            const char* src = term;
            memcpy(dest, src, term_len);
            current_buf += term_len;
            left_len -= term_len;
        }
        else 
        {
            /* part1 will stay in a default buffer. */
            const int part1_len = left_len;
            const int part2_len = term_len - part1_len;
            const int new_segment_len = part2_len + RESERVED_LEN;

            /* Create new data segment. */
            DataSegment* 
            new_segment = this->alloc(new_segment_len);
            new_segment->size = new_segment_len;
            new_segment->next = NULL;

            const char* part1_src = term;
            const char* part2_src = term + part1_len;

            char* part1_dest = current_buf;
            char* part2_dest = new_segment->data;

            /* Copy data */
            memcpy(part1_dest, part1_src, part1_len);
            memcpy(part2_dest, part2_src, part2_len);

            if (first_segment == NULL) {
                /* Set as a first segment. */
                first_segment = new_segment;
            } else {
                /* Link to a last segment. */
                last_segment->next = new_segment;
            }

            /* Save state */
            last_segment = new_segment;
            current_buf = part2_dest + part2_len;
            left_len = RESERVED_LEN;
        }
        
        result_len = new_len;
    }

    /**
     * Get return result
     */
    operator int() {
        if (result_len > default_len)
        {
            ErlDrvBinary* bin = driver_alloc_binary(result_len);
            if (bin == NULL)
                throw MemoryAllocationDriverError(result_len);
            // TODO: can be skipped
            memset(bin->orig_bytes, 0, result_len);

            char* dest = bin->orig_bytes;
            char* src = default_buf;
            memcpy(dest, src, default_len);
            dest += default_len;

            /* Shrink data size to copy. */
            assert(left_len >= 0);
            last_segment->size -= left_len;
            do 
            {
                assert(first_segment != NULL);

                /* No infinite cycles. */
                assert(first_segment->next != first_segment);

                int segment_size = first_segment->size;
                assert(segment_size > 0);

                /* Real buffer length is not greater, then allocated buffer length. */
                assert((dest - (bin->orig_bytes) + segment_size) <= result_len);

                src = first_segment->data;
                memcpy(dest, src, segment_size);
                dest += segment_size;

                /* Use last_segment as a temp variable. */
                last_segment = first_segment;

                /* Move a pointer on the next segment. */
                first_segment = first_segment->next;

                /* Delete copied segment. */
                driver_free(last_segment);
            }
            while(first_segment != NULL);
            *result_buf = (char*) bin;
        }
        
        return result_len;
    }


    void clear() {
        if (result_len > default_len)
        {
            do 
            {
                assert(first_segment != NULL);

                /* No infinite cycles. */
                assert(first_segment->next != first_segment);

                /* Use last_segment as a temp variable. */
                last_segment = first_segment;

                /* Move a pointer on the next segment. */
                first_segment = first_segment->next;

                /* Delete copied segment. */
                driver_free(last_segment);
            }
            while(first_segment != NULL);
        }
    }

};




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
    static const int OPEN               = 0;
    static const int LAST_DOC_ID        = 1;
    static const int ADD_DOCUMENT       = 2;
    static const int TEST               = 3;
    static const int GET_DOCUMENT_BY_ID = 4;
    static const int START_TRANSACTION  = 5;
    static const int CANCEL_TRANSACTION = 6;
    static const int COMMIT_TRANSACTION = 7;
    
    static const uint8_t SUCCESS = 0;
    static const uint8_t ERROR   = 1;

    static int control(
        ErlDrvData drv_data, 
        unsigned int command, 
        char *buf, 
        int len, 
        char **rbuf, 
        int rlen)
    {
        ParamDecoder params(buf, len); 
        XapianErlangDriver& drv = * (XapianErlangDriver*) drv_data;
        ResultEncoder& result = * drv.getResultEncoder();
        result.setBuffer(rbuf, rlen);
        result << SUCCESS;

        try
        {
            switch(command) {
            case OPEN: 
                {
                const string dbpath = params;
                int8_t mode = params;
                return drv.open(dbpath, mode);
                }

            case LAST_DOC_ID:
                return drv.getLastDocId();

            case ADD_DOCUMENT:
                return drv.addDocument(params);

            case TEST:
                return drv.test(params);

            case GET_DOCUMENT_BY_ID:
                return drv.getDocumentById(params);

            case START_TRANSACTION:
                return drv.startTransaction();

            case CANCEL_TRANSACTION:
                return drv.cancelTransaction();

            case COMMIT_TRANSACTION:
                return drv.commitTransaction();
            }
        }
        catch (DriverRuntimeError& e) 
        {
            result.clear();
            result.setBuffer(rbuf, rlen);
            result << ERROR;
            result << e.get_type();
            result << e.what();
            return result;
        }
        catch (Xapian::Error& e) 
        {
            result.clear();
            result.setBuffer(rbuf, rlen);
            result << ERROR;
            result << e.get_type();
            result << e.get_msg();
            return result;
        }
        return -1;
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
        return result;
    }

    int getLastDocId()
    {
        //Xapian::docid get_lastdocid() const
        Xapian::docid 
        docid = db->get_lastdocid();
        result << docid;
        return result;
    }


    int addDocument(ParamDecoder& params)
    {
        Xapian::Document doc;
        applyDocument(params, doc);
        Xapian::docid
        docid = wdb->add_document(doc);
        result << docid;
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

    static const int8_t GET_VALUE      = 1;
    static const int8_t GET_DATA       = 2;
    static const int8_t GET_DOCID      = 3;

    /* Gets a copy of params */
    void retrieveDocument(ParamDecoder params, Xapian::Document& doc)
    {
        while (int8_t command = params)
        /* Do, while command != stop != 0 */
        {
            switch (command)
            {
                case GET_VALUE:
                {
                    Xapian::valueno   slot  = params;
                    const string value = doc.get_value(slot);
                    result << value;
                    break;
                }

                case GET_DATA:
                {
                    const string data = doc.get_data();
                    result << data;
                    break;
                }

                case GET_DOCID:
                {
                    const Xapian::docid docid = doc.get_docid();
                    result << docid;
                    break;
                }

                default:
                    throw BadCommandDriverError(command);
            }
        }
    }

    void assertWriteable() const
    {
        if (wdb == NULL)
            throw NotWritableDatabaseError();
    }

    int startTransaction()
    {
        assertWriteable();
        wdb->begin_transaction();
        return result;
    }

    int cancelTransaction()
    {
        assertWriteable();
        wdb->cancel_transaction();
        return result;
    }

    int commitTransaction()
    {
        assertWriteable();
        wdb->commit_transaction();
        return result;
    }

    int getDocumentById(ParamDecoder& params)
    {
        Xapian::docid docid = params;
        Xapian::Document doc = db->get_document(docid);
        retrieveDocument(params, doc);
        return result;
    }


    static const int8_t TEST_RESULT_ENCODER = 1;
    static const int8_t TEST_EXCEPTION      = 2;

    int test(ParamDecoder& params)
    {
        int8_t num = params;
        switch (num)
        {
            case TEST_RESULT_ENCODER:
            {
                Xapian::docid from = params;
                Xapian::docid to = params;
//              Xapian::docid from = 1;
//              Xapian::docid to = 1000;

                return testResultEncoder(from, to);
            }

            case TEST_EXCEPTION:
                return testException();
        }
        return 0;
    }

    int testResultEncoder(Xapian::docid from, Xapian::docid to)
    {
        for (; from <= to; from++)
            result << from;
        return result;
    }

    int testException()
    {
        throw MemoryAllocationDriverError(1000);
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
