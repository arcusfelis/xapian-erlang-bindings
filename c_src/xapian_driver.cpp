/* vim: set filetype=cpp shiftwidth=4 tabstop=4 expandtab tw=80: */

/**
 * Prefix m_ (member) for properties means that property is private.
 */



// -------------------------------------------------------------------
// Includes
// -------------------------------------------------------------------

#include <xapian.h>

#include <cstdlib>
#include <vector>
#include <string>
#include <cstring>
#include <sstream>

#include <assert.h>
#include <stdexcept>

/* For int32_t, uint8_t and so on. */
#include <stdint.h>


#include "erl_driver.h"

/* Hack to handle R15 driver used with pre R15 driver */
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif


// -------------------------------------------------------------------
// Defines
// -------------------------------------------------------------------

/* Name of the so or dll library. */
#define DRIVER_NAME xapian_drv

/* These macroses help to transform an argument into a string. */
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)

#define REG_TYPE(CLASS) const char CLASS::TYPE[] = STR(CLASS);

/* Helper used by ParamDecoder. */
//#define READ_TYPE(T) (*((T*) move(sizeof(T))))
#define READ_TYPE(T) (*(reinterpret_cast<T*>( move(sizeof(T)) )))

/*
 * The length of free space in bytes, 
 * which will be allocatied new memory for storing result. 
 */
#define RESERVED_LEN 100

/* Helper used by ResultEncoder. */
#define SIZE_OF_SEGMENT(LEN) (sizeof(DataSegment) + LEN)

/* Helper used by ResultEncoder. */
//#define PUT_VALUE(X) (put((char*) &(X), sizeof(X)))
#define PUT_VALUE(X) (put(reinterpret_cast<char*>( &(X) ), sizeof(X)))

/* Uncomment to disable asserts. */
//#define NDEBUG

using namespace std;


// -------------------------------------------------------------------
// Exceptions
// -------------------------------------------------------------------

class DriverRuntimeError: public runtime_error
{
    const char* m_type;

    public:
    DriverRuntimeError(const char * type, const string& str):
        runtime_error(str) { m_type = type; }

    const char* 
    get_type() const
    {
       return m_type;
    }

};

class MemoryAllocationDriverError: public DriverRuntimeError
{
    static const char TYPE[];
    public:

    MemoryAllocationDriverError(size_t size) : 
        DriverRuntimeError(TYPE, buildString(size)) {}

    static const string 
    buildString(size_t size)
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
    BadCommandDriverError(int command_id) : 
        DriverRuntimeError(TYPE, buildString(command_id)) {}

    static const string 
    buildString(int command_id)
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



REG_TYPE(MemoryAllocationDriverError)
REG_TYPE(BadCommandDriverError)
REG_TYPE(OverflowDriverError)
REG_TYPE(NotWritableDatabaseError)


// -------------------------------------------------------------------
// Decoder of the parameters from erlang's calls
// -------------------------------------------------------------------

class ParamDecoder
{
    char *m_buf; 
    size_t m_len;

    public:
    /**
     * Constructor
     */
    ParamDecoder(char *buf, size_t len) :
        m_buf( buf ),
        m_len( len ) {}


    char* 
    move(const size_t size)
    {
        if (size > m_len)
            throw OverflowDriverError();
        m_len -= size;
        char* old_buf = m_buf;
        m_buf += size;
        return old_buf;
    }

    /**
     * Decodes <<StringLen:32/native-signed-integer, StringBin/binary>>
     */
    operator const string() {
        const int32_t str_len = READ_TYPE(int32_t);
        const char* str_bin = m_buf;
        move(str_len);
        const string str(str_bin, static_cast<size_t>(str_len));
        return str;
    }

    /**
     * Decodes <<Int8t:8/native-signed-integer>>
     */
    operator int8_t() {
        return READ_TYPE(int8_t);
    }

    /**
     * Decodes <<Int8t:8/native-unsigned-integer>>
     */
    operator uint8_t() {
        return READ_TYPE(uint8_t);
    }

    /**
     * Decodes <<Num:32/native-signed-integer>>
     */
    operator int32_t() {
        return READ_TYPE(int32_t);
    }

    /**
     * Decodes <<Num:32/native-unsigned-integer>>
     * Results are Xapian::valueno or Xapian::termcount
     */
    operator uint32_t() {
        return READ_TYPE(uint32_t);
    }

};



// -------------------------------------------------------------------
// Structure with variable length
// -------------------------------------------------------------------

typedef struct DataSegment DataSegment;
struct DataSegment
{
    DataSegment* next;
    size_t size;
    char data[]; /* flexible-array member */
};



// -------------------------------------------------------------------
// Result encoder: appends variables to the buffer
// -------------------------------------------------------------------

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
    char**  m_result_buf;
    size_t  m_result_len;

    /* Preallocated */
    char*   m_default_buf;

    /* Where to write now */
    char*   m_current_buf;

    /* Len of buffer, allocated by a port */
    size_t  m_default_len;
    size_t  m_left_len;

    /* Next segment */
    DataSegment* m_first_segment;
    DataSegment* m_last_segment;

    public:

    void
    setBuffer(char** rbuf, const size_t rlen) 
    {
        m_result_buf = rbuf;
        m_current_buf = m_default_buf = *rbuf;
        m_default_len = rlen;
        m_result_len = 0;
        m_first_segment = NULL;
        m_left_len = rlen;
    }

    


    ResultEncoder& 
    operator<<(const string str)
    {
        uint32_t len = static_cast<uint32_t>( str.length() );
        PUT_VALUE(len);
        put(str.data(), len);
        return *this;
    }

    ResultEncoder& 
    operator<<(const char * str)
    {
        uint32_t len = static_cast<uint32_t>( strlen(str) );
        PUT_VALUE(len);
        put(str, len);
        return *this;
    }

    ResultEncoder& 
    operator<<(uint32_t value)
    {
        PUT_VALUE(value);
        return *this;
    }

    ResultEncoder& 
    operator<<(uint8_t value)
    {
        PUT_VALUE(value);
        return *this;
    }

    ResultEncoder& 
    operator<<(double value)
    {
        PUT_VALUE(value);
        return *this;
    }

    DataSegment* 
    alloc(size_t size)
    {
        size = SIZE_OF_SEGMENT(size);
        DataSegment* ds = static_cast<DataSegment*>( driver_alloc(size) );
        if (ds == NULL)
            throw MemoryAllocationDriverError(size);
        return ds;
    }

    void put(const char* term, const size_t term_len);

    operator size_t();

    void clear();
};



/** 
 * Copy termLen bytes from term. 
 */
void 
ResultEncoder::put(const char* term, const size_t term_len)
{
    /* Len of the whole buffer after coping */
    const size_t new_len = term_len + m_result_len;
    const bool allocated = m_first_segment != NULL;
    const bool large = allocated 
        || m_default_buf == NULL 
        || new_len > m_default_len;
    const bool no_alloc = allocated && term_len <= m_left_len;

    if (!large || no_alloc)
    {
        /* Whole term can be stored in the preallocated area. */
        char* dest = m_current_buf;
        const char* src = term;
        memcpy(dest, src, term_len);
        m_current_buf += term_len;
        m_left_len -= term_len;
    }
    else 
    {
        /* part1 will stay in a default buffer. */
        const size_t part1_len = m_left_len;
        const size_t part2_len = term_len - part1_len;
        const size_t new_segment_len = part2_len + RESERVED_LEN;

        /* Create new data segment. */
        DataSegment* 
        new_segment = this->alloc(new_segment_len);
        new_segment->size = new_segment_len;
        new_segment->next = NULL;

        const char* part1_src = term;
        const char* part2_src = term + part1_len;

        char* part1_dest = m_current_buf;
        char* part2_dest = new_segment->data;

        /* Copy data */
        memcpy(part1_dest, part1_src, part1_len);
        memcpy(part2_dest, part2_src, part2_len);

        if (m_first_segment == NULL) {
            /* Set as a first segment. */
            m_first_segment = new_segment;
        } else {
            /* Link to a last segment. */
            m_last_segment->next = new_segment;
        }

        /* Save state */
        m_last_segment = new_segment;
        m_current_buf = part2_dest + part2_len;
        m_left_len = RESERVED_LEN;
    }
    
    m_result_len = new_len;
}


/**
 * Clear the state of the object.
 */
void 
ResultEncoder::clear() {
    if (m_result_len > m_default_len)
    {
        do 
        {
            assert(m_first_segment != NULL);

            /* No infinite cycles. */
            assert(m_first_segment->next != m_first_segment);

            /* Use last_segment as a temp variable. */
            m_last_segment = m_first_segment;

            /* Move a pointer on the next segment. */
            m_first_segment = m_first_segment->next;

            /* Delete copied segment. */
            driver_free(m_last_segment);
        }
        while(m_first_segment != NULL);
    }
}


/**
 * Returns result
 */
ResultEncoder::operator size_t() {
    if (m_result_len > m_default_len)
    {
        ErlDrvBinary* bin = driver_alloc_binary(m_result_len);
        if (bin == NULL)
            throw MemoryAllocationDriverError(m_result_len);
        // TODO: can be skipped
        memset(bin->orig_bytes, 0, m_result_len);

        char* dest = bin->orig_bytes;
        char* src = m_default_buf;
        memcpy(dest, src, m_default_len);
        dest += m_default_len;

        /* Shrink data size to copy. */
        m_last_segment->size -= m_left_len;
        do 
        {
            assert(m_first_segment != NULL);

            /* No infinite cycles. */
            assert(m_first_segment->next != m_first_segment);

            size_t segment_size = m_first_segment->size;
            assert(segment_size > 0);

            /* Real buffer length is not greater, then allocated buffer length. */
            assert((dest - bin->orig_bytes + segment_size) <= m_result_len);

            src = m_first_segment->data;
            memcpy(dest, src, segment_size);
            dest += segment_size;

            /* Use last_segment as a temp variable. */
            m_last_segment = m_first_segment;

            /* Move a pointer on the next segment. */
            m_first_segment = m_first_segment->next;

            /* Delete copied segment. */
            driver_free(m_last_segment);
        }
        while(m_first_segment != NULL);
        *m_result_buf = (char*) bin;
    }
    
    return m_result_len;
}



// -------------------------------------------------------------------
// Main Driver Class
// -------------------------------------------------------------------

class XapianErlangDriver 
{
    Xapian::Database* m_db;
    Xapian::WritableDatabase* m_wdb;
    ResultEncoder m_result;


    public:

    // Commands
    // used in the control function
    enum command {
        OPEN                        = 0,
        LAST_DOC_ID                 = 1,
        ADD_DOCUMENT                = 2,
        TEST                        = 3,
        GET_DOCUMENT_BY_ID          = 4,
        START_TRANSACTION           = 5,
        CANCEL_TRANSACTION          = 6,
        COMMIT_TRANSACTION          = 7,
        QUERY_PAGE                  = 8
    };

    // Error prefix tags.
    // used in the control function
    enum errorCode {
        SUCCESS                     = 0,
        ERROR                       = 1
    };

    // Modes for opening of a db
    // used in the open function
    enum openMode {
        READ_OPEN                   = 0,
        WRITE_CREATE_OR_OPEN        = 1,
        WRITE_CREATE                = 2,
        WRITE_CREATE_OR_OVERWRITE   = 3,
        WRITE_OPEN                  = 4
    };

    // Types of fields
    // Used in the applyDocument function.
    enum fieldTypeIn {
        STEMMER                     = 1,
        DATA                        = 2,
        VALUE                       = 3,
        DELTA                       = 4,
        TEXT                        = 5,
        TERM                        = 6,
        POSTING                     = 7
    };

    // Types of the fields.
    // Used in the retrieveDocument function.
    enum fieldTypeOut {
        GET_VALUE                   = 1,
        GET_DATA                    = 2,
        GET_DOCID                   = 3,
        GET_WEIGHT                  = 4,
        GET_RANK                    = 5,
        GET_PERCENT                 = 6
    };

    // Numbers of tests.
    // Used in the test function.
    enum testNumber {
        TEST_RESULT_ENCODER         = 1,
        TEST_EXCEPTION              = 2
    };


    enum queryType {
        QUERY_GROUP                 = 1,
        QUERY_VALUE                 = 2,
        QUERY_VALUE_RANGE           = 3,
        QUERY_TERM                  = 4
    };


    /**
     * Here we do some initialization, start is called from open_port. 
     * The drv_data will be passed to control and stop.
     */
    static ErlDrvData start(
        ErlDrvPort port, 
        char* /* buf */)
    {
        /* If the flag is set to PORT_CONTROL_FLAG_BINARY, 
           a binary will be returned. */       
        set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY); 
        XapianErlangDriver* drv_data = new XapianErlangDriver();
        return reinterpret_cast<ErlDrvData>( drv_data );
    }


    static void stop(
        ErlDrvData drv_data) 
    {
        XapianErlangDriver* 
        drv = reinterpret_cast<XapianErlangDriver*>( drv_data );

        if (drv != NULL)
            delete drv;
    }   


    static ErlDrvSSizeT control(
        ErlDrvData      drv_data, 
        unsigned int    command, 
        char*           buf, 
        ErlDrvSizeT     len, 
        char**          rbuf, 
        ErlDrvSizeT     rlen);



    ResultEncoder* getResultEncoder()
    {
        return &m_result;
    }

    XapianErlangDriver()
    {
        m_db = NULL;
        m_wdb = NULL;
    }

    ~XapianErlangDriver()
    {
        if (m_db != NULL) 
            delete m_db;
    }


    size_t open(const string& dbpath, int8_t mode);

    size_t getLastDocId()
    {
        //Xapian::docid get_lastdocid() const
        const Xapian::docid 
        docid = m_db->get_lastdocid();
        m_result << static_cast<uint32_t>(docid);
        return m_result;
    }


    size_t addDocument(ParamDecoder& params)
    {
        Xapian::Document doc;
        applyDocument(params, doc);
        const Xapian::docid
        docid = m_wdb->add_document(doc);
        m_result << static_cast<uint32_t>(docid);
        return m_result;
    }


    /**
     * Read commands, encoded by xapian_document:encode.
     * Used in update, replace, add document functions
     */
    void applyDocument(ParamDecoder& params, Xapian::Document& doc);


    size_t query(ParamDecoder& params)
    {
        /* offset, pagesize, query, template */
        const uint32_t offset   = params;
        const uint32_t pagesize = params;

        // Use an Enquire object on the database to run the query.
        Xapian::Enquire enquire(*m_db);
        Xapian::Query   query = buildQuery(params);
        enquire.set_query(query);
         

        // Get an result
        Xapian::MSet mset = enquire.get_mset(
            static_cast<Xapian::termcount>(offset), 
            static_cast<Xapian::termcount>(pagesize));

        Xapian::doccount count = mset.size();
        m_result << static_cast<uint32_t>(count);

        for (Xapian::MSetIterator m = mset.begin(); m != mset.end(); ++m) {
            Xapian::docid did = *m;
            Xapian::Document doc = m.get_document();
            retrieveDocument(params, doc, &m);
        }
        return m_result;
    }

    /** 
     * Gets a copy of params.
     *
     * `params' is a clone.
     */
    void retrieveDocument(ParamDecoder, Xapian::Document&, Xapian::MSetIterator*);

    Xapian::Query 
    buildQuery(ParamDecoder& params);


    /**
     * Throws error if the database was opened only for reading.
     */
    void assertWriteable() const
    {
        if (m_wdb == NULL)
            throw NotWritableDatabaseError();
    }

    size_t startTransaction()
    {
        assertWriteable();
        m_wdb->begin_transaction();
        return m_result;
    }

    size_t cancelTransaction()
    {
        assertWriteable();
        m_wdb->cancel_transaction();
        return m_result;
    }

    size_t commitTransaction()
    {
        assertWriteable();
        m_wdb->commit_transaction();
        return m_result;
    }

    size_t getDocumentById(ParamDecoder& params)
    {
        const Xapian::docid docid = params;
        Xapian::Document doc = m_db->get_document(docid);
        retrieveDocument(params, doc, NULL);
        return m_result;
    }


    size_t test(ParamDecoder& params)
    {
        const int8_t num = params;
        switch (num)
        {
            case TEST_RESULT_ENCODER:
            {
                const Xapian::docid from = params;
                const Xapian::docid to = params;

                return testResultEncoder(from, to);
            }

            case TEST_EXCEPTION:
                return testException();
        }
        return 0;
    }

    size_t testResultEncoder(Xapian::docid from, Xapian::docid to)
    {
        for (; from <= to; from++)
            m_result << static_cast<uint32_t>(from);
        return m_result;
    }

    size_t testException()
    {
        throw MemoryAllocationDriverError(1000);
        return 0;
    }
};


Xapian::Query 
XapianErlangDriver::buildQuery(ParamDecoder& params)
{
    const uint8_t type = params;
    switch (type)
    {
        case QUERY_GROUP:
        {    
            const uint8_t     op              = params;
            const uint32_t    parameter       = params;
            const uint32_t    subQueryCount   = params;
            vector<Xapian::Query> subQueries;

            for (uint32_t i = 0; i < subQueryCount; i++)
                subQueries.push_back(buildQuery(params));

            vector<Xapian::Query>::iterator qbegin = subQueries.begin();
            vector<Xapian::Query>::iterator qend   = subQueries.end();
            Xapian::Query q(
                static_cast<Xapian::Query::op>(op), 
                qbegin, 
                qend, 
                static_cast<Xapian::termcount>(parameter));
            return q;
        }

        case QUERY_VALUE:
        {    
            const uint8_t           op       = params;
            const Xapian::valueno   slot     = params;
            const string&           value    = params;
            Xapian::Query q(
                static_cast<Xapian::Query::op>(op), 
                slot, 
                value);
            return q;
        }
            
        case QUERY_VALUE_RANGE:
        {    
            const uint8_t           op       = params;
            const Xapian::valueno   slot     = params;
            const string&           from     = params;
            const string&           to       = params;
            Xapian::Query q(
                static_cast<Xapian::Query::op>(op), 
                slot, 
                from,
                to);
            return q;
        }

        case QUERY_TERM:
        {    
            const string&      name     = params;
            const uint32_t     wqf      = params;
            const uint32_t     pos      = params;
            Xapian::Query q(
                name,
                wqf, 
                pos);
            return q;
        }

        default:
            throw BadCommandDriverError(type);
    }
}


ErlDrvSSizeT 
XapianErlangDriver::control(
    ErlDrvData drv_data, 
    const unsigned int  command, 
    char*         buf, 
    ErlDrvSizeT   e_len, 
    char**        rbuf, 
    ErlDrvSizeT   e_rlen)
{
    const size_t len  = static_cast<int>(e_len);
    const size_t rlen = static_cast<int>(e_rlen);

    ParamDecoder params(buf, len); 
    XapianErlangDriver& drv = * reinterpret_cast<XapianErlangDriver*>( drv_data );
    ResultEncoder& result = * drv.getResultEncoder();
    result.setBuffer(rbuf, rlen);
    result << static_cast<uint8_t>( SUCCESS );

    try
    {
        switch(command) {
        case OPEN: 
            {
            const string&   dbpath = params;
            const int8_t    mode = params;
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

        case QUERY_PAGE:
            return drv.query(params);
        }
    }
    catch (DriverRuntimeError& e) 
    {
        result.clear();
        result.setBuffer(rbuf, rlen);
        result << static_cast<uint8_t>( ERROR );
        result << e.get_type();
        result << e.what();
        return result;
    }
    catch (Xapian::Error& e) 
    {
        result.clear();
        result.setBuffer(rbuf, rlen);
        result << static_cast<uint8_t>( ERROR );
        result << e.get_type();
        result << e.get_msg();
        return result;
    }
    return -1;
}


size_t 
XapianErlangDriver::open(const string& dbpath, int8_t mode)
{
    // Is already opened?
    if (m_db != NULL)
        return -1; // return `badarg'

    switch(mode) {
        // Open readOnly db
        case READ_OPEN:
            m_db = new Xapian::Database(dbpath);
            break;

        // open for read/write; create if no db exists
        case WRITE_CREATE_OR_OPEN:
            m_db = m_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_CREATE_OR_OPEN);
            break;

        // create new database; fail if db exists
        case WRITE_CREATE:
            m_db = m_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_CREATE);
            break;

        // overwrite existing db; create if none exists
        case WRITE_CREATE_OR_OVERWRITE:
            m_db = m_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_CREATE_OR_OVERWRITE);
            break;

        // open for read/write; fail if no db exists
        case WRITE_OPEN:
            m_db = m_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_OPEN);
            break;

        default:
            return -1; // badarg
    }
    return m_result;
}


void 
XapianErlangDriver::applyDocument(
    ParamDecoder& params, 
    Xapian::Document& doc)
{
    Xapian::TermGenerator   termGenerator;
    termGenerator.set_document(doc);

    while (const int8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case STEMMER:
            {
                // see xapian_document:append_stemmer
                const string&   language = params;
                termGenerator.set_stemmer(Xapian::Stem(language));
                break;
            }

            case DATA:
            {
                // see xapian_document:append_data
                const string&   data = params;
                doc.set_data(data);
                break;
            }

            case VALUE:
            {
                // see xapian_document:append_value
                const uint32_t    slot  = params;
                const string&     value = params;
                doc.add_value(static_cast<Xapian::valueno>(slot), value); 
                break;
            }

            case DELTA:
            {
                // see xapian_document:append_delta
                const uint32_t   delta = params;
                termGenerator.increase_termpos(static_cast<Xapian::termcount>(delta));
                break;
            }

            case TEXT:
            {
                // see xapian_document:append_delta
                const string&     text    = params; // value
                const uint32_t    wdf_inc = params; // pos
                const string&     prefix  = params;
                termGenerator.index_text(text, 
                    static_cast<Xapian::termcount>(wdf_inc), 
                    prefix); 
                break;
            }

            case TERM:
            {
                // see xapian_document:append_term
                const string&           tname   = params; // value
                const Xapian::termcount wdf_inc = params; 
                // Pos = undefined
                doc.add_term(tname, wdf_inc);
                break;
            }

            case POSTING:
            {
                // see xapian_document:append_term
                const string&     tname   = params; // value
                const uint32_t    tpos    = params;
                const uint32_t    wdf_inc = params;
                doc.add_posting(tname, 
                    static_cast<Xapian::termpos>(tpos), 
                    static_cast<Xapian::termcount>(wdf_inc));
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}


void 
XapianErlangDriver::retrieveDocument(
    ParamDecoder params, 
    Xapian::Document& doc,
    Xapian::MSetIterator* mset_iter)
{
    while (const int8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case GET_VALUE:
            {
                const uint32_t    slot  = params;
                const string      value = doc.get_value(static_cast<Xapian::valueno>(slot));
                m_result << value;
                break;
            }

            case GET_DATA:
            {
                const string data = doc.get_data();
                m_result << data;
                break;
            }

            case GET_DOCID:
            {
                const Xapian::docid docid = doc.get_docid();
                m_result << static_cast<uint32_t>(docid);
                break;
            }

            case GET_WEIGHT:
            {
                if (mset_iter == NULL)
                    throw BadCommandDriverError(command);

                const Xapian::weight    w = mset_iter->get_weight();
                m_result << static_cast<double>(w);
                break;
            }

            case GET_RANK:
            {
                if (mset_iter == NULL)
                    throw BadCommandDriverError(command);

                const Xapian::doccount    r = mset_iter->get_rank();
                m_result << static_cast<uint32_t>(r);
                break;
            }

            case GET_PERCENT:
            {
                if (mset_iter == NULL)
                    throw BadCommandDriverError(command);

                const Xapian::doccount    p = mset_iter->get_rank();
                m_result << static_cast<uint8_t>(p);
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}



// -------------------------------------------------------------------
// Meta information for Erlang
// -------------------------------------------------------------------

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



// -------------------------------------------------------------------
// Call Erlang handler
// -------------------------------------------------------------------

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
