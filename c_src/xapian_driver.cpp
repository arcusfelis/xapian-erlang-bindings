/* vim: set filetype=cpp shiftwidth=4 tabstop=4 expandtab tw=80: */

/**
 * Prefix m_ (member) for properties means that property is private.
 */

// -------------------------------------------------------------------
// Includes
// -------------------------------------------------------------------


/* Include other headers from the binding. */
#include "param_decoder.h"
#include "param_decoder_controller.h"
#include "result_encoder.h"
#include "xapian_exception.h"
/* Include it after "param_decoder_controller.h" */
#include "qlc_table.h"
#include "object_register.h"

#include <assert.h>
#include <cstdlib>

template class ObjectRegister<Xapian::Enquire>;
template class ObjectRegister<Xapian::MSet>;
template class ObjectRegister<const QlcTable>;




// -------------------------------------------------------------------
// Defines
// -------------------------------------------------------------------

/* Name of the so or dll library. */
#define DRIVER_NAME xapian_drv

/* These macroses help to transform an argument into a string. */
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)



/*
 * The length of free space in bytes, 
 * which will be allocatied new memory for storing result. 
 */
#define RESERVED_LEN 100

/* Helper used by ResultEncoder. */
#define SIZE_OF_SEGMENT(LEN) (sizeof(DataSegment) + (LEN) - 1)

/* Helper used by ResultEncoder. */
//#define PUT_VALUE(X) (put((char*) &(X), sizeof(X)))
#define PUT_VALUE(X) (put(reinterpret_cast<char*>( &(X) ), sizeof(X)))

/* Uncomment to disable asserts. */
//#define NDEBUG








// -------------------------------------------------------------------
// Main Driver Class
// -------------------------------------------------------------------

#include "xapian_driver.h"


const uint8_t XapianErlangDriver::PARSER_FEATURE_COUNT = 13;
const unsigned 
XapianErlangDriver::PARSER_FEATURES[PARSER_FEATURE_COUNT] = {
        0,
    /*  1 */ Xapian::QueryParser::FLAG_BOOLEAN,
    /*  2 */ Xapian::QueryParser::FLAG_PHRASE,
    /*  3 */ Xapian::QueryParser::FLAG_LOVEHATE,
    /*  4 */ Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE,
    /*  5 */ Xapian::QueryParser::FLAG_WILDCARD,
    /*  6 */ Xapian::QueryParser::FLAG_PURE_NOT,
    /*  7 */ Xapian::QueryParser::FLAG_PARTIAL,
    /*  8 */ Xapian::QueryParser::FLAG_SPELLING_CORRECTION,
    /*  9 */ Xapian::QueryParser::FLAG_SYNONYM,
    /* 10 */ Xapian::QueryParser::FLAG_AUTO_SYNONYMS,
    /* 11 */ Xapian::QueryParser::FLAG_AUTO_MULTIWORD_SYNONYMS,
    /* 12 */ Xapian::QueryParser::FLAG_DEFAULT
};


const uint8_t XapianErlangDriver::STEM_STRATEGY_COUNT = 3;
const Xapian::QueryParser::stem_strategy
XapianErlangDriver::STEM_STRATEGIES[STEM_STRATEGY_COUNT] = {
    /*  0 */ Xapian::QueryParser::STEM_NONE, // default
    /*  1 */ Xapian::QueryParser::STEM_SOME,
    /*  2 */ Xapian::QueryParser::STEM_ALL
};


const uint8_t XapianErlangDriver::DOCID_ORDER_TYPE_COUNT = 3;
const Xapian::Enquire::docid_order
XapianErlangDriver::DOCID_ORDER_TYPES[DOCID_ORDER_TYPE_COUNT] = {
    /*  0 */ Xapian::Enquire::ASCENDING, // default
    /*  1 */ Xapian::Enquire::DESCENDING,
    /*  2 */ Xapian::Enquire::DONT_CARE
};





ErlDrvData 
XapianErlangDriver::start(
    ErlDrvPort port, 
    char* /* buf */)
{
    /* If the flag is set to PORT_CONTROL_FLAG_BINARY, 
       a binary will be returned. */       
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY); 
    XapianErlangDriver* drv_data = new XapianErlangDriver();
    return reinterpret_cast<ErlDrvData>( drv_data );
}


void 
XapianErlangDriver::stop(
    ErlDrvData drv_data) 
{
    XapianErlangDriver* 
    drv = reinterpret_cast<XapianErlangDriver*>( drv_data );

    if (drv != NULL)
        delete drv;
}   

ResultEncoder* 
XapianErlangDriver::getResultEncoder()
{
    return &m_result;
}

XapianErlangDriver::XapianErlangDriver()
{
    mp_db = NULL;
    mp_wdb = NULL;
    mp_default_stemmer = NULL;
    // RESOURCE_TYPE_ID_MARK
    m_stores[0] = &m_enquire_store;
    m_stores[1] = &m_mset_store;
    m_stores[2] = &m_qlc_store;
}

XapianErlangDriver::~XapianErlangDriver()
{
    if (mp_db != NULL) 
        delete mp_db;

    if (mp_default_stemmer != NULL)
        delete mp_default_stemmer;
}

void 
XapianErlangDriver::setDefaultStemmer(const Xapian::Stem* stemmer)
{
    if (mp_default_stemmer != NULL)
        delete mp_default_stemmer;

    mp_default_stemmer = stemmer;
    m_default_parser.set_stemmer(*mp_default_stemmer);
}


size_t 
XapianErlangDriver::setDefaultStemmer(ParamDecoder& params)
{
    const Xapian::Stem&  stemmer = params;
    setDefaultStemmer(new Xapian::Stem(stemmer));
    return m_result;
}

size_t 
XapianErlangDriver::setDefaultPrefixes(ParamDecoder& params)
{
    const uint32_t count   = params;
    for (uint32_t i = 0; i < count; i++)
    {
        addPrefix(m_default_parser, params);
    }
    return m_result;
}

ObjectBaseRegister&
XapianErlangDriver::getRegisterByType(uint8_t type)
{
    if (type > m_stores_count)
      throw BadCommandDriverError(type);
    return * (m_stores[type]);
}

size_t 
XapianErlangDriver::getLastDocId()
{
    //Xapian::docid get_lastdocid() const
    const Xapian::docid 
    docid = mp_db->get_lastdocid();
    m_result << static_cast<uint32_t>(docid);
    return m_result;
}


size_t 
XapianErlangDriver::addDocument(ParamDecoder& params)
{
    Xapian::Document doc;
    applyDocument(params, doc);
    const Xapian::docid
    docid = mp_wdb->add_document(doc);
    m_result << static_cast<uint32_t>(docid);
    return m_result;
}


size_t 
XapianErlangDriver::query(ParamDecoder& params)
{
    /* offset, pagesize, query, template */
    const uint32_t offset   = params;
    const uint32_t pagesize = params;

    // Use an Enquire object on the database to run the query.
    Xapian::Enquire enquire(*mp_db);
    Xapian::Query   query = buildQuery(params);
    enquire.set_query(query);
     

    // Get an result
    Xapian::MSet mset = enquire.get_mset(
        static_cast<Xapian::termcount>(offset), 
        static_cast<Xapian::termcount>(pagesize));

    Xapian::doccount count = mset.size();
    m_result << static_cast<uint32_t>(count);

    for (Xapian::MSetIterator m = mset.begin(); m != mset.end(); ++m) {
      //Xapian::docid did = *m;
        Xapian::Document doc = m.get_document();
        retrieveDocument(params, doc, &m);
    }
    return m_result;
}


size_t 
XapianErlangDriver::enquire(ParamDecoder& params)
{
    // Use an Enquire object on the database to run the query.
    Xapian::Enquire* p_enquire = new Xapian::Enquire(*mp_db);
    fillEnquire(*p_enquire, params);

    uint32_t num = m_enquire_store.put(p_enquire);
    m_result << num;

    return m_result;
}


size_t 
XapianErlangDriver::releaseResource(ParamDecoder& params)
{
    uint8_t   type = params;
    uint32_t   num = params;
    ObjectBaseRegister&
    reg = getRegisterByType(type);
    reg.remove(num);

    return m_result;
}


size_t 
XapianErlangDriver::matchSet(ParamDecoder& params)
{
    uint32_t   enquire_num = params;
    Xapian::Enquire& enquire = *m_enquire_store.get(enquire_num);

    Xapian::doccount    first    = 0;
    Xapian::doccount    maxitems =  mp_db->get_doccount();
    Xapian::MSet mset = enquire.get_mset(
        first, 
        maxitems);

    uint32_t mset_num = m_mset_store.put(new Xapian::MSet(mset));
    m_result << mset_num;

    return m_result;
}


size_t 
XapianErlangDriver::qlcInit(ParamDecoder& params)
{
    uint8_t   resource_type = params;
    uint32_t   resource_num = params;
    switch (resource_type)
    {
        case MSET_RESOURCE_TYPE:
        {
            Xapian::MSet& mset = *m_mset_store.get(resource_num);
            const ParamDecoderController& schema  
                = retrieveDocumentSchema(params);
            const MSetQlcTable* qlcTable = new MSetQlcTable(*this, mset, schema);
            const uint32_t qlc_num = m_qlc_store.put(qlcTable);
            const uint32_t mset_size = qlcTable->numOfObjects();
        
            m_result << qlc_num << mset_size;
            return m_result;
        }

        default:
            throw BadCommandDriverError(resource_type);
    }
}


size_t 
XapianErlangDriver::qlcNext(ParamDecoder& params)
{
    uint32_t   resource_num = params;
    uint32_t   from         = params;
    uint32_t   count        = params;
 
    const QlcTable& qlcTable = *m_qlc_store.get(resource_num);
    qlcTable.getPage(from, count);
    return m_result;
}


size_t 
XapianErlangDriver::qlcLookup(ParamDecoder& params)
{
    uint32_t   resource_num = params;
 
    const QlcTable& qlcTable = *m_qlc_store.get(resource_num);
    qlcTable.lookup(params);
    return m_result;
}


void 
XapianErlangDriver::assertWriteable() const
{
    if (mp_wdb == NULL)
        throw NotWritableDatabaseError();
}

size_t 
XapianErlangDriver::startTransaction()
{
    assertWriteable();
    mp_wdb->begin_transaction();
    return m_result;
}

size_t 
XapianErlangDriver::cancelTransaction()
{
    assertWriteable();
    mp_wdb->cancel_transaction();
    return m_result;
}

size_t 
XapianErlangDriver::commitTransaction()
{
    assertWriteable();
    mp_wdb->commit_transaction();
    return m_result;
}

size_t 
XapianErlangDriver::getDocumentById(ParamDecoder& params)
{
    const Xapian::docid docid = params;
    Xapian::Document doc = mp_db->get_document(docid);
    retrieveDocument(params, doc, NULL);
    return m_result;
}


size_t 
XapianErlangDriver::test(ParamDecoder& params)
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

size_t 
XapianErlangDriver::testResultEncoder(Xapian::docid from, Xapian::docid to)
{
    for (; from <= to; from++)
        m_result << static_cast<uint32_t>(from);
    return m_result;
}

size_t 
XapianErlangDriver::testException()
{
    throw MemoryAllocationDriverError(1000);
    return 0;
}

unsigned
XapianErlangDriver::idToParserFeature(uint8_t type)
{
  if (type > PARSER_FEATURE_COUNT)
    throw BadCommandDriverError(type);
  return PARSER_FEATURES[type];
}

unsigned 
XapianErlangDriver::decodeParserFeatureFlags(ParamDecoder& params)
{
    unsigned flags = 0;
    while (const uint8_t type = params)
    {
        flags |= idToParserFeature(type);
    }
    return flags;
}


Xapian::QueryParser::stem_strategy
XapianErlangDriver::readStemmingStrategy(ParamDecoder& params)
{
  const uint8_t type = params;
  if (type > STEM_STRATEGY_COUNT)
    throw BadCommandDriverError(type);
  return STEM_STRATEGIES[type];
}


void 
XapianErlangDriver::addPrefix(Xapian::QueryParser& qp, ParamDecoder& params)
{
    const std::string&      field          = params;
    const std::string&      prefix         = params;
    const bool              is_boolean     = params;
    const bool              is_exclusive   = params;

    if (is_boolean)
        qp.add_boolean_prefix(field, prefix, is_exclusive);
    else
        qp.add_prefix(field, prefix);
}


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
            std::vector<Xapian::Query> subQueries;

            for (uint32_t i = 0; i < subQueryCount; i++)
                subQueries.push_back(buildQuery(params));

            std::vector<Xapian::Query>::iterator qbegin = subQueries.begin();
            std::vector<Xapian::Query>::iterator qend   = subQueries.end();
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
            const std::string&      value    = params;
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
            const std::string&      from     = params;
            const std::string&      to       = params;
            Xapian::Query q(
                static_cast<Xapian::Query::op>(op), 
                slot, 
                from,
                to);
            return q;
        }

        case QUERY_TERM:
        {    
            const std::string& name     = params;
            const uint32_t     wqf      = params;
            const uint32_t     pos      = params;
            Xapian::Query q(
                name,
                wqf, 
                pos);
            return q;
        }

        case QUERY_PARSER:
        {
            Xapian::QueryParser parser    = readParser(params);
            const std::string&  query_string   = params;
            const std::string&  default_prefix = params;
            const unsigned flags               = decodeParserFeatureFlags(params); 

            Xapian::Query q = 
            parser.parse_query(
                query_string, 
                flags, 
                default_prefix);
            return q;
        }

        default:
            throw BadCommandDriverError(type);
    }
}


void 
XapianErlangDriver::fillEnquire(Xapian::Enquire& enquire, ParamDecoder& params)
{
    Xapian::termcount   qlen = 0;

    while (uint8_t command = params)
    switch (command)
    {
    case EC_QUERY:
        {
        Xapian::Query   query = buildQuery(params);
        enquire.set_query(query, qlen);
        break;
        }

    case EC_QUERY_LEN:
        {
        uint32_t value = params;
        qlen = value;
        break;
        }

    case EC_ORDER:
        {
        uint8_t type   = params;
        bool reverse   = params;
        uint32_t value = params;
        fillEnquireOrder(enquire, type, value, reverse);
        break;
        }

    case EC_DOCID_ORDER:
        {
        uint8_t type   = params;
        if (type >= DOCID_ORDER_TYPE_COUNT)
            throw BadCommandDriverError(type);
        
        Xapian::Enquire::docid_order order = DOCID_ORDER_TYPES[type];
        enquire.set_docid_order(order);
        break;
        }

    case EC_WEIGHTING_SCHEME:
        {
      //const Weight &  weight;
      //enquire.set_weighting_scheme(weight);
        break;
        }

    case EC_CUTOFF:
        {
        uint8_t percent_cutoff   = params;
        double  weight_cutoff    = params;
        enquire.set_cutoff(percent_cutoff, weight_cutoff);
        break;
        }

    case EC_COLLAPSE_KEY:
        {
        uint32_t collapse_key = params;
        uint32_t collapse_max = params;
        enquire.set_collapse_key(
            !collapse_key ? Xapian::BAD_VALUENO : collapse_key, 
            collapse_max);
        break;
        }

    default:
        throw BadCommandDriverError(command);
    }
}

void
XapianErlangDriver::fillEnquireOrder(Xapian::Enquire& enquire, 
    const uint8_t type, const uint32_t value, const bool reverse)
{
    switch(type)
    {
    case OT_KEY:
      //Xapian::KeyMaker *      
      //enquire.set_sort_by_key(sorter, reverse);
      //break;
    case OT_KEY_RELEVANCE:
      //enquire.set_sort_by_key_then_relevance(sorter, reverse);
      //break;
    case OT_RELEVANCE_KEY:
      //enquire.set_sort_by_relevance_then_key(sorter, reverse);
      //break;

    case OT_VALUE:
        enquire.set_sort_by_value(value, reverse);
        break;

    case OT_RELEVANCE_VALUE:
        enquire.set_sort_by_relevance_then_value(value, reverse);
        break;

    case OT_VALUE_RELEVANCE:
        enquire.set_sort_by_value_then_relevance(value, reverse);
        break;

    default:
        throw BadCommandDriverError(type);
    }
}

Xapian::QueryParser 
XapianErlangDriver::selectParser(ParamDecoder& params)
{
    uint8_t type = params;
    switch (type)
    {
    case QP_TYPE_DEFAULT:
        return m_default_parser;

    case QP_TYPE_EMPTY:
        return m_empty_parser;

    default:
        throw BadCommandDriverError(type);
    }
}


Xapian::QueryParser 
XapianErlangDriver::readParser(ParamDecoder& params)
{
  uint8_t command = params;
  // No parameters?
  // DEFAULT_PARSER_CHECK_MARK -- mark for Erlang
  if (!command)
    return m_default_parser;
 
  // Clone parser
  Xapian::QueryParser qp = m_default_parser;
  do
  {
    switch (command)
    {
    case QP_PARSER_TYPE: 
        qp = selectParser(params);
        break; 

    case QP_STEMMER: 
        {
        const Xapian::Stem&  stemmer = params;
        qp.set_stemmer(stemmer);
        break; 
        }

    case QP_STEMMING_STRATEGY: 
        {
        Xapian::QueryParser::stem_strategy 
        strategy = readStemmingStrategy(params);
        qp.set_stemming_strategy(strategy);
        break;
        }

    case QP_MAX_WILDCARD_EXPANSION:
        {
        const uint32_t   limit = params;
        qp.set_max_wildcard_expansion(static_cast<Xapian::termcount>(limit));
        break;
        }

    case QP_DEFAULT_OP:
        {
        const uint8_t     default_op    = params;
        qp.set_default_op(static_cast<Xapian::Query::op>(default_op));
        break;
        }

    case QP_PREFIX:
        addPrefix(qp, params);
        break;

    default:
        throw BadCommandDriverError(command);
    }
  } while((command = params)); // yes, it's an assignment [-Wparentheses]
  // warning: suggest parentheses around assignment used as truth value

  return qp;
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
            const std::string&   dbpath = params;
            const int8_t         mode = params;
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

        case SET_DEFAULT_STEMMER:
            return drv.setDefaultStemmer(params);

        case SET_DEFAULT_PREFIXES:
            return drv.setDefaultPrefixes(params);

        case ENQUIRE:
            return drv.enquire(params);

        case RELEASE_RESOURCE:
            return drv.releaseResource(params);

        case MATCH_SET:
            return drv.matchSet(params);

        case QLC_INIT:
            return drv.qlcInit(params);

        case QLC_NEXT_PORTION:
            return drv.qlcNext(params);

        case QLC_LOOKUP:
            return drv.qlcLookup(params);

        default:
            throw BadCommandDriverError(command);
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
XapianErlangDriver::open(const std::string& dbpath, int8_t mode)
{
    // Is already opened?
    if (mp_db != NULL)
        throw DbAlreadyOpenedDriverError();

    switch(mode) {
        // Open readOnly db
        case READ_OPEN:
            mp_db = new Xapian::Database(dbpath);
            break;

        // open for read/write; create if no db exists
        case WRITE_CREATE_OR_OPEN:
            mp_db = mp_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_CREATE_OR_OPEN);
            break;

        // create new database; fail if db exists
        case WRITE_CREATE:
            mp_db = mp_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_CREATE);
            break;

        // overwrite existing db; create if none exists
        case WRITE_CREATE_OR_OVERWRITE:
            mp_db = mp_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_CREATE_OR_OVERWRITE);
            break;

        // open for read/write; fail if no db exists
        case WRITE_OPEN:
            mp_db = mp_wdb = new Xapian::WritableDatabase(dbpath, 
                Xapian::DB_OPEN);
            break;

        default:
            throw BadCommandDriverError(mode);
    }
    m_default_parser.set_database(*mp_db);
    m_empty_parser.set_database(*mp_db);
    return m_result;
}


void 
XapianErlangDriver::applyDocument(
    ParamDecoder& params, 
    Xapian::Document& doc)
{
    Xapian::TermGenerator   termGenerator;
    termGenerator.set_document(doc);
    if (mp_default_stemmer != NULL)
        termGenerator.set_stemmer(*mp_default_stemmer);

    while (const int8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case STEMMER:
            {
                // see xapian_document:append_stemmer
                const Xapian::Stem&  stemmer = params;
                termGenerator.set_stemmer(stemmer);
                break;
            }

            case DATA:
            {
                // see xapian_document:append_data
                const std::string&   data = params;
                doc.set_data(data);
                break;
            }

            case VALUE:
            {
                // see xapian_document:append_value
                const uint32_t         slot  = params;
                const std::string&     value = params;
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
                const std::string&     text    = params; // value
                const uint32_t         wdf_inc = params; // pos
                const std::string&     prefix  = params;
                termGenerator.index_text(text, 
                    static_cast<Xapian::termcount>(wdf_inc), 
                    prefix); 
                break;
            }

            case TERM:
            {
                // see xapian_document:append_term
                const std::string&           tname   = params; // value
                const Xapian::termcount      wdf_inc = params; 
                // Pos = undefined
                doc.add_term(tname, wdf_inc);
                break;
            }

            case POSTING:
            {
                // see xapian_document:append_term
                const std::string&     tname   = params; // value
                const uint32_t         tpos    = params;
                const uint32_t         wdf_inc = params;
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
    ParamDecoder params,  /* yes, it is a copy */
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
                const uint32_t     slot  = params;
                const std::string  value = doc.get_value(static_cast<Xapian::valueno>(slot));
                m_result << value;
                break;
            }

            case GET_DATA:
            {
                const std::string data = doc.get_data();
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

ParamDecoderController
XapianErlangDriver::retrieveDocumentSchema(
    ParamDecoder& params) const
{
    const char* from = params.currentPosition();

    while (const int8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case GET_VALUE:
            {
                //static_cast<uint32_t>( params ); // slot
                uint32_t slot = params; // slot
                (void) slot;
                break;
            }

            case GET_DATA:
            case GET_DOCID:
            case GET_WEIGHT:
            case GET_RANK:
            case GET_PERCENT:
                break;

            default:
                throw BadCommandDriverError(command);
        }
    }

    const char* to = params.currentPosition();

    size_t len = to - from;
    ParamDecoderController ctrl(from, len);
    return ctrl;
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
    const_cast<char*>( STR(DRIVER_NAME) ), /* it is a macro */

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
