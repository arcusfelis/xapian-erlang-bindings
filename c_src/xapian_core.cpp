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
#include "user_resources.h"

#include <assert.h>
#include <cstdlib>

template class ObjectRegister<Xapian::Enquire>;
template class ObjectRegister<Xapian::MSet>;
template class ObjectRegister<const QlcTable>;
template class ObjectRegister<const Xapian::Weight>;
template class ObjectRegister<Xapian::KeyMaker>;
template class ObjectRegister<const Xapian::Query>;
template class ObjectRegister<const Xapian::MatchDecider>;
template class ObjectRegister<const Xapian::Stem>;
template class ObjectRegister<const Xapian::ExpandDecider>;
template class ObjectRegister<const Xapian::DateValueRangeProcessor>;
template class ObjectRegister<Xapian::MatchSpy>;

// used in user_resources
template class ObjectRegister<UserResource>;


// -------------------------------------------------------------------
// Globals
// -------------------------------------------------------------------

ResourceGenerator* gp_generator = NULL;


// -------------------------------------------------------------------
// Main Driver Class
// -------------------------------------------------------------------

#include "xapian_core.h"


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


/**
 * Create global variables
 */
int 
XapianErlangDriver::init()
{
    if (gp_generator == NULL)
    {
        gp_generator = new ResourceGenerator();
        registerUserCallbacks(*gp_generator);
    }
    return 0;
}


/**
 * Delete global variables
 */
void 
XapianErlangDriver::finish()
{
    if (gp_generator != NULL)
        delete gp_generator;

    gp_generator = NULL;
}


ErlDrvData 
XapianErlangDriver::start(
    ErlDrvPort port, 
    char* /* buf */)
{
    /* If the flag is set to PORT_CONTROL_FLAG_BINARY, 
       a binary will be returned. */       
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY); 
    assert(gp_generator != NULL);
    XapianErlangDriver* drv_data = new XapianErlangDriver(*gp_generator);
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


XapianErlangDriver::XapianErlangDriver(ResourceGenerator& generator)
: m_generator(generator), m_stores(generator)
{
    mp_db = NULL;
    mp_wdb = NULL;
    mp_default_stemmer = NULL;

    // RESOURCE_TYPE_ID_MARK
    m_stores.add(ResourceType::ENQUIRE,        &m_enquire_store);
    m_stores.add(ResourceType::MSET,           &m_mset_store);
    m_stores.add(ResourceType::QLC_TABLE,      &m_qlc_store);
    m_stores.add(ResourceType::WEIGHT,         &m_weight_store);
    m_stores.add(ResourceType::KEY_MAKER,      &m_key_maker_store);
    m_stores.add(ResourceType::QUERY,          &m_query_store);
    m_stores.add(ResourceType::MATCH_DECIDER,  &m_match_decider_store);
    m_stores.add(ResourceType::STEM,           &m_stem_store);
    m_stores.add(ResourceType::EXPAND_DECIDER, &m_expand_decider_store);
    m_stores.add(ResourceType::DATE_VALUE_RANGE_PROCESSOR, 
        &m_date_value_range_processor_store);
    m_stores.add(ResourceType::MATCH_SPY,      &m_match_spy_store);
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
    return m_stores.get(type);
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

    Xapian::doccount    first, maxitems;
    first = params;
    uint8_t is_undefined = params;
    maxitems = is_undefined 
        ? mp_db->get_doccount() 
        : params;
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
        case ResourceType::MSET:
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

        case QUERY_PARSER: // query_string
        {
            Xapian::QueryParser parser = readParser(params);
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
        uint32_t num = params;
        const Xapian::Weight& 
        weight = *m_weight_store.get(num);
        enquire.set_weighting_scheme(weight);
        break;
        }

    case EC_MATCH_SPY:
        {
        uint32_t num = params;
        Xapian::MatchSpy* 
        p_spy = m_match_spy_store.get(num);
        enquire.add_matchspy(p_spy);
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

        case GET_RESOURCE_INFO:
            return drv.getResourceInfo();

        case CREATE_RESOURCE:
            return drv.createResource(params);

        case MSET_INFO:
            return drv.msetInfo(params);

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
    m_stores.set_database(*mp_db);
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

                const Xapian::percent    p = mset_iter->get_percent();
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
// Resource Driver Helpers
// -------------------------------------------------------------------

/**
 * This function will be called inside xapian_drv:init
 */
size_t
XapianErlangDriver::getResourceInfo()
{
    ObjectRegister<UserResource>& 
    reg = m_generator.getRegister();
    ObjectRegister<UserResource>::Hash&
    elements = reg.getElements();

    typename ObjectRegister<UserResource>::Hash::iterator i, e, b;
    b = elements.begin();
    e = elements.end();
    for(i = b; i != e; i++)
    {
        uint32_t            num     = i->first;
        UserResource&       res     = * (i->second);
        const std::string&  name    = res.getName();
        uint8_t             type    = res.getType();
        m_result << type << num << name;
    }

    return m_result;
}


size_t 
XapianErlangDriver::createResource(ParamDecoder& params)
{
    uint32_t resource_num = m_stores.createAndRegister(params);
    m_result << resource_num;
    return m_result;
}


size_t 
XapianErlangDriver::msetInfo(ParamDecoder& params)
{
    uint32_t   mset_num = params;
    Xapian::MSet& mset = *m_mset_store.get(mset_num);
    while (uint8_t command = params)
    switch(command)
    {
        case MI_MATCHES_LOWER_BOUND:
            m_result << static_cast<uint32_t>(mset.get_matches_lower_bound());
            break;

        case MI_MATCHES_ESTIMATED:
            m_result << static_cast<uint32_t>(mset.get_matches_estimated());
            break;

        case MI_MATCHES_UPPER_BOUND:
            m_result << static_cast<uint32_t>(mset.get_matches_upper_bound());
            break;

        case MI_UNCOLLAPSED_MATCHES_LOWER_BOUND:
            m_result << static_cast<uint32_t>(
                mset.get_uncollapsed_matches_lower_bound());
            break;

        case MI_UNCOLLAPSED_MATCHES_ESTIMATED:
            m_result << static_cast<uint32_t>(
                    mset.get_uncollapsed_matches_estimated());
            break;

        case MI_UNCOLLAPSED_MATCHES_UPPER_BOUND:
            m_result << static_cast<uint32_t>(
                mset.get_uncollapsed_matches_upper_bound());
            break;

        case MI_SIZE:
            m_result << static_cast<uint32_t>(mset.size());
            break;

        case MI_GET_MAX_POSSIBLE:
            m_result << static_cast<double>(mset.get_max_possible());
            break;

        case MI_GET_MAX_ATTAINED:
            m_result << static_cast<double>(mset.get_max_attained());
            break;

        case MI_TERM_WEIGHT:
        {
            const std::string tname = params;
            m_result << static_cast<double>(mset.get_termweight(tname));
            break;
        }

        case MI_TERM_FREQ:
        {
            const std::string tname = params;
            m_result << static_cast<uint32_t>(mset.get_termfreq(tname));
            break;
        }

        default:
            throw BadCommandDriverError(command);
    }

    return m_result;
}



