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
#include "qlc_table.h"
#include "object_register.h"
#include "user_resources.h"
#include "spy_ctrl.h"
#include "termiter_doc_gen.h"

#include <assert.h>
#include <cstdlib>

// -------------------------------------------------------------------
// Main Driver Class
// -------------------------------------------------------------------

#include "xapian_core.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN


// -------------------------------------------------------------------
// Globals
// -------------------------------------------------------------------

ResourceGenerator* gp_generator = NULL;


const uint8_t Driver::PARSER_FEATURE_COUNT = 13;
const unsigned 
Driver::PARSER_FEATURES[PARSER_FEATURE_COUNT] = {
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


const uint8_t Driver::STEM_STRATEGY_COUNT = 3;
const Xapian::QueryParser::stem_strategy
Driver::STEM_STRATEGIES[STEM_STRATEGY_COUNT] = {
    /*  0 */ Xapian::QueryParser::STEM_NONE, // default
    /*  1 */ Xapian::QueryParser::STEM_SOME,
    /*  2 */ Xapian::QueryParser::STEM_ALL
};


const uint8_t Driver::DOCID_ORDER_TYPE_COUNT = 3;
const Xapian::Enquire::docid_order
Driver::DOCID_ORDER_TYPES[DOCID_ORDER_TYPE_COUNT] = {
    /*  0 */ Xapian::Enquire::ASCENDING, // default
    /*  1 */ Xapian::Enquire::DESCENDING,
    /*  2 */ Xapian::Enquire::DONT_CARE
};


/**
 * Create global variables
 */
int 
Driver::init()
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
Driver::finish()
{
    if (gp_generator != NULL)
        delete gp_generator;

    gp_generator = NULL;
}


ErlDrvData 
Driver::start(
    ErlDrvPort port, 
    char* /* buf */)
{
    /* If the flag is set to PORT_CONTROL_FLAG_BINARY, 
       a binary will be returned. */       
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY); 
    assert(gp_generator != NULL);
    Driver* drv_data = new Driver(*gp_generator);
    return reinterpret_cast<ErlDrvData>( drv_data );
}


void 
Driver::stop(
    ErlDrvData drv_data) 
{
    Driver* 
    drv = reinterpret_cast<Driver*>( drv_data );

    if (drv != NULL)
        delete drv;
}   


ResultEncoder* 
Driver::getResultEncoder()
{
    return &m_result;
}


Driver::Driver(ResourceGenerator& generator)
: m_generator(generator), m_stores(generator), m_number_of_databases(0)
{
    // RESOURCE_TYPE_ID_MARK
    m_stores.add(ResourceType::DOCUMENT,       &m_document_store);
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
    m_stores.add(ResourceType::DOCUMENT,       &m_document_store);


    m_default_parser.set_database(m_db);
    m_empty_parser.set_database(m_db);
    m_stores.set_database(m_db);
}


Driver::~Driver()
{}


void 
Driver::setDefaultStemmer(const Xapian::Stem& stemmer)
{
    m_default_stemmer = stemmer;
    m_default_parser.set_stemmer(m_default_stemmer);
}


size_t 
Driver::setDefaultStemmer(ParamDecoder& params)
{
    const Xapian::Stem&  stemmer = params;
    setDefaultStemmer(Xapian::Stem(stemmer));
    return m_result;
}


size_t 
Driver::setDefaultPrefixes(ParamDecoder& params)
{
    const uint32_t count   = params;
    for (uint32_t i = 0; i < count; i++)
    {
        addPrefix(m_default_parser, params);
    }
    return m_result;
}


ObjectBaseRegister&
Driver::getRegisterByType(uint8_t type)
{
    return m_stores.get(type);
}


size_t 
Driver::getLastDocId()
{
    //Xapian::docid get_lastdocid() const
    const Xapian::docid 
    docid = m_db.get_lastdocid();
    m_result << static_cast<uint32_t>(docid);
    return m_result;
}


size_t 
Driver::addDocument(ParamDecoder& params)
{
    assertWriteable();

    Xapian::Document doc;
    applyDocument(params, doc);
    const Xapian::docid
    docid = m_wdb.add_document(doc);
    m_result << static_cast<uint32_t>(docid);
    return m_result;
}


size_t 
Driver::replaceDocument(ParamDecoder& params)
{
    assertWriteable();

    Xapian::Document doc;
    Xapian::docid docid;

    applyDocument(params, doc);
    switch(uint8_t idType = params)
    {
        case UNIQUE_DOCID:
        {
            docid = params;
            m_wdb.replace_document(docid, doc);
            break;
        }

        case UNIQUE_TERM:
        {
            const std::string& unique_term = params;
            docid = m_wdb.replace_document(unique_term, doc);
            break;
        }

        default:
            throw BadCommandDriverError(idType);
    }
        
    m_result << static_cast<uint32_t>(docid);
    return m_result;
}


size_t 
Driver::updateDocument(ParamDecoder& params, bool create)
{
    assertWriteable();
    const ParamDecoderController& schema  
        = applyDocumentSchema(params);
    

    Xapian::Document doc;
    Xapian::docid docid;

    switch(uint8_t idType = params)
    {
        case UNIQUE_DOCID:
        {
            docid = params;
            
            // If create = true, then ignore errors.
            if (create)
                try {
                    doc = m_wdb.get_document(docid);
                } catch (Xapian::DocNotFoundError e) {}
            else 
                doc = m_wdb.get_document(docid);

            ParamDecoder params = schema;
            applyDocument(params, doc);
            m_wdb.replace_document(docid, doc);
            break;
        }

        case UNIQUE_TERM:
        {
            const std::string& unique_term = params;
            if (m_wdb.term_exists(unique_term))
            {
                // Start searching
                Xapian::Enquire     enquire(m_wdb);
                enquire.set_query(Xapian::Query(unique_term));

                // Get a set of documents with term
                Xapian::MSet mset = enquire.get_mset(
                    0, m_wdb.get_doccount());
                
                for (Xapian::MSetIterator m = mset.begin(); 
                        m != mset.end(); ++m) {
                    docid = *m;
                    Xapian::Document doc = m.get_document();
                    ParamDecoder params = schema;
                    applyDocument(params, doc);
                    m_wdb.replace_document(docid, doc);
                }
                // new document was not added added
                docid = 0;
            }
            else if (create)
            {
                
                ParamDecoder params = schema;
                applyDocument(params, doc);
                docid = m_wdb.add_document(doc);
            }
            else 
            {
                throw BadArgumentDriverError();
            }
            break;
        }

        default:
            throw BadCommandDriverError(idType);
    }
        
    m_result << static_cast<uint32_t>(docid);
    return m_result;
}


void 
Driver::deleteDocument(ParamDecoder& params)
{
    assertWriteable();

    switch(uint8_t idType = params)
    {
        case UNIQUE_DOCID:
        {
            const Xapian::docid
            docid = params;
            m_wdb.delete_document(docid);
            break;
        }

        case UNIQUE_TERM:
        {
            const std::string& unique_term = params;
            m_wdb.delete_document(unique_term);
            break;
        }

        default:
            throw BadCommandDriverError(idType);
    }
}

size_t 
Driver::query(ParamDecoder& params)
{
    /* offset, pagesize, query, template */
    const uint32_t offset   = params;
    const uint32_t pagesize = params;

    // Use an Enquire object on the database to run the query.
    Xapian::Enquire enquire(m_db);
    Xapian::Query   query = buildQuery(params);
    enquire.set_query(query);
     
    // Get an result
    Xapian::MSet mset = enquire.get_mset(
        static_cast<Xapian::termcount>(offset), 
        static_cast<Xapian::termcount>(pagesize));

    Xapian::doccount count = mset.size();
    m_result << static_cast<uint32_t>(count);
    retrieveDocuments(params, mset.begin(), mset.end());
    return m_result;
}

void
Driver::retrieveDocuments(ParamDecoder params, 
    Xapian::MSetIterator iter, Xapian::MSetIterator end)
{
    ParamDecoder params_copy = params;
    switch (uint8_t decoder_type = params_copy)
    {
        case DEC_DOCUMENT:
            for (; iter != end; ++iter)
            {
                Xapian::Document doc = iter.get_document();
                retrieveDocument(params, doc);
            }
            break;

        case DEC_ITERATOR:
            for (; iter != end; ++iter)
                retrieveDocument(params, iter);
            break;

        case DEC_BOTH:
            for (; iter != end; ++iter)
            {
                Xapian::Document doc = iter.get_document();
                retrieveDocument(params, doc, iter);
            }
            break;

        default:
            throw BadCommandDriverError(decoder_type);
    }
}


void
Driver::selectEncoderAndRetrieveDocument(
    ParamDecoder& params, Xapian::MSetIterator& iter)
{
    ParamDecoder params_copy = params;
    switch (uint8_t decoder_type = params_copy)
    {
        case DEC_DOCUMENT:
        {
            Xapian::Document doc = iter.get_document();
            retrieveDocument(params, doc);
            break;
        }

        case DEC_ITERATOR:
            retrieveDocument(params, iter);
            break;

        case DEC_BOTH:
        {
            Xapian::Document doc = iter.get_document();
            retrieveDocument(params, doc, iter);
            break;
        }

        default:
            throw BadCommandDriverError(decoder_type);
    }
}


size_t 
Driver::enquire(ParamDecoder& params)
{
    // Use an Enquire object on the database to run the query.
    Xapian::Enquire* p_enquire = new Xapian::Enquire(m_db);
    fillEnquire(*p_enquire, params);

    // m_enquire_store will call the delete operator.
    uint32_t num = m_enquire_store.put(p_enquire);
    m_result << num;

    return m_result;
}


// Get a copy of a document.
// Caller must deallocate the returned object.
Xapian::Document
Driver::getDocument(ParamDecoder& params)
{
    switch(uint8_t idType = params)
    {
        case UNIQUE_DOCID:
        {
            Xapian::docid docid = params;
            return m_db.get_document(docid);
        }

        case UNIQUE_TERM:
        {
            const std::string& unique_term = params;
            if (m_wdb.term_exists(unique_term))
            {
                // Start searching
                Xapian::Enquire     enquire(m_wdb);
                enquire.set_query(Xapian::Query(unique_term));

                // Get a set of documents with term
                Xapian::MSet mset = enquire.get_mset(
                    0, 1);
                Xapian::MSetIterator 
                    iter = mset.begin(),
                    end  = mset.end();
                if (iter == end) 
                    throw BadArgumentDriverError(); // doc us not found

                return iter.get_document();
            }
            break;
        }

        default:
            throw BadCommandDriverError(idType);
    }
    throw BadArgumentDriverError();
}


// Create a doc as a resource
size_t
Driver::document(ParamDecoder& params)
{
    const Xapian::Document& doc = getDocument(params);
    // m_document_store will call delete
    uint32_t num = m_document_store.put(new Xapian::Document(doc));
    m_result << num;

    return m_result;
}


size_t 
Driver::releaseResource(ParamDecoder& params)
{
    uint8_t   type = params;
    uint32_t   num = params;
    ObjectBaseRegister&
    reg = getRegisterByType(type);
    reg.remove(num);

    return m_result;
}


size_t 
Driver::matchSet(ParamDecoder& params)
{
    uint32_t   enquire_num = params;
    Xapian::Enquire& enquire = *m_enquire_store.get(enquire_num);

    Xapian::doccount    first, maxitems, checkatleast;
    first = params;
    uint8_t is_undefined = params;
    maxitems = is_undefined 
        ? m_db.get_doccount() 
        : params;
    checkatleast = params;


    while (const uint32_t num = params)
    {
        SpyController&
        spy = *m_match_spy_store.get(num);

        if (!spy.is_finalized())
        {
            // It can be added just once
            enquire.add_matchspy(spy.getSpy());
            spy.finalize();
        } else {
            throw MatchSpyFinalizedDriverError();
        }
        break;
    }

    Xapian::MSet mset = enquire.get_mset(
        first, 
        maxitems,
        checkatleast);

    enquire.clear_matchspies();

    // m_mset_store will call delete
    uint32_t mset_num = m_mset_store.put(new Xapian::MSet(mset));
    m_result << mset_num;

    return m_result;
}


size_t 
Driver::qlcInit(ParamDecoder& params)
{
    uint8_t   qlc_type      = params;
    uint8_t   resource_type = params;
    uint32_t  resource_num  = params;
    switch (qlc_type)
    {
        case QlcType::MSET:
        {
            assert(resource_type == ResourceType::MSET);
             
            Xapian::MSet& mset = *m_mset_store.get(resource_num);
            const ParamDecoderController& schema  
                = retrieveDocumentSchema(params);
            MSetQlcTable* qlcTable = new MSetQlcTable(*this, mset, schema);
            // m_qlc_store will call delete
            const uint32_t qlc_num = m_qlc_store.put(qlcTable);
            const uint32_t mset_size = qlcTable->numOfObjects();
        
            m_result << qlc_num << mset_size;
            return m_result;
        }

        case QlcType::TERMS:
        case QlcType::SPY_TERMS:
        {
            TermIteratorGenerator* p_gen = 
            termGenerator(params, qlc_type, resource_type, resource_num);
            const ParamDecoderController& schema  
                = retrieveTermSchema(params);
            TermQlcTable* qlcTable = new TermQlcTable(*this, p_gen, schema);
            // qlcTable is now a master of p_gen object. 
            // m_qlc_store will call delete
            const uint32_t qlc_num = m_qlc_store.put(qlcTable);
            const uint32_t list_size = qlcTable->numOfObjects();
        
            m_result << qlc_num << list_size;
            return m_result;
        }

        default:
            throw BadCommandDriverError(resource_type);
    }
}


/**
 * Caller must delete returned value.
 */
TermIteratorGenerator*
Driver::termGenerator(ParamDecoder& params, 
    const /*QlcType*/ int8_t qlc_type, 
    const /*ResourceType*/ int8_t resource_type, 
    const uint32_t resource_num)
{
    switch (qlc_type)
    {
        case QlcType::TERMS:
        {
            assert(resource_type == ResourceType::DOCUMENT);
            Xapian::Document& doc = *m_document_store.get(resource_num);
            return new DocumentTermIteratorGenerator(doc);
        }

        case QlcType::SPY_TERMS:
        {
            // Init commons
            assert(resource_type == ResourceType::MATCH_SPY);

            SpyController&
            spy = *m_match_spy_store.get(resource_num);
            return spy.getIteratorGenerator(params);
        }

        default:
            throw BadCommandDriverError(resource_type);
    }
}

size_t 
Driver::qlcNext(ParamDecoder& params)
{
    uint32_t   resource_num = params;
    uint32_t   from         = params;
    uint32_t   count        = params;
 
    QlcTable& qlcTable = *m_qlc_store.get(resource_num);
    qlcTable.getPage(from, count);
    return m_result;
}


size_t 
Driver::qlcLookup(ParamDecoder& params)
{
    uint32_t   resource_num = params;
 
    QlcTable& qlcTable = *m_qlc_store.get(resource_num);
    qlcTable.lookup(params);
    return m_result;
}


void 
Driver::assertWriteable() const
{}


size_t 
Driver::startTransaction()
{
    assertWriteable();

    m_wdb.begin_transaction();
    return m_result;
}


size_t 
Driver::cancelTransaction()
{
    assertWriteable();

    m_wdb.cancel_transaction();
    return m_result;
}


size_t 
Driver::commitTransaction()
{
    assertWriteable();

    m_wdb.commit_transaction();
    return m_result;
}


size_t 
Driver::getDocumentById(ParamDecoder& params)
{
    const Xapian::docid docid = params;
    Xapian::Document doc = m_db.get_document(docid);
    retrieveDocument(params, doc);
    return m_result;
}


size_t 
Driver::test(ParamDecoder& params)
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
Driver::testResultEncoder(Xapian::docid from, Xapian::docid to)
{
    for (; from <= to; from++)
        m_result << static_cast<uint32_t>(from);
    return m_result;
}


size_t 
Driver::testException()
{
    throw MemoryAllocationDriverError(1000);
    return 0;
}


unsigned
Driver::idToParserFeature(uint8_t type)
{
  if (type > PARSER_FEATURE_COUNT)
    throw BadCommandDriverError(type);
  return PARSER_FEATURES[type];
}


unsigned 
Driver::decodeParserFeatureFlags(ParamDecoder& params)
{
    unsigned flags = 0;
    while (const uint8_t type = params)
    {
        flags |= idToParserFeature(type);
    }
    return flags;
}


Xapian::QueryParser::stem_strategy
Driver::readStemmingStrategy(ParamDecoder& params)
{
  const uint8_t type = params;
  if (type > STEM_STRATEGY_COUNT)
    throw BadCommandDriverError(type);
  return STEM_STRATEGIES[type];
}


void 
Driver::addPrefix(Xapian::QueryParser& qp, ParamDecoder& params)
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
Driver::buildQuery(ParamDecoder& params)
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

        case QUERY_SCALE_WEIGHT: // case with a double parameter
        {
            const uint8_t op        = params;
            const double  factor    = params;
            Xapian::Query sub_query = buildQuery(params);

            Xapian::Query q(
                static_cast<Xapian::Query::op>(op), 
                sub_query, 
                factor);
            return q;
        }

        default:
            throw BadCommandDriverError(type);
    }
}


void 
Driver::fillEnquire(Xapian::Enquire& enquire, ParamDecoder& params)
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
Driver::fillEnquireOrder(Xapian::Enquire& enquire, 
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
Driver::selectParser(ParamDecoder& params)
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
Driver::readParser(ParamDecoder& params)
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
Driver::control(
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
    Driver& drv = * reinterpret_cast<Driver*>( drv_data );
    ResultEncoder& result = * drv.getResultEncoder();
    result.setBuffer(rbuf, rlen);
    result << static_cast<uint8_t>( SUCCESS );

    try
    {
        switch(command) {
        case OPEN: 
            {
            const uint8_t        mode   = params;
            const std::string&   dbpath = params;
            return drv.open(mode, dbpath);
            }

        case OPEN_PROG: 
            {
            const uint8_t        mode     = params;
            const std::string&   prog     = params;
            const std::string&   args     = params;
            const uint32_t       timeout  = params;
            return drv.open(mode, prog, args, timeout);
            }

        case OPEN_TCP: 
            {
            const uint8_t        mode     = params;
            const std::string&   host     = params;
            const uint16_t       port     = params;
            const uint32_t       timeout  = params;
            const uint32_t       ctimeout = params;
            return drv.open(mode, host, port, timeout, ctimeout);
            }

        case LAST_DOC_ID:
            return drv.getLastDocId();

        case ADD_DOCUMENT:
            return drv.addDocument(params);

        case UPDATE_DOCUMENT:

        case UPDATE_OR_CREATE_DOCUMENT:
            return drv.updateDocument(params, 
                command == UPDATE_OR_CREATE_DOCUMENT);

        case DELETE_DOCUMENT:
            drv.deleteDocument(params);
            return result;

        case REPLACE_DOCUMENT:
            return drv.replaceDocument(params);

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

        case DOCUMENT:
            return drv.document(params);

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

        case DB_INFO:
            return drv.databaseInfo(params);

        case SET_METADATA:
            drv.setMetadata(params);
            return result;

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
Driver::open(uint8_t mode, const std::string& dbpath)
{
    switch(mode) 
    {
        // Open readOnly db
        case READ_OPEN:
            m_db.add_database(Xapian::Database(dbpath));
            m_number_of_databases++;
            break;

        case WRITE_CREATE_OR_OPEN:
        case WRITE_CREATE:
        case WRITE_CREATE_OR_OVERWRITE:
        case WRITE_OPEN:
            m_wdb = Xapian::WritableDatabase(dbpath, openWriteMode(mode));
            m_db = m_wdb;
            m_number_of_databases = 1;
            break;

        default:
            throw BadCommandDriverError(mode);
    }
    
    return m_result;
}

int 
Driver::openWriteMode(uint8_t mode)
{
    switch(mode)
    {
        // create new database; fail if db exists
        case WRITE_CREATE_OR_OPEN:
            return Xapian::DB_CREATE_OR_OPEN;

        // overwrite existing db; create if none exists
        case WRITE_CREATE:
            return Xapian::DB_CREATE;

        // open for read/write; fail if no db exists
        case WRITE_CREATE_OR_OVERWRITE:
            return Xapian::DB_CREATE_OR_OVERWRITE;

        // open for read/write; fail if no db exists
        case WRITE_OPEN:
            return Xapian::DB_OPEN;

        default:
            throw BadCommandDriverError(mode);
    }
}


/**
 * Open an remote TCP database.
 *
 * http://xapian.org/docs/apidoc/html/namespaceXapian_1_1Remote.html
 */
size_t 
Driver::open(uint8_t mode, const std::string& host, uint16_t port, 
             uint32_t timeout, uint32_t connect_timeout)
{
    switch(mode) 
    {
        // Open readOnly db
        case READ_OPEN:
            m_db.add_database(
                Xapian::Remote::open(host, port, timeout, connect_timeout));
            m_number_of_databases++;
            break;

        // open for read/write; fail if no db exists
        case WRITE_OPEN:
            m_wdb = Xapian::Remote::open_writable(host, port, 
                    timeout, connect_timeout);
            m_db = m_wdb;
            m_number_of_databases = 1;
            break;

        default:
            throw BadCommandDriverError(mode);
    }
    return m_result;
}


/**
 * Open an remote program database.
 *
 * http://xapian.org/docs/apidoc/html/namespaceXapian_1_1Remote.html
 */
size_t 
Driver::open(uint8_t mode, const std::string& prog, const std::string& args, 
             uint32_t timeout)
{
    switch(mode) {
        // Open readOnly db
        case READ_OPEN:
            m_db.add_database(Xapian::Remote::open(prog, args, timeout));
            m_number_of_databases++;
            break;

        // open for read/write; fail if no db exists
        case WRITE_OPEN:
            m_wdb = Xapian::Remote::open_writable(prog, args, timeout);
            m_db = m_wdb;
            m_number_of_databases = 1;
            break;

        default:
            throw BadCommandDriverError(mode);
    }
    return m_result;
}

void 
Driver::applyDocument(
    ParamDecoder& params, 
    Xapian::Document& doc)
{
    Xapian::TermGenerator   termGenerator;
    termGenerator.set_document(doc);
    termGenerator.set_stemmer(m_default_stemmer);

    while (const uint8_t command = params)
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

            case SET_TERM:
            case ADD_TERM:
            case UPDATE_TERM:
            case REMOVE_TERM:
                handleTerm(command, params, doc);
                break;

            case ADD_VALUE:
            case SET_VALUE:
            case UPDATE_VALUE:
            case REMOVE_VALUE:
                handleValue(command, params, doc);
                break;

            case SET_POSTING:
            case ADD_POSTING:
            case UPDATE_POSTING:
            case REMOVE_POSTING:
                handlePosting(command, params, doc);
                break;

            case SET_WDF:
            case DEC_WDF:
            // see append_decrease_wdf
            // see append_set_wdf
            {
                const std::string&           tname   = params; // value
                const uint32_t               wdf     = params;
                const bool                   ignore  = params; 

                const Xapian::termcount wdf2 =
                    static_cast<Xapian::termcount>(wdf);
                
                if (command == SET_WDF)
                    trySetWDF(doc, tname, wdf2, ignore);
                else
                    tryDecreaseWDF(doc, tname, wdf2, ignore);
                break;
            }

            case REMOVE_VALUES:
                doc.clear_values();
                break;

            case REMOVE_TERMS:
                doc.clear_terms();
                break;

            case REMOVE_POSITIONS:
                clearTermPositions(doc);
                break;

            case REMOVE_TERM_POSITIONS:
            {
                const std::string&     tname   = params; // value
                const bool             ignore  = params; 
                tryClearTermPositions(doc, tname, ignore);
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}


void
Driver::handleTerm(uint8_t command,
    ParamDecoder& params, 
    Xapian::Document& doc)
{
    // see xapian_document:append_term
    const std::string&          tname    = params; // value
    const uint32_t              wdf      = params;
    const bool                  ignore   = params; 
    // Pos = undefined

    const Xapian::termcount wdf_inc =
        static_cast<Xapian::termcount>(wdf);

    bool is_error = false;

    switch (command)
    {
        case REMOVE_TERM:
            if ((!wdf_inc) || (wdf_inc == getTermFrequency(doc, tname)))
            {
                tryRemoveTerm(doc, tname, ignore);
                return;
            }
            else
                is_error = true;

        case ADD_TERM:
            if (isTermExist(doc, tname))
                is_error = true;
            break;
                
        case UPDATE_TERM:
            if (!isTermExist(doc, tname))
                is_error = true;
    }

    if (is_error)
    {
        if (ignore) return;
        else        throw BadArgumentDriverError();
    }

    doc.add_term(tname, wdf_inc);
}


const std::string 
Driver::decodeValue(ParamDecoder& params)
{
    switch(uint8_t type = params)
    {
        case STRING_TYPE:
            return params;

        case DOUBLE_TYPE:
            return Xapian::sortable_serialise(params);

        default:
            throw BadCommandDriverError(type);
     }
}

void
Driver::handleValue(uint8_t command,
    ParamDecoder& params, 
    Xapian::Document& doc)
{
    // see xapian_document:append_value
    const uint32_t         slot     = params;
    const std::string&     value    = decodeValue(params);
    const bool             ignore   = params; 

    const Xapian::valueno slot_no  = 
        static_cast<Xapian::valueno>(slot);

    bool is_error = false;

    switch (command)
    {
        case REMOVE_VALUE:
            // If value is an empty string, then remove any value in 
            // the slot.
            // Otherwise, remove only if passed and current values 
            // are equal.
            if ((value == "") || (value == doc.get_value(slot_no)))
                tryRemoveValue(doc, slot_no, ignore); 
            return;

        case ADD_VALUE:
            if (isValueExist(doc, slot_no))
                is_error = true;
            break;
                
        case UPDATE_VALUE:
            if (!isValueExist(doc, slot_no))
                is_error = true;
    }

    if (is_error)
    {
        if (ignore) return;
        else        throw BadArgumentDriverError();
    }

    doc.add_value(slot_no, value); 
}


void
Driver::handlePosting(uint8_t command,
    ParamDecoder& params, 
    Xapian::Document& doc)
{
    // see xapian_document:append_term
    const std::string&     tname   = params; // value
    const uint32_t         tpos    = params;
    const uint32_t         wdf     = params;
    const bool             ignore  = params; 

    const Xapian::termpos term_pos = 
        static_cast<Xapian::termpos>(tpos);

    const Xapian::termcount wdf2 =
        static_cast<Xapian::termcount>(wdf);

    bool is_error = false;

    switch (command)
    {
        case REMOVE_POSTING:
            tryRemovePosting(doc, tname, term_pos, wdf2, ignore);
            return;

        case ADD_POSTING:
            if (isPostingExist(doc, tname, term_pos))
                is_error = true;
            break;
                
        case UPDATE_POSTING:
            if (!isPostingExist(doc, tname, term_pos))
                is_error = true;
    }

    if (is_error)
    {
        if (ignore) return;
        else        throw BadArgumentDriverError();
    }

    doc.add_posting(tname, term_pos, wdf2);
}


// Helper for getting frequency (wdf) of the passed term.
// If term is not found, then return 0.
Xapian::termcount
Driver::getTermFrequency(
    Xapian::Document& doc, const std::string& tname)
{
    Xapian::TermIterator 
        iter = doc.termlist_begin(),
        end = doc.termlist_end();

    if (iter == end)
        return 0;
    
    iter.skip_to(tname);
    // Current element is a term and its value is tname.
    return ((iter != end) && (tname == (*iter))) 
        ? iter.get_wdf() : 0;
}


// If term is not found, then BadArgumentDriverError will be thrown.
Xapian::termcount
Driver::getExistedTermFrequency(
    Xapian::Document& doc, const std::string& tname)
{
    Xapian::TermIterator 
        iter = doc.termlist_begin(),
        end = doc.termlist_end();

    if (iter == end)
        throw BadArgumentDriverError();

    iter.skip_to(tname);

    if ((iter == end) || (tname != (*iter))) 
            throw BadArgumentDriverError();

    return iter.get_wdf();
}


void
Driver::tryRemoveValue(
    Xapian::Document& doc, Xapian::valueno slot_no, bool ignoreErrors)
{
    if (ignoreErrors)
        try {
            doc.remove_value(slot_no);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        doc.remove_value(slot_no);
}


void
Driver::tryRemoveTerm(
    Xapian::Document& doc, const std::string& tname, bool ignoreErrors)
{
    if (ignoreErrors)
        try {
            doc.remove_term(tname);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        doc.remove_term(tname);
}


void
Driver::tryRemovePosting(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termpos tpos, 
    Xapian::termcount wdf_inc,
    bool ignoreErrors)
{
    if (ignoreErrors)
        try {
            doc.remove_posting(tname, tpos, wdf_inc);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        doc.remove_posting(tname, tpos, wdf_inc);
}


/**
 * Tiny dirty class
 */
class HellTermPosition
{ 
    static const Xapian::termpos HELL_POS = 666;
    bool m_is_exist;
    Xapian::Document& m_doc;
    const std::string& m_tname;
    
    public:
    HellTermPosition(
        Xapian::Document& doc, 
        const std::string& tname)
    : m_doc(doc), m_tname(tname)
    {
        if (!Driver::isTermExist(doc, tname))
            throw BadArgumentDriverError();

        m_is_exist = Driver::isPostingExist(doc, tname, HELL_POS);
    }

    void
    dec_wdf(const Xapian::termcount wdf)
    {
        m_doc.add_posting(m_tname, HELL_POS, 0);
        m_doc.remove_posting(m_tname, HELL_POS, wdf);
    }

    void
    inc_wdf(const Xapian::termcount wdf)
    {
        m_doc.add_term(m_tname, wdf);
    }

    void
    set_wdf(const Xapian::termcount wdf)
    {
        const Xapian::termcount old_wdf = 
            Driver::getTermFrequency(m_doc, m_tname);
        if (old_wdf < wdf) // inc
            inc_wdf(wdf - old_wdf);
        else 
        if (old_wdf > wdf) // dec
            dec_wdf(old_wdf - wdf);
    }

    ~HellTermPosition()
    {
        if (m_is_exist)
            m_doc.add_posting(m_tname, HELL_POS, 0);
    }
};


void
Driver::tryDecreaseWDF(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termcount wdf, 
    bool ignoreErrors)
{
    if (ignoreErrors)
    {
        try 
        {
            // TODO: dirty :(
            HellTermPosition hpos(doc, tname);
            hpos.dec_wdf(wdf);
        } 
        catch (Xapian::InvalidArgumentError& e) {}
        catch (BadCommandDriverError& e) {}
    }
    else
    {
        // TODO: dirty :(
        HellTermPosition hpos(doc, tname);
        hpos.dec_wdf(wdf);
    }
}


void
Driver::trySetWDF(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termcount wdf, 
    bool ignoreErrors)
{
    if (ignoreErrors)
    {
        try 
        {
            // TODO: dirty :(
            HellTermPosition hpos(doc, tname);
            hpos.set_wdf(wdf);
        } 
        catch (Xapian::InvalidArgumentError& e) {}
        catch (BadCommandDriverError& e) {}
    }
    else
    {
        // TODO: dirty :(
        HellTermPosition hpos(doc, tname);
        hpos.set_wdf(wdf);
    }
}


void
Driver::tryClearTermPositions(
    Xapian::Document& doc, 
    const std::string& tname, 
    bool ignoreErrors)
{
    // TODO: dirty :(
    if (ignoreErrors)
        try {
            clearTermPositions(doc, tname);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        clearTermPositions(doc, tname);
}


void
Driver::clearTermPositions(
    Xapian::Document& doc, 
    const std::string& tname)
{
    // TODO: dirty :(
    Xapian::termcount old_wdf = getExistedTermFrequency(doc, tname);
    doc.remove_term(tname);
    doc.add_term(tname, old_wdf);
}


void
Driver::clearTermPositions(
    Xapian::Document& doc)
{
    // TODO: dirty :(
    typedef std::map<std::string, Xapian::termcount> map_type;
    map_type map;

    for (Xapian::TermIterator i = doc.termlist_begin(), 
            e = doc.termlist_end(); i != e; i++)
    {
        map[*i] = i.get_wdf();
    }
    doc.clear_terms();
    for (map_type::iterator i = map.begin(); i != map.end(); i++)
    {
        doc.add_term(i->first, i->second);
    }
}


bool
Driver::isValueExist(Xapian::Document& doc, Xapian::valueno slot_no)
{
    Xapian::ValueIterator    
        iter = doc.values_begin(),
        end = doc.values_end();

    if (iter == end)
        return 0;

    iter.skip_to(slot_no);
    return (iter != end) && (iter.get_valueno() == slot_no);
}


bool
Driver::isTermExist(Xapian::Document& doc, const std::string& tname)
{
    Xapian::TermIterator    
        iter = doc.termlist_begin(),
        end = doc.termlist_end();

    if (iter == end)
        return false;

    iter.skip_to(tname);
    return (iter != end) && ((*iter) == tname);
}


bool
Driver::isPostingExist(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termpos term_pos)
{
    Xapian::TermIterator    
        titer = doc.termlist_begin(),
        tend = doc.termlist_end();

    if (titer == tend)
        return false;

    titer.skip_to(tname);

    if ((titer != tend) && ((*titer) == tname))
    {
        // term exist
        Xapian::PositionIterator    
            piter = titer.positionlist_begin(), 
            pend = titer.positionlist_end();

        if (piter == pend)
            return false;

        piter.skip_to(term_pos);
        return (piter != pend) && ((*piter) == term_pos);
    }
    return false;
}

void 
Driver::retrieveDocument(
    ParamDecoder params,  /* yes, it is a copy */
    Xapian::Document& doc)
{
    const uint8_t decoder_type = params;
    if (decoder_type != DEC_DOCUMENT)
        throw BadArgumentDriverError();

    while (const uint8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case GET_VALUE:
            {
                const uint32_t     slot  = params;
                const uint8_t      type  = STRING_TYPE;
                const std::string& value = 
                    doc.get_value(static_cast<Xapian::valueno>(slot));
                m_result << type << value;
                break;
            }

            case GET_FLOAT_VALUE:
            {
                const uint32_t     slot  = params;
                const uint8_t      type  = DOUBLE_TYPE;
                const double       value = 
                    Xapian::sortable_unserialise(
                        doc.get_value(static_cast<Xapian::valueno>(slot)));
                m_result << type << value;
                break;
            }

            case GET_DATA:
            {
                const std::string& data = doc.get_data();
                m_result << data;
                break;
            }

            case GET_DOCID:
            {
                const Xapian::docid docid = doc.get_docid();
                m_result << static_cast<uint32_t>(docid);
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}

void 
Driver::retrieveDocument(
    ParamDecoder params,  /* yes, it is a copy */
    Xapian::MSetIterator& mset_iter)
{
    const uint8_t decoder_type = params;
    if (decoder_type != DEC_ITERATOR)
        throw BadArgumentDriverError();

    while (const uint8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case GET_WEIGHT:
            {
                const Xapian::weight    w = mset_iter.get_weight();
                m_result << static_cast<double>(w);
                break;
            }

            case GET_RANK:
            {
                const Xapian::doccount    r = mset_iter.get_rank();
                m_result << static_cast<uint32_t>(r);
                break;
            }

            case GET_PERCENT:
            {
                const Xapian::percent    p = mset_iter.get_percent();
                m_result << static_cast<uint8_t>(p);
                break;
            }

            // http://trac.xapian.org/wiki/FAQ/MultiDatabaseDocumentID
            case GET_DOCID:
            {
                m_result << static_cast<uint32_t>(docid_sub(*mset_iter));
                break;
            }

            case GET_MULTI_DOCID:
            {
                m_result << static_cast<uint32_t>(*mset_iter);
                break;
            }


            case GET_DB_NUMBER:
            {
                m_result << static_cast<uint32_t>(subdb_num(*mset_iter));
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}


void 
Driver::retrieveDocument(
    ParamDecoder params,  /* yes, it is a copy */
    Xapian::Document& doc,
    Xapian::MSetIterator& mset_iter)
{
    const uint8_t decoder_type = params;
    if (decoder_type != DEC_BOTH)
        throw BadArgumentDriverError();

    //Xapian::docid did = *m;
    while (const uint8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case GET_VALUE:
            {
                const uint32_t     slot  = params;
                const uint8_t      type  = STRING_TYPE;
                const std::string& value = 
                    doc.get_value(static_cast<Xapian::valueno>(slot));
                m_result << type << value;
                break;
            }

            case GET_FLOAT_VALUE:
            {
                const uint32_t     slot  = params;
                const uint8_t      type  = DOUBLE_TYPE;
                const double       value = 
                    Xapian::sortable_unserialise(
                        doc.get_value(static_cast<Xapian::valueno>(slot)));
                m_result << type << value;
                break;
            }

            case GET_DATA:
            {
                const std::string& data = doc.get_data();
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
                const Xapian::weight    w = mset_iter.get_weight();
                m_result << static_cast<double>(w);
                break;
            }

            case GET_RANK:
            {
                const Xapian::doccount    r = mset_iter.get_rank();
                m_result << static_cast<uint32_t>(r);
                break;
            }

            case GET_PERCENT:
            {
                const Xapian::percent    p = mset_iter.get_percent();
                m_result << static_cast<uint8_t>(p);
                break;
            }

            // http://trac.xapian.org/wiki/FAQ/MultiDatabaseDocumentID
            case GET_MULTI_DOCID:
            {
                m_result << static_cast<uint32_t>(*mset_iter);
                break;
            }

            case GET_DB_NUMBER:
            {
                m_result << static_cast<uint32_t>(subdb_num(*mset_iter));
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}


void 
Driver::retrieveTerm(
    ParamDecoder params,  /* yes, it is a copy */
    const Xapian::TermIterator& iter)
{
    retrieveTerm(params, m_result, iter);
}


void 
Driver::retrieveTerm(
    ParamDecoder params,  /* yes, it is a copy */
    ResultEncoder& result,
    const Xapian::TermIterator& iter)
{
    while (const uint8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case TERM_VALUE:
            {
                const std::string& value = *iter;
                result << value;
                break;
            }

            case TERM_WDF:
            {
                result << static_cast<uint32_t>(iter.get_wdf());
                break;
            }

            case TERM_FREQ:
            {
                result << static_cast<uint32_t>(iter.get_termfreq());
                break;
            }

            case TERM_POS_COUNT:
            {
                result << static_cast<uint32_t>(iter.positionlist_count());
                break;
            }

            case TERM_POSITIONS:
            {
                Xapian::termcount count = iter.positionlist_count();
                result << static_cast<uint32_t>(count);
                if (count > 0)
                    for (Xapian::PositionIterator 
                            piter = iter.positionlist_begin(),
                            pend = iter.positionlist_end();
                        piter != pend;
                        piter++)
                        result << static_cast<uint32_t>(*piter);
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}

ParamDecoderController
Driver::retrieveTermSchema(
    ParamDecoder& params) const
{
    const char* from = params.currentPosition();

    while (const uint8_t command = params)
    /* Do, while command != stop != 0 */
    {}

    const char* to = params.currentPosition();

    size_t len = to - from;
    ParamDecoderController ctrl(from, len);
    return ctrl;
}




ParamDecoderController
Driver::retrieveDocumentSchema(
    ParamDecoder& params) const
{
    const char* from = params.currentPosition();
    uint8_t decoder_type = params;
    (void) decoder_type;

    while (const uint8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case GET_FLOAT_VALUE:
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
            case GET_MULTI_DOCID:
            case GET_DB_NUMBER:
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


ParamDecoderController
Driver::applyDocumentSchema(
    ParamDecoder& params) const
{
    const char* from = params.currentPosition();

    while (const uint8_t command = params)
    /* Do, while command != stop != 0 */
    {
        switch (command)
        {
            case STEMMER:
            {
                const Xapian::Stem&  stemmer = params;
                (void) stemmer;
                break;
            }

            case DATA:
            {
                const std::string&   data = params;
                (void) data;
                break;
            }

            case DELTA:
            {
                const uint32_t   delta = params;
                (void) delta;
                break;
            }

            case TEXT:
            {
                const std::string&     text    = params; // value
                const uint32_t         wdf_inc = params; // pos
                const std::string&     prefix  = params;
                (void) text;
                (void) wdf_inc;
                (void) prefix;
                break;
            }

            case SET_TERM:
            case ADD_TERM:
            case UPDATE_TERM:
            case REMOVE_TERM:
            {
                const std::string&           tname   = params; // value
                const Xapian::termcount      wdf_inc = params; 
                const bool                   ignore  = params;
                (void) tname;
                (void) wdf_inc;
                (void) ignore;
                break;
            }

            case ADD_VALUE:
            case SET_VALUE:
            case UPDATE_VALUE:
            case REMOVE_VALUE:
            {
                const uint32_t         slot    = params;
                const std::string&     value   = params;
                const bool             ignore  = params;
                (void) slot;
                (void) value;
                (void) ignore;
                break;
            }

            case SET_POSTING:
            case ADD_POSTING:
            case UPDATE_POSTING:
            case REMOVE_POSTING:
            {
                const std::string&     tname   = params; // value
                const uint32_t         tpos    = params;
                const uint32_t         wdf_inc = params;
                const bool             ignore  = params;
                (void) tname;
                (void) tpos;
                (void) wdf_inc;
                (void) ignore;
                break;
            }

            // work with WDF
            case DEC_WDF:
            case SET_WDF:
            {
                const std::string&           tname   = params; // value
                const Xapian::termcount      wdf     = params; 
                const bool                   ignore  = params;
                (void) tname;
                (void) wdf;
                (void) ignore;
                break;
            }

            case REMOVE_VALUES:
            case REMOVE_TERMS:
            case REMOVE_POSITIONS:
                break;

            case REMOVE_TERM_POSITIONS:
            {
                const std::string&     tname   = params; // value
                const bool             ignore  = params;
                (void) tname;
                (void) ignore;
                break;
            }

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
Driver::getResourceInfo()
{
    ObjectRegister<UserResource>& 
    reg = m_generator.getRegister();
    ObjectRegister<UserResource>::Hash&
    elements = reg.getElements();

    ObjectRegister<UserResource>::Hash::iterator i, e, b;
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
Driver::createResource(ParamDecoder& params)
{
    uint32_t resource_num = m_stores.createAndRegister(params);
    m_result << resource_num;
    return m_result;
}


size_t 
Driver::msetInfo(ParamDecoder& params)
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
            const std::string& tname = params;
            m_result << static_cast<double>(mset.get_termweight(tname));
            break;
        }

        case MI_TERM_FREQ:
        {
            const std::string& tname = params;
            m_result << static_cast<uint32_t>(mset.get_termfreq(tname));
            break;
        }

        default:
            throw BadCommandDriverError(command);
    }

    return m_result;
}


size_t 
Driver::databaseInfo(ParamDecoder& params)
{
    while (uint8_t command = params)
    switch(command)
    {
        case DBI_HAS_POSITIONS:
            m_result << static_cast<uint8_t>(m_db.has_positions());
            break;

        case DBI_DOCCOUNT:
            m_result << static_cast<uint32_t>(m_db.get_doccount());
            break;

        case DBI_LASTDOCID:
            m_result << static_cast<uint32_t>(m_db.get_lastdocid());
            break;

        case DBI_AVLENGTH:
            m_result << static_cast<double>(m_db.get_avlength());
            break;


        case DBI_TERM_EXISTS:
        {
            const std::string& tname = params;
            m_result << static_cast<uint8_t>(m_db.term_exists(tname));
            break;
        }

        case DBI_TERM_FREQ:
        {
            const std::string& tname = params;
            m_result << static_cast<uint32_t>(m_db.get_termfreq(tname));
            break;
        }

        case DBI_COLLECTION_FREQ:
        {
            const std::string& tname = params;
            m_result << static_cast<uint32_t>(m_db.get_collection_freq(tname));
            break;
        }

        case DBI_VALUE_FREQ:
        {
            const Xapian::valueno slot = params;
            m_result << static_cast<uint32_t>(m_db.get_value_freq(slot));
            break;
        }

        case DBI_VALUE_LOWER_BOUND:
        {
            const Xapian::valueno slot = params;
            m_result << m_db.get_value_lower_bound(slot);
            break;
        }

        case DBI_VALUE_UPPER_BOUND:
        {
            const Xapian::valueno slot = params;
            m_result << m_db.get_value_upper_bound(slot);
            break;
        }

        case DBI_DOCLENGTH_LOWER_BOUND:
            m_result << m_db.get_doclength_lower_bound();
            break;

        case DBI_DOCLENGTH_UPPER_BOUND:
            m_result << m_db.get_doclength_upper_bound();
            break;

        case DBI_WDF_UPPER_BOUND:
        {
            const std::string& tname = params;
            m_result << static_cast<uint32_t>(m_db.get_wdf_upper_bound(tname));
            break;
        }

        case DBI_DOCLENGTH:
        {
            const Xapian::docid docid = params;
            m_result << static_cast<uint32_t>(m_db.get_doclength(docid));
            break;
        }

        case DBI_UUID:
            m_result << m_db.get_uuid();
            break;

        case DBI_METADATA:
        {
            const std::string& key = params;
            m_result << m_db.get_metadata(key);
            break;
        }

// TODO: synonym, spellcorrection

        default:
            throw BadCommandDriverError(command);
    }

    return m_result;
}


void 
Driver::setMetadata(ParamDecoder& params)
{
    assertWriteable();

    const std::string& key = params;
    const std::string& value = params;
    m_wdb.set_metadata(key, value);
}


/**
 * Allow to find and write terms by name.
 *
 * Helper for TermQlcTable class.
 * Use set order of elements.
 *
 * @param driver_params Contains which keys (term names) to find. Ends with "".
 * @param schema_params Contains which fields to write. 
 * @param result        A buffer for writing.
 * @param iter          First term for searching in.
 * @param end           Last term for searching in.
 */
void
Driver::qlcTermIteratorLookup(
    ParamDecoder& driver_params, 
    const ParamDecoder& schema_params, 
    ResultEncoder& result,
    Xapian::TermIterator iter,
    const Xapian::TermIterator end)
{
    // Flags, that signal about end of list.
    const uint8_t more = 1, stop = 0;
    std::set<std::string> terms;

    while(true)
    {
        const std::string& term = driver_params;
        // first term is not empty
        assert(!terms.empty() || !term.empty());
        if (term.empty()) break;
        terms.insert(term);
        assert(!terms.empty());
    }

    assert(!terms.empty());
    // TODO: it can be an exception
    if (terms.empty())
        return;

    // Special case when we want to lookup only 1 element
    if (terms.size() == 1)
    {
        std::string term = *(terms.begin());
        iter.skip_to(term);
        if ((iter != end) && (*iter == term))
        {
            // Put a flag
            result << more;

            ParamDecoder params = schema_params;
            retrieveTerm(params, result, iter);
        }
        result << stop;
        return;    
    }

    for (; iter != end; iter++)
    {
        if (terms.find(*iter) != terms.end())
        {
            // Put a flag
            result << more;

            // Clone params
            ParamDecoder params = schema_params;
            retrieveTerm(params, result, iter);
        }
    };
    result << stop;
}

XAPIAN_ERLANG_NS_END
