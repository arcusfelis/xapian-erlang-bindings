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
#include "extension/value_count_mspy.h"

#include <assert.h>
#include <cstdlib>

// -------------------------------------------------------------------
// Main Driver Class
// -------------------------------------------------------------------


#include "xapian_core.h"

XAPIAN_ERLANG_NS_BEGIN

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


Driver::Driver(MemoryManager& mm)
: m_number_of_databases(0), m_mm(mm)
{
    m_default_parser_factory.set_database(m_db);
    m_standard_parser_factory.set_database(m_db);
    m_default_parser_factory.set_database(m_db);
    m_standard_parser_factory.set_database(m_db);
}


Driver::~Driver()
{}


void 
Driver::setDefaultStemmer(const Xapian::Stem& stemmer)
{
    m_default_stemmer = stemmer;
    m_default_parser.set_stemmer(m_default_stemmer);
    m_default_parser_factory.set_stemmer(m_default_stemmer);
}


void
Driver::setDefaultStemmer(ParamDecoder& params)
{
    const Xapian::Stem&  stemmer = params;
    setDefaultStemmer(Xapian::Stem(stemmer));
}


void
Driver::setDefaultPrefixes(ParamDecoder& params)
{
    const uint32_t count   = params;
    for (uint32_t i = 0; i < count; i++)
    {
        addPrefix(params, m_default_parser_factory);
    }
    // Update the mirror
    m_default_parser = m_default_parser_factory;
}


void
Driver::getLastDocId(ResultEncoder& result)
{
    //Xapian::docid get_lastdocid() const
    const Xapian::docid 
    docid = m_db.get_lastdocid();
    result << static_cast<uint32_t>(docid);
}


void
Driver::addDocument(PR)
{
    assertWriteable();

    Xapian::Document doc;
    applyDocument(params, doc);
    const Xapian::docid
    docid = m_wdb.add_document(doc);
    result << static_cast<uint32_t>(docid);
}


// REP_CRT_DOC_MARK
void
Driver::replaceOrCreateDocument(PR)
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
        
    result << static_cast<uint32_t>(docid);
}


// REP_DOC_MARK
void
Driver::replaceDocument(PR)
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
            try {
                m_wdb.get_document(docid);
                m_wdb.replace_document(docid, doc);
            } catch (Xapian::DocNotFoundError e) {
                docid = 0;
            }
            break;
        }

        case UNIQUE_TERM:
        {
            const std::string& unique_term = params;
            if (m_wdb.term_exists(unique_term))
                docid = m_wdb.replace_document(unique_term, doc);
            else
                docid = 0;
            break;
        }

        default:
            throw BadCommandDriverError(idType);
    }
        
    result << static_cast<uint32_t>(docid);
}


void
Driver::updateDocument(PR, bool create)
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
                // new document was not added.
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
        
    result << static_cast<uint32_t>(docid);
}


void 
Driver::deleteDocument(PR)
{
    assertWriteable();
    uint8_t is_exist;

    switch(uint8_t idType = params)
    {
        case UNIQUE_DOCID:
        {
            const Xapian::docid
            docid = params;
            try
            { 
                m_wdb.delete_document(docid);
                is_exist = true;
            } catch (Xapian::DocNotFoundError e) 
            { 
                is_exist = false;
            }
            break;
        }

        case UNIQUE_TERM:
        {
            const std::string& unique_term = params;
            is_exist = m_db.term_exists(unique_term);
            if (is_exist)
                m_wdb.delete_document(unique_term);
            break;
        }

        default:
            throw BadCommandDriverError(idType);
    }
    result << is_exist;
}


void 
Driver::isDocumentExist(PR)
{
    uint8_t is_exist;
    switch(uint8_t idType = params)
    {
        case UNIQUE_DOCID:
        {
            const Xapian::docid docid = params;
            try
            { 
                m_db.get_document(docid);
                is_exist = true;
            } catch (Xapian::DocNotFoundError e) 
            {
                is_exist = false;
            }
            break;
        }

        case UNIQUE_TERM:
        {
            // If there is the term in the database, then there is a document
            // with this term.
            const std::string& unique_term = params;
            is_exist = m_db.term_exists(unique_term);
            break;
        }

        default:
            throw BadCommandDriverError(idType);
    }
    result << is_exist;
}


void
Driver::query(CPR)
{
    /* offset, pagesize, query, template */
    const uint32_t offset   = params;
    const uint32_t pagesize = params;

    // Use an Enquire object on the database to run the query.
    Xapian::Enquire enquire(m_db);
    Xapian::Query   query = buildQuery(con, params);
    enquire.set_query(query);
     
    // Get an result
    Xapian::MSet mset = enquire.get_mset(
        static_cast<Xapian::termcount>(offset), 
        static_cast<Xapian::termcount>(pagesize));

    Xapian::doccount count = mset.size();
    result << static_cast<uint32_t>(count);
    retrieveDocuments(params, result, mset.begin(), mset.end());
}

void
Driver::retrieveDocuments(PCR, 
    Xapian::MSetIterator iter, Xapian::MSetIterator end)
{
    ParamDecoder params_copy = params;
    switch (uint8_t decoder_type = params_copy)
    {
        case DEC_DOCUMENT:
            for (; iter != end; ++iter)
            {
                Xapian::Document doc = iter.get_document();
                retrieveDocument(params, result, doc);
            }
            break;

        case DEC_ITERATOR:
            for (; iter != end; ++iter)
                retrieveDocument(params, result, iter);
            break;

        case DEC_BOTH:
            for (; iter != end; ++iter)
            {
                Xapian::Document doc = iter.get_document();
                retrieveDocument(params, result, doc, iter);
            }
            break;

        default:
            throw BadCommandDriverError(decoder_type);
    }
}


/**
 * Read sources of information and write information fields.
 * Sources are selected from Erlang code.
 */
void
Driver::selectEncoderAndRetrieveDocument(PR, Xapian::MSetIterator& iter)
{
    ParamDecoder params_copy = params;
    switch (uint8_t decoder_type = params_copy)
    {
        // Source is a document.
        // Fields from the iterator are not used.
        case DEC_DOCUMENT:
        {
            Xapian::Document doc = iter.get_document();
            retrieveDocument(params, result, doc);
            break;
        }

        // Source is an iterator.
        // Fields from the document are not used.
        case DEC_ITERATOR:
            retrieveDocument(params, result, iter);
            break;

        // Fields both from the iterator and from the document are used.
        case DEC_BOTH:
        {
            Xapian::Document doc = iter.get_document();
            retrieveDocument(params, result, doc, iter);
            break;
        }

        default:
            throw BadCommandDriverError(decoder_type);
    }
}


void
Driver::enquire(PR)
{
    // Use an Enquire object on the database to run the query.
    // Create a new context.
    Resource::Element elem = 
        Resource::Element::wrap(new Xapian::Enquire(m_db));
    Xapian::Enquire& enquire = elem;

    // Use elem as a context.
    fillEnquire(elem, params, enquire);

    m_store.save(elem, result);
}



/**
 * Create a parser with a corrected string inside.
 */
void
Driver::createQueryParser(PR)
{
    // Create a new context.
    Resource::Element parser_con = Resource::Element::createContext();

    // Read parser into allocated pointer.
    // parser_con holds child resources.
    Xapian::QueryParser* p_parser = 
        new Xapian::QueryParser(readParser(parser_con, params));

    Resource::Element elem = 
        Resource::Element::wrap(p_parser);

    // Add the context as a child.
    elem.attach(parser_con);
    m_store.save(elem, result);
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
void
Driver::document(PR)
{
    const Xapian::Document& doc = getDocument(params);

    Resource::Element elem = 
        Resource::Element::wrap(new Xapian::Document(doc));
    Xapian::Enquire& enquire = elem;

    m_store.save(elem, result);
}

void 
Driver::releaseResource(ParamDecoder& params)
{
    m_store.release(params);
}


void 
Driver::matchSet(CPR)
{
    // Spies and MatchSpy must be sepated.
    // Enquire and Spies will be stored inside a temporary context (con
    // parameter).
    Xapian::Enquire& enquire = extractEnquire(con, params);

    Xapian::doccount    first, maxitems, checkatleast;
    first = params;
    uint8_t is_undefined = params;
    maxitems = is_undefined 
        ? m_db.get_doccount() 
        : params;
    checkatleast = params;

    /* Read a count of passed Spy objects. */
    uint32_t count = params;
    while (count--)
    {
        // It can be added just once
        Xapian::MatchSpy& spy = extractWritableSpy(con, params);
        enquire.add_matchspy(&spy);
    }

    Xapian::MSet mset = enquire.get_mset(
        first, 
        maxitems,
        checkatleast);

    enquire.clear_matchspies();

    Resource::Element elem = 
        Resource::Element::wrap(new Xapian::MSet(mset));

    m_store.save(elem, result);
}

Xapian::MatchSpy&
Driver::extractWritableSpy(CP)
{
    Resource::Element elem = m_store.extract(con, params);
    if (elem.is_finalized())
        throw MatchSpyFinalizedDriverError();
    elem.finalize();
    return elem;
}

void
Driver::qlcInit(PR)
{
    switch (uint8_t qlc_type = params)
    {
        case QlcType::MSET:
        {
            // Cannot use extractMSet(), because we need Element for linking.
            Resource::Element mset_elem = m_store.extract(params);
            Xapian::MSet& mset = mset_elem;
            // Extract a schema (a list of fields, settings for QLC).
            const ParamDecoderController& schema  
                = retrieveDocumentSchema(params);
            // Allocate the object
            MSetQlcTable* qlcTable = new MSetQlcTable(*this, mset, schema);

            Resource::Element elem = 
                Resource::Element::wrap(qlcTable);

            // Add MSet as a child of the QLC Table.
            elem.attach(mset_elem);

            // Write a resource.
            m_store.save(elem, result);
        
            // Write the size.
            const uint32_t mset_size = qlcTable->size();
            result << mset_size;
            break;
        }

        case QlcType::TERMS:
        {
            // Don't store copy of the document controller.
            // The whole document will be copied into QlcTable.
            Xapian::Document& doc = m_store.extract(params);
            TermIteratorGenerator* p_gen = TermIteratorGenerator::create(doc);

            const ParamDecoderController& schema  
                = retrieveTermSchema(params);

            TermQlcTable* qlcTable = new TermQlcTable(*this, p_gen, schema);

            Resource::Element elem = 
                Resource::Element::wrap(qlcTable);

            // Write a resource.
            m_store.save(elem, result);
        
            // Write the size.
            const uint32_t mset_size = qlcTable->size();
            result << mset_size;
            break;
        }

        case QlcType::SPY_TERMS:
        {
            Resource::Element spy_elem = m_store.extract(params);
            Xapian::ValueCountMatchSpy& spy = spy_elem;
            TermIteratorGenerator* p_gen = 
                TermIteratorGenerator::create(params, spy);

            const ParamDecoderController& schema  
                = retrieveTermSchema(params);

            TermQlcTable* qlcTable = new TermQlcTable(*this, p_gen, schema);

            Resource::Element elem = 
                Resource::Element::wrap(qlcTable);

            spy_elem.attach(elem);

            // Write a resource.
            m_store.save(elem, result);
        
            // Write the size.
            const uint32_t mset_size = qlcTable->size();
            result << mset_size;
            break;
        }

        default:
            throw BadCommandDriverError(qlc_type);
    }
}


void
Driver::qlcNext(PR)
{
    QlcTable& qlc_table = m_store.extract(params);
    uint32_t   from     = params;
    uint32_t   count    = params;
 
    qlc_table.getPage(result, from, count);
}

void
Driver::qlcLookup(PR)
{
    QlcTable& qlc_table = m_store.extract(params);
    qlc_table.lookup(params, result);
}


void 
Driver::assertWriteable() const
{}


void
Driver::startTransaction()
{
    assertWriteable();

    m_wdb.begin_transaction();
}


void
Driver::cancelTransaction()
{
    assertWriteable();

    m_wdb.cancel_transaction();
}


void
Driver::commitTransaction()
{
    assertWriteable();

    m_wdb.commit_transaction();
}


void
Driver::getDocumentById(PR)
{
    const Xapian::docid docid = params;
    Xapian::Document doc = m_db.get_document(docid);
    retrieveDocument(params, result, doc);
}


/**
 * Get document metadata without putting it in DB.
 */
void
Driver::documentInfo(PR)
{
    Xapian::Document doc;
    applyDocument(params, doc);
    retrieveDocument(params, result, doc);
}


/**
 * Return the document resource without putting it in DB.
 */
void
Driver::documentInfoResource(PR)
{
    Xapian::Document* doc = new Xapian::Document();
    applyDocument(params, *doc);

    Resource::Element elem = Resource::Element::wrap(doc);
    m_store.save(elem, result);
}


void
Driver::test(PR)
{
    const int8_t num = params;
    switch (num)
    {
        case TEST_RESULT_ENCODER:
        {
            const Xapian::docid from = params;
            const Xapian::docid to = params;

            testResultEncoder(result, from, to);
            break;
        }

        case TEST_EXCEPTION:
            testException();
            break;

        case TEST_ECHO:
            testEcho(params, result);
            break;

        case TEST_MEMORY:
            testMemory();
            break;

        default:
            throw BadCommandDriverError(num);
    }
}


void 
Driver::testResultEncoder(ResultEncoder& result, 
    Xapian::docid from, Xapian::docid to)
{
    for (; from <= to; from++)
        result << static_cast<uint32_t>(from);
}


void Driver::testEcho(PR)
{
    for (uint32_t len = params; len; len--)
    {
        uint8_t value = params;
        result << value;
    }
}


void
Driver::testException()
{
    throw MemoryAllocationDriverError(1000);
}


void
Driver::testMemory()
{
    void* cblock = malloc(100);
    free(cblock);
    
    void* block = m_mm.alloc(100);
    m_mm.free(block);
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
Driver::addPrefix(ParamDecoder& params, Xapian::QueryParser& qp)
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


void 
Driver::addPrefix(ParamDecoder& params, QueryParserFactory& qpf)
{
    const std::string&      field          = params;
    const std::string&      prefix         = params;
    const bool              is_boolean     = params;
    const bool              is_exclusive   = params;

    if (is_boolean)
        qpf.add_boolean_prefix(field, prefix, is_exclusive);
    else
        qpf.add_prefix(field, prefix);
}


Xapian::Query 
Driver::buildQuery(CP)
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
                subQueries.push_back(buildQuery(con, params));

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
            const std::string&      value    = decodeValue(params);
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
            const std::string&      from     = decodeValue(params);
            const std::string&      to       = decodeValue(params);
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
            Xapian::QueryParser parser = readParser(con, params);
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
            Xapian::Query sub_query = buildQuery(con, params);

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
Driver::fillEnquire(CP, Xapian::Enquire& enquire)
{
    Xapian::termcount   qlen = 0;

    while (uint8_t command = params)
    switch (command)
    {
    case EC_QUERY:
        {
        Xapian::Query   query = buildQuery(con, params);
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
        fillEnquireOrder(con, params, enquire);
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
        const Xapian::Weight& weight = extractWeight(con, params);
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
Driver::fillEnquireOrder(CP, Xapian::Enquire& enquire)
{
    uint8_t type   = params;
    bool reverse   = params;

    switch(type)
    {
    case OT_KEY:
        {
        Xapian::KeyMaker& sorter = extractKeyMaker(con, params);
        enquire.set_sort_by_key(&sorter, reverse);
        break;
        }

    case OT_KEY_RELEVANCE:
        {
        Xapian::KeyMaker& sorter = extractKeyMaker(con, params);
        enquire.set_sort_by_key_then_relevance(&sorter, reverse);
        break;
        }

    case OT_RELEVANCE_KEY:
        {
        Xapian::KeyMaker& sorter = extractKeyMaker(con, params);
        enquire.set_sort_by_relevance_then_key(&sorter, reverse);
        break;
        }

    case OT_VALUE:
        {
        uint32_t value = params;
        enquire.set_sort_by_value(value, reverse);
        break;
        }

    case OT_RELEVANCE_VALUE:
        {
        uint32_t value = params;
        enquire.set_sort_by_relevance_then_value(value, reverse);
        break;
        }

    case OT_VALUE_RELEVANCE:
        {
        uint32_t value = params;
        enquire.set_sort_by_value_then_relevance(value, reverse);
        break;
        }

    default:
        throw BadCommandDriverError(type);
    }
}


/** 
 * Returns a cloned parser.
 */
Xapian::QueryParser 
Driver::selectParser(ParamDecoder& params)
{
    uint8_t type = params;
    switch (type)
    {
    case QP_TYPE_DEFAULT:
        return m_default_parser_factory;

    case QP_TYPE_EMPTY:
        return m_standard_parser_factory;

    default:
        throw BadCommandDriverError(type);
    }
}


Xapian::QueryParser
Driver::readParser(CP)
{
  uint8_t command = params;
  // No parameters?
  // DEFAULT_PARSER_CHECK_MARK -- mark for Erlang
  if (!command)
    return m_default_parser;
 
  // Clone parser
  Xapian::QueryParser qp = m_default_parser_factory;
  do
  {
    switch (command)
    {
    case QP_PARSER_TYPE: 
        // Clone
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
        addPrefix(params, qp);
        break;

    case QP_VALUE_RANGE_PROCESSOR:
        {
        Xapian::ValueRangeProcessor& vrp = 
            extractRangeProcessor(con, params);
        qp.add_valuerangeprocessor(&vrp);
        break;
        }

    default:
        throw BadCommandDriverError(command);
    }
  } while((command = params)); // yes, it's an assignment [-Wparentheses]
  // warning: suggest parentheses around assignment used as truth value

  return qp;
}


void
Driver::handleCommand(PR,
    const unsigned int  command)
{
    result << static_cast<uint8_t>( SUCCESS );
    Resource::Element con = Resource::Element::createContext();

    try
    {
        switch(command) {
        case OPEN: 
            {
            const uint8_t        mode   = params;
            const std::string&   dbpath = params;
            open(mode, dbpath);
            break;
            }

        case OPEN_PROG: 
            {
            const uint8_t        mode     = params;
            const std::string&   prog     = params;
            const std::string&   args     = params;
            const uint32_t       timeout  = params;
            open(mode, prog, args, timeout);
            break;
            }

        case OPEN_TCP: 
            {
            const uint8_t        mode     = params;
            const std::string&   host     = params;
            const uint16_t       port     = params;
            const uint32_t       timeout  = params;
            const uint32_t       ctimeout = params;
            open(mode, host, port, timeout, ctimeout);
            break;
            }

        case LAST_DOC_ID:
            getLastDocId(result);
            break;

        case ADD_DOCUMENT:
            addDocument(params, result);
            break;

        case UPDATE_DOCUMENT:
        case UPDATE_OR_CREATE_DOCUMENT:
            updateDocument(params, result,
                command == UPDATE_OR_CREATE_DOCUMENT);
            break;

        case IS_DOCUMENT_EXIST:
            isDocumentExist(params, result);
            break;

        case DELETE_DOCUMENT:
            deleteDocument(params, result);
            break;

        case REPLACE_DOCUMENT:
            replaceDocument(params, result);
            break;

        case REPLACE_OR_CREATE_DOCUMENT:
            replaceOrCreateDocument(params, result);
            break;

        case TEST:
            test(params, result);
            break;

        case GET_DOCUMENT_BY_ID:
            getDocumentById(params, result);
            break;

        case DOCUMENT_INFO:
            documentInfo(params, result);
            break;

        case DOCUMENT_INFO_RESOURCE:
            documentInfoResource(params, result);
            break;


        case START_TRANSACTION:
            startTransaction();
            break;

        case CANCEL_TRANSACTION:
            cancelTransaction();
            break;

        case COMMIT_TRANSACTION:
            commitTransaction();
            break;

        case QUERY_PAGE:
            query(con, params, result);
            break;

        case SET_DEFAULT_STEMMER:
            setDefaultStemmer(params);
            break;

        case SET_DEFAULT_PREFIXES:
            setDefaultPrefixes(params);
            break;

        case ENQUIRE:
            enquire(params, result);
            break;

        case DOCUMENT:
            document(params, result);
            break;

        case VALUE_MATCH_SPY_TO_SLOT:
            valueMatchSpyToSlot(params, result);
            break;

        case RELEASE_RESOURCE:
            releaseResource(params);
            break;

        case MATCH_SET:
            matchSet(con, params, result);
            break;

        case QLC_INIT:
            qlcInit(params, result);
            break;

        case QLC_NEXT_PORTION:
            qlcNext(params, result);
            break;

        case QLC_LOOKUP:
            qlcLookup(params, result);
            break;

        case GET_RESOURCE_CONSTRUCTORS:
            getResourceConstructors(result);
            break;

        case CREATE_RESOURCE:
            createResource(params, result);
            break;

        case MSET_INFO:
            msetInfo(params, result);
            break;

        case DB_INFO:
            databaseInfo(params, result);
            break;

        case MATCH_SPY_INFO:
            matchSpyInfo(params, result);
            break;

        case SET_METADATA:
            setMetadata(params);
            break;

        case CLOSE: 
            m_wdb.close();
            m_db.close();
            break;

        case CREATE_QUERY_PARSER: 
            createQueryParser(params, result);
            break;

        default:
            throw BadCommandDriverError(command);
        }
    }
    catch (DriverRuntimeError& e) 
    {
        result.reset();
        result << static_cast<uint8_t>( ERROR );
        result << e.get_type();
        result << e.what();
    }
    catch (Xapian::Error& e) 
    {
        result.reset();
        result << static_cast<uint8_t>( ERROR );
        result << e.get_type();
        result << e.get_msg();
    }
}


void 
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
void 
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
}


/**
 * Open an remote program database.
 *
 * http://xapian.org/docs/apidoc/html/namespaceXapian_1_1Remote.html
 */
void 
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
                handleTerm(params, command, doc);
                break;

            case ADD_VALUE:
            case SET_VALUE:
            case UPDATE_VALUE:
            case REMOVE_VALUE:
                handleValue(params, command, doc);
                break;

            case SET_POSTING:
            case ADD_POSTING:
            case UPDATE_POSTING:
            case REMOVE_POSTING:
                handlePosting(params, command, doc);
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
Driver::handleTerm(
    ParamDecoder& params, 
    uint8_t command,
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
Driver::handleValue(
    ParamDecoder& params, 
    uint8_t command,
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
Driver::handlePosting(
    ParamDecoder& params, 
    uint8_t command,
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
Driver::retrieveDocument(PCR,
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
                result << type << value;
                break;
            }

            case GET_FLOAT_VALUE:
            {
                const uint32_t     slot  = params;
                const uint8_t      type  = DOUBLE_TYPE;
                const double       value = 
                    Xapian::sortable_unserialise(
                        doc.get_value(static_cast<Xapian::valueno>(slot)));
                result << type << value;
                break;
            }

            case GET_DATA:
            {
                const std::string& data = doc.get_data();
                result << data;
                break;
            }

            case GET_DOCID:
            {
                const Xapian::docid docid = doc.get_docid();
                result << static_cast<uint32_t>(docid);
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}

void 
Driver::retrieveDocument(PCR,
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
                result << static_cast<double>(w);
                break;
            }

            case GET_RANK:
            {
                const Xapian::doccount    r = mset_iter.get_rank();
                result << static_cast<uint32_t>(r);
                break;
            }

            case GET_PERCENT:
            {
                const Xapian::percent    p = mset_iter.get_percent();
                result << static_cast<uint8_t>(p);
                break;
            }

            // http://trac.xapian.org/wiki/FAQ/MultiDatabaseDocumentID
            case GET_DOCID:
            {
                result << static_cast<uint32_t>(docid_sub(*mset_iter));
                break;
            }

            case GET_MULTI_DOCID:
            {
                result << static_cast<uint32_t>(*mset_iter);
                break;
            }


            case GET_DB_NUMBER:
            {
                result << static_cast<uint32_t>(subdb_num(*mset_iter));
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}


void 
Driver::retrieveDocument(PCR,
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
                result << type << value;
                break;
            }

            case GET_FLOAT_VALUE:
            {
                const uint32_t     slot  = params;
                const uint8_t      type  = DOUBLE_TYPE;
                const double       value = 
                    Xapian::sortable_unserialise(
                        doc.get_value(static_cast<Xapian::valueno>(slot)));
                result << type << value;
                break;
            }

            case GET_DATA:
            {
                const std::string& data = doc.get_data();
                result << data;
                break;
            }

            case GET_DOCID:
            {
                const Xapian::docid docid = doc.get_docid();
                result << static_cast<uint32_t>(docid);
                break;
            }

            case GET_WEIGHT:
            {
                const Xapian::weight    w = mset_iter.get_weight();
                result << static_cast<double>(w);
                break;
            }

            case GET_RANK:
            {
                const Xapian::doccount    r = mset_iter.get_rank();
                result << static_cast<uint32_t>(r);
                break;
            }

            case GET_PERCENT:
            {
                const Xapian::percent    p = mset_iter.get_percent();
                result << static_cast<uint8_t>(p);
                break;
            }

            // http://trac.xapian.org/wiki/FAQ/MultiDatabaseDocumentID
            case GET_MULTI_DOCID:
            {
                result << static_cast<uint32_t>(*mset_iter);
                break;
            }

            case GET_DB_NUMBER:
            {
                result << static_cast<uint32_t>(subdb_num(*mset_iter));
                break;
            }

            default:
                throw BadCommandDriverError(command);
        }
    }
}


/**
 * @a params is copy.
 */
void 
Driver::retrieveTerm(PCR, const Xapian::TermIterator& iter)
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

            case TERM_FLOAT_VALUE:
            {
                const std::string& value = *iter;
                const double float_value = Xapian::sortable_unserialise(value);
                result << float_value;
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
    ParamDecoderController ctrl(m_mm, from, len);
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
    ParamDecoderController ctrl(m_mm, from, len);
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
    ParamDecoderController ctrl(m_mm, from, len);
    return ctrl;
}


// -------------------------------------------------------------------
// Resource Driver Helpers
// -------------------------------------------------------------------

/**
 * This function will be called inside xapian_open:init
 */
void
Driver::getResourceConstructors(ResultEncoder& result)
{
    m_store.getResourceConstructors(result);
}


void 
Driver::createResource(PR)
{
    Resource::Element elem = m_store.extract(params);
    m_store.save(elem, result);
}


void 
Driver::msetInfo(PR)
{
    Xapian::MSet& mset = m_store.extract(params);

    while (uint8_t command = params)
    switch (command)
    {
        case MI_MATCHES_LOWER_BOUND:
            result << static_cast<uint32_t>(mset.get_matches_lower_bound());
            break;

        case MI_MATCHES_ESTIMATED:
            result << static_cast<uint32_t>(mset.get_matches_estimated());
            break;

        case MI_MATCHES_UPPER_BOUND:
            result << static_cast<uint32_t>(mset.get_matches_upper_bound());
            break;

        case MI_UNCOLLAPSED_MATCHES_LOWER_BOUND:
            result << static_cast<uint32_t>(
                mset.get_uncollapsed_matches_lower_bound());
            break;

        case MI_UNCOLLAPSED_MATCHES_ESTIMATED:
            result << static_cast<uint32_t>(
                    mset.get_uncollapsed_matches_estimated());
            break;

        case MI_UNCOLLAPSED_MATCHES_UPPER_BOUND:
            result << static_cast<uint32_t>(
                mset.get_uncollapsed_matches_upper_bound());
            break;

        case MI_SIZE:
            result << static_cast<uint32_t>(mset.size());
            break;

        case MI_GET_MAX_POSSIBLE:
            result << static_cast<double>(mset.get_max_possible());
            break;

        case MI_GET_MAX_ATTAINED:
            result << static_cast<double>(mset.get_max_attained());
            break;

        case MI_TERM_WEIGHT:
        {
            const std::string& tname = params;
            result << static_cast<double>(mset.get_termweight(tname));
            break;
        }

        case MI_TERM_FREQ:
        {
            const std::string& tname = params;
            result << static_cast<uint32_t>(mset.get_termfreq(tname));
            break;
        }

        default:
            throw BadCommandDriverError(command);
    }
}


void
Driver::databaseInfo(PR)
{
    while (uint8_t command = params)
    switch(command)
    {
        case DBI_HAS_POSITIONS:
            result << static_cast<uint8_t>(m_db.has_positions());
            break;

        case DBI_DOCCOUNT:
            result << static_cast<uint32_t>(m_db.get_doccount());
            break;

        case DBI_LASTDOCID:
            result << static_cast<uint32_t>(m_db.get_lastdocid());
            break;

        case DBI_AVLENGTH:
            result << static_cast<double>(m_db.get_avlength());
            break;


        case DBI_TERM_EXISTS:
        {
            const std::string& tname = params;
            result << static_cast<uint8_t>(m_db.term_exists(tname));
            break;
        }

        case DBI_TERM_FREQ:
        {
            const std::string& tname = params;
            if (result.maybe(m_db.term_exists(tname)))
                result << static_cast<uint32_t>(m_db.get_termfreq(tname));
            break;
        }

        case DBI_COLLECTION_FREQ:
        {
            const std::string& tname = params;
            if (result.maybe(m_db.term_exists(tname)))
                result << static_cast<uint32_t>(m_db.get_collection_freq(tname));
            break;
        }

        case DBI_VALUE_FREQ:
        {
            const Xapian::valueno slot = params;
            result << static_cast<uint32_t>(m_db.get_value_freq(slot));
            break;
        }

        case DBI_VALUE_LOWER_BOUND:
        {
            const Xapian::valueno slot = params;
            result << m_db.get_value_lower_bound(slot);
            break;
        }

        case DBI_VALUE_UPPER_BOUND:
        {
            const Xapian::valueno slot = params;
            result << m_db.get_value_upper_bound(slot);
            break;
        }

        case DBI_DOCLENGTH_LOWER_BOUND:
            result << m_db.get_doclength_lower_bound();
            break;

        case DBI_DOCLENGTH_UPPER_BOUND:
            result << m_db.get_doclength_upper_bound();
            break;

        case DBI_WDF_UPPER_BOUND:
        {
            const std::string& tname = params;
            if (result.maybe(m_db.term_exists(tname)))
                result << static_cast<uint32_t>(m_db.get_wdf_upper_bound(tname));
            break;
        }

        case DBI_DOCLENGTH:
        {
            const Xapian::docid docid = params;
            try
            {
                Xapian::termcount len = m_db.get_doclength(docid);
                result.maybe(true);
                result << static_cast<uint32_t>(len);
            } catch (Xapian::DocNotFoundError e) 
            {
                result.maybe(false);
            }
            break;
        }

        case DBI_UUID:
            result << m_db.get_uuid();
            break;

        case DBI_METADATA:
        {
            const std::string& key = params;
            result << m_db.get_metadata(key);
            break;
        }

// TODO: synonym, spellcorrection

        default:
            throw BadCommandDriverError(command);
    }
}

void
Driver::matchSpyInfo(PR)
{
    Resource::Element elem = m_store.extract(params);
    Xapian::MatchSpy&
    spy = elem;
    while (uint8_t field = params)
    switch (field)
    {
        case SPY_DOCUMENT_COUNT:
        {
            // This field is for ValueCountMatchSpy only.
            Xapian::ValueCountMatchSpy&
            vc_spy = elem;
            uint32_t document_count = vc_spy.get_total();
            result << document_count;
            break;
        }

        case SPY_SLOT:
        {
            // The extension is for fields, that have no public access.
            Extension::ValueCountMatchSpy&
            vc_spy_ext = elem;
            
            uint32_t slot = vc_spy_ext.getSlot();
            result << slot;
            break;
        }

        default:
            throw BadCommandDriverError(field);
    }
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

    const uint8_t encoder_type = driver_params;

    switch (encoder_type)
    {
        case TERM_VALUE:
        {
            while(true)
            {
                const std::string& term = driver_params;
                // first term is not empty
                assert(!terms.empty() || !term.empty());
                if (term.empty()) break;
                terms.insert(term);
                assert(!terms.empty());
            }
            break;
        }

        case TERM_FLOAT_VALUE:
        {
            for (uint32_t length = driver_params; length; length--)
            {
                const double& float_term = driver_params;
                const std::string& term = Xapian::sortable_serialise(float_term);

                terms.insert(term);
                assert(!terms.empty());
            }
            break;
        }

        default:
            throw BadCommandDriverError(encoder_type);
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
