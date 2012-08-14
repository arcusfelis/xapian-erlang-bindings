#ifndef XAPIAN_CORE_H
#define XAPIAN_CORE_H
#include <xapian.h>
#include <string>
#include <stdint.h>

#include "result_encoder.h"
#include "query_parser_factory.h"
#include "term_generator_factory.h"
#include "qlc.h"
#include "resource/factory.h"


#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

// internal
class HellTermPosition;


// -------------------------------------------------------------------
// Main Driver Class
// -------------------------------------------------------------------

class Driver 
{
    Xapian::Database m_db;
    Xapian::WritableDatabase m_wdb;
    Xapian::Stem m_default_stemmer;

    Xapian::QueryParser m_default_parser;
    Xapian::QueryParser m_standard_parser;
    Xapian::TermGenerator m_default_generator;
    Xapian::TermGenerator m_standard_generator;

    QueryParserFactory m_default_parser_factory;
    QueryParserFactory m_standard_parser_factory;
    TermGeneratorFactory m_default_generator_factory;
    TermGeneratorFactory m_standard_generator_factory;

    /**
     * It is global.
     * It knows how to create user customized resources.
     */
    Resource::Factory m_store;

    /**
     * It is different for each port.
     * It is a manager of ObjectRegisters.
     */
    unsigned            m_number_of_databases;
    MemoryManager&      m_mm;

    /// Assignment operator.
    /// Assignment is not allowed.
    Driver & operator= (const Driver & /*source*/) { assert(false); return *this; }

    /// Copy constructor.
    /// Copy is not allowed.
    Driver(const Driver & source) : m_mm(source.m_mm) { assert(false); }

    public:
    friend class MSetQlcTable;
    friend class TermQlcTable;

    // Commands
    // used in the control function
    enum e_command {
        OPEN                        = 0,
        LAST_DOC_ID                 = 1,
        ADD_DOCUMENT                = 2,
        TEST                        = 3,
        GET_DOCUMENT_BY_ID          = 4,
        START_TRANSACTION           = 5,
        CANCEL_TRANSACTION          = 6,
        COMMIT_TRANSACTION          = 7,
        QUERY_PAGE                  = 8,
        SET_DEFAULT_STEMMER         = 9,
        SET_DEFAULT_PREFIXES        = 10,
        ENQUIRE                     = 11,
        RELEASE_RESOURCE            = 12,
        MATCH_SET                   = 13,
        QLC_INIT                    = 14,
        QLC_NEXT_PORTION            = 15,
        QLC_LOOKUP                  = 16,
        GET_RESOURCE_CONSTRUCTORS   = 17,
        CREATE_RESOURCE             = 18,
        MSET_INFO                   = 19,
        DB_INFO                     = 20,
        DELETE_DOCUMENT             = 21,
        REPLACE_OR_CREATE_DOCUMENT  = 22,
        SET_METADATA                = 23,
        UPDATE_DOCUMENT             = 24,
        UPDATE_OR_CREATE_DOCUMENT   = 25,
        DOCUMENT                    = 26,
        OPEN_PROG                   = 27,
        OPEN_TCP                    = 28,
        CLOSE                       = 29,
        DOCUMENT_INFO               = 30,
        DOCUMENT_INFO_RESOURCE      = 31,
        IS_DOCUMENT_EXIST           = 32,
        REPLACE_DOCUMENT            = 33,
        RELEASE_RESOURCES           = 34,
        MATCH_SPY_INFO              = 35,
        CREATE_QUERY_PARSER         = 36,
        PARSE_STRING                = 37,
        ADD_SPELLING                = 38
    };


    // Error prefix tags.
    // used in the control function
    enum e_errorCode {
        SUCCESS                     = 0,
        ERROR                       = 1
    };

    // see fun xapian_common:append_unique_document_id/2
    enum e_uniqueIdType {
        UNIQUE_DOCID                = 1,
        UNIQUE_TERM                 = 2
    };

    // Modes for opening of a db
    // used in the open function
    enum e_openMode {
        READ_OPEN                   = 0,
        WRITE_CREATE_OR_OPEN        = 1,
        WRITE_CREATE                = 2,
        WRITE_CREATE_OR_OVERWRITE   = 3,
        WRITE_OPEN                  = 4
    };

    /// Types of fields.
    /// Used in the @ref applyDocument function.
    enum e_fieldTypeIn {
        STEMMER                     = 1,  /// Set a stemmer.
        DATA                        = 2,  /// Set data.
        DELTA                       = 3,  /// Add delta.
        TEXT                        = 4,  /// Set text.
        TERM_GENERATOR              = 5,  /// Select TermGenerator.
                                           
        SET_POSTING                 = 15, /// Add posting term.
        ADD_POSTING                 = 25,
        UPDATE_POSTING              = 35,
        REMOVE_POSTING              = 45,

        SET_TERM                    = 16,
        ADD_TERM                    = 26,
        UPDATE_TERM                 = 36,
        REMOVE_TERM                 = 46,

        ADD_VALUE                   = 17,
        SET_VALUE                   = 27,
        UPDATE_VALUE                = 37,
        REMOVE_VALUE                = 47,

        DEC_WDF                     = 101,
        SET_WDF                     = 111,
        REMOVE_VALUES               = 103, /// Clear all values.
        REMOVE_TERMS                = 104, /// Clear all terms and postings.
        REMOVE_POSITIONS            = 105,
        REMOVE_TERM_POSITIONS       = 106,
        REMOVE_TERM_POSITIONS_SAVE  = 116
    };

    /// Types of the fields.
    /// Used in the retrieveDocument function.
    enum e_fieldTypeOut {
        GET_VALUE                   = 1,
        GET_FLOAT_VALUE             = 2,
        GET_DATA                    = 3,
        GET_DOCID                   = 4,
        GET_WEIGHT                  = 5,
        GET_RANK                    = 6,
        GET_PERCENT                 = 7,
        GET_MULTI_DOCID             = 8,
        GET_DB_NUMBER               = 9
    };

    enum e_encodedValueType {
        STRING_TYPE = 0,
        DOUBLE_TYPE = 1
    };

    /// Numbers of tests.
    /// Used in the test function.
    enum e_testNumber {
        TEST_RESULT_ENCODER         = 1,
        TEST_EXCEPTION              = 2,
        TEST_ECHO                   = 3,
        TEST_MEMORY                 = 4
    };

    enum e_queryType {
        QUERY_GROUP                 = 1,
        QUERY_VALUE                 = 2,
        QUERY_VALUE_RANGE           = 3,
        QUERY_TERM                  = 4,
        QUERY_PARSER                = 5,
        QUERY_SCALE_WEIGHT          = 6,  /// query, double
        QUERY_REFERENCE             = 7
    };

    enum e_queryParserCommand {
        QP_STEMMER                  = 1,
        QP_STEMMING_STRATEGY        = 2,
        QP_MAX_WILDCARD_EXPANSION   = 3,
        QP_DEFAULT_OP               = 4,
        QP_PARSER_TYPE              = 5,
        QP_PREFIX                   = 6,
        QP_VALUE_RANGE_PROCESSOR    = 7,
        QP_FROM_RESOURCE            = 8,
        QP_STEMMER_RESOURCE         = 9,
        QP_STOPPER_RESOURCE         = 10
    };

    enum e_termGeneratorCommand {
        TG_STEMMER                  = 1,
        TG_STEMMING_STRATEGY        = 2,
        TG_GENERATOR_TYPE           = 3,
        TG_FROM_RESOURCE            = 4,
        TG_STEMMER_RESOURCE         = 5,
        TG_STOPPER_RESOURCE         = 6
    };

    enum e_queryParserType {
        QP_TYPE_DEFAULT             = 0,
        QP_TYPE_EMPTY               = 1
    };

    enum e_termGeneratorType {
        TG_TYPE_DEFAULT             = 0,
        TG_TYPE_EMPTY               = 1
    };

    enum e_parseStringFieldId {
        PS_QUERY_RESOURCE           = 1,
        PS_CORRECTED_QUERY_STRING   = 2
     };


    /// see `xapian_enquire:encode'
    enum e_enquireCommand {
        EC_STOP                     = 0,
        EC_QUERY                    = 1,
        EC_QUERY_LEN                = 2,
        EC_ORDER                    = 3,
        EC_DOCID_ORDER              = 4,
        EC_WEIGHTING_SCHEME         = 5,
        EC_CUTOFF                   = 6,
        EC_COLLAPSE_KEY             = 7
    };

    enum e_enquireOrderTypes {
        OT_KEY                = 1,
        OT_VALUE              = 2,
        OT_KEY_RELEVANCE      = 3,
        OT_RELEVANCE_KEY      = 4,
        OT_RELEVANCE_VALUE    = 5,
        OT_VALUE_RELEVANCE    = 6
    };

    enum e_msetInfoParams {
        MI_MATCHES_LOWER_BOUND              = 1,
        MI_MATCHES_ESTIMATED                = 2,
        MI_MATCHES_UPPER_BOUND              = 3,
        MI_UNCOLLAPSED_MATCHES_LOWER_BOUND  = 4,
        MI_UNCOLLAPSED_MATCHES_ESTIMATED    = 5,
        MI_UNCOLLAPSED_MATCHES_UPPER_BOUND  = 6,
        MI_SIZE                             = 7,
        MI_GET_MAX_POSSIBLE                 = 8,
        MI_GET_MAX_ATTAINED                 = 9,
        MI_TERM_WEIGHT                      = 10,
        MI_TERM_FREQ                        = 11
    };

    enum e_matchSpyInfoParams {
        SI_DOCUMENT_COUNT = 1,
        SI_VALUE_SLOT     = 2
    };


    enum e_dbInfoParams {
        DBI_HAS_POSITIONS                   = 1,
        DBI_DOCCOUNT                        = 2,
        DBI_LASTDOCID                       = 3,
        DBI_AVLENGTH                        = 4,
        DBI_TERM_EXISTS                     = 5,
        DBI_TERM_FREQ                       = 6,
        DBI_COLLECTION_FREQ                 = 7,
        DBI_VALUE_FREQ                      = 8,
        DBI_VALUE_LOWER_BOUND               = 9,
        DBI_VALUE_UPPER_BOUND               = 10,
        DBI_DOCLENGTH_LOWER_BOUND           = 11,
        DBI_DOCLENGTH_UPPER_BOUND           = 12,
        DBI_WDF_UPPER_BOUND                 = 13,
        DBI_DOCLENGTH                       = 14,
        DBI_UUID                            = 15,
        DBI_METADATA                        = 16
    };

    enum e_termInfoFields {
        TERM_VALUE                          = 1,
        TERM_WDF                            = 2,
        TERM_FREQ                           = 3,
        TERM_POSITIONS                      = 4,
        TERM_POS_COUNT                      = 5,
        TERM_FLOAT_VALUE                    = 6
    };

    enum e_decoderTypeFunIds {
        DEC_DOCUMENT                        = 0,
        DEC_ITERATOR                        = 1,
        DEC_BOTH                            = 2
    };

    enum e_textFlags {
        TEXT_FLAG_POSITIONS = 1
    };


    static const unsigned
    PARSER_FEATURES[];

    static const uint8_t
    PARSER_FEATURE_COUNT;

    static const unsigned
    GENERATOR_FEATURES[];

    static const uint8_t
    GENERATOR_FEATURE_COUNT;

    static const int
    GENERATOR_AND_TEXT_FEATURES_DELIM;

    static const unsigned
    GENERATOR_DEFAULT_FEATURES;

    static const unsigned
    TEXT_DEFAULT_FEATURES;

    static const int
    GENERATOR_AND_TEXT_DEFAULT_FEATURES;

    static const unsigned
    TEXT_FEATURES[];

    static const uint8_t
    TEXT_FEATURE_COUNT;

    static const uint8_t
    STEM_STRATEGY_COUNT;

    static const Xapian::QueryParser::stem_strategy
    STEM_STRATEGIES[];

//  static const uint8_t
//  TG_STEM_STRATEGY_COUNT;

//  static const Xapian::TermGenerator::stem_strategy
//  TG_STEM_STRATEGIES[];

    static const uint8_t
    DOCID_ORDER_TYPE_COUNT;

    static const Xapian::Enquire::docid_order
    DOCID_ORDER_TYPES[];


    ResultEncoder* getResultEncoder();

    /**
     * A constructor.
     */
    Driver(MemoryManager&);

    ~Driver();

    /**
     * Read and execute one command from a client.
     */
    void
    handleCommand(PR, const unsigned int  command);

    void setDefaultStemmer(const Xapian::Stem& stemmer);

    int openWriteMode(uint8_t mode);

    Xapian::Document
    getDocument(ParamDecoder&);

    /*! \name Command helpers. */
    /*! \{ */
    void setDefaultStemmer(ParamDecoder& params);
    void setDefaultPrefixes(ParamDecoder& params);

    void open(uint8_t mode, const std::string& dbpath);
    void open(uint8_t mode, const std::string& host, uint16_t port, 
                uint32_t timeout, uint32_t connect_timeout);
    void open(uint8_t mode, const std::string& prog, const std::string& args, 
                uint32_t timeout);

    void getLastDocId(ResultEncoder&);

    void addDocument(PR);
    void addSpelling(ParamDecoder&);
    void replaceDocument(PR);
    void replaceOrCreateDocument(PR);
    void updateDocument(PR, bool create);
    void isDocumentExist(PR);
    void deleteDocument(PR);
    void setMetadata(ParamDecoder&);

    /**
     * `query_page'
     */
    void query(CPR);

    /**
     * Write a resource.
     */
    void enquire(PR);

    /**
     * Write a resource.
     */
    void document(PR);

    void valueMatchSpyToSlot(PR);

    /**
     * Erase the stored object.
     */
    void releaseResource(ParamDecoder&);
    void releaseResources(ParamDecoder&);

    /**
     * Converts an enquire into a match set.
     * Write a resource.
     */
    void matchSet(CPR);

    void qlcInit(PR);

    void qlcNext(PR);

    void qlcLookup(PR);

    void startTransaction();

    void cancelTransaction();

    void commitTransaction();

    void getDocumentById(PR);

    void documentInfo(PR);

    void documentInfoResource(PR);

    void matchSpyInfo(PR);


    void testResultEncoder(ResultEncoder&, Xapian::docid from, Xapian::docid to);
    void testEcho(PR);
    void testException();
    void testMemory();

    void getResourceConstructors(ResultEncoder&);

    /**
     * Create a resource object using an user function.
     */
    void createResource(PR);
    void test(PR);
    void msetInfo(PR);
    void databaseInfo(PR);
    /*! \} */

    /** 
     * Gets a copy of params.
     *
     * `params' is a clone.
     */
    void retrieveDocument(PCR, Xapian::Document&, Xapian::MSetIterator&);
    void retrieveDocument(PCR, Xapian::Document&);
    void retrieveDocument(PCR, Xapian::MSetIterator&);
    void retrieveDocuments(PCR, Xapian::MSetIterator, Xapian::MSetIterator);
    void selectEncoderAndRetrieveDocument(PR, Xapian::MSetIterator&);

    ParamDecoderController
    retrieveDocumentSchema(ParamDecoder&) const;

    static void
    retrieveTerm(PCR, const Xapian::TermIterator& iter);
    
     
    ParamDecoderController
    retrieveTermSchema(ParamDecoder&) const; 


    /**
     * Read commands, encoded by `xapian_document:encode'.
     * Used in update, replace, add document functions.
     */
    void 
    applyDocument(ParamDecoder&, Xapian::Document& doc);

    /**
     * Run applyDocument without creating the real doc.
     * It is useful, if you want to find the position of the ParamDecoder
     * after running the applyDocument.
     */
    ParamDecoderController
    applyDocumentSchema(ParamDecoder&);
    

    Xapian::Query 
    buildQuery(CP);

    void fillEnquire(CP, Xapian::Enquire& enquire);

    void fillEnquireOrder(CP, Xapian::Enquire& enquire);

    /**
     * Throws error if the database was opened only for reading.
     */
    void assertWriteable() const;

    static unsigned
    idToParserFeature(int type);

    static unsigned 
    decodeParserFeatureFlags(ParamDecoder&);

    static unsigned
    idToGeneratorFeature(int type);
    static unsigned
    idToTextFeature(int type);
    static void
    decodeGeneratorFeatureFlags(
            ParamDecoder& params, 
            unsigned& genFlags,
            unsigned& textFlags);

    static Xapian::QueryParser::stem_strategy
    readStemmingStrategy(ParamDecoder&);

    Xapian::QueryParser
    readParser(CP);

    Xapian::QueryParser 
    selectParser(ParamDecoder&);

    Xapian::TermGenerator
    readGenerator(CP);

    Xapian::TermGenerator 
    selectGenerator(ParamDecoder& params);

    void addPrefix(ParamDecoder&, Xapian::QueryParser& qp);
    void addPrefix(ParamDecoder&, QueryParserFactory& qpf);



    private:
    Xapian::docid docid_sub(const Xapian::docid docid_combined)
    {
        return (docid_combined - 1) / m_number_of_databases + 1;
    }

    Xapian::docid subdb_num(const Xapian::docid docid_combined)
    {
        // Encounted from 1
        return (docid_combined - 1) % m_number_of_databases + 1; 
    }

    /*! \name Private static helpers. */
    /*! \{ */
    
    TermGenerator::Iterator*
    termGenerator(ParamDecoder& params, 
        int8_t qlc_type, int8_t resource_type, uint32_t resource_num);

    
    static void
    handlePosting(ParamDecoder&, uint8_t command, Xapian::Document& doc);

    static void
    handleValue  (ParamDecoder&, uint8_t command, Xapian::Document& doc);

    static void
    handleTerm   (ParamDecoder&, uint8_t command, Xapian::Document& doc);

    static void
    handleSpelling  (ParamDecoder&, uint8_t command, 
                    Xapian::WritableDatabase& mdb);


    static void
    qlcTermIteratorLookup(
        ParamDecoder& driver_params, 
        const ParamDecoder& schema_params, 
        ResultEncoder& result,
        Xapian::TermIterator iter,
        const Xapian::TermIterator end);



    static const std::string 
    decodeValue(ParamDecoder& params);
    /*! \} */


    Xapian::Stem&
    extractStem(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::Stopper&
    extractStopper(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::QueryParser&
    extractQueryParser(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::Query&
    extractQuery(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::MatchSpy&
    extractSpy(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::MatchSpy&
    extractWritableSpy(CP);

    Xapian::ValueRangeProcessor&
    extractRangeProcessor(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::Weight&
    extractWeight(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::KeyMaker&
    extractKeyMaker(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::Enquire&
    extractEnquire(CP)
    {
        return m_store.extract(con, params);
    }

    Xapian::MSet&
    extractMSet(CP)
    {
        return m_store.extract(con, params);
    }

    void
    createQueryParser(PR);

    void
    parseString(CPR);

    void
    setDatabaseAgain();
};

XAPIAN_ERLANG_NS_END
#endif
