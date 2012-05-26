#ifndef XAPIAN_CORE_H
#define XAPIAN_CORE_H

#include "result_encoder.h"
#include "qlc_table.h"
#include "object_register.h"
#include "user_resources.h"
#include "spy_ctrl.h"
#include <xapian.h>
#include <string>
#include <stdint.h>

#include "erl_driver.h"
/* Hack to handle R15 driver used with pre R15 driver */
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif


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

    ResultEncoder m_result;
    Xapian::QueryParser m_default_parser;
    Xapian::QueryParser m_empty_parser;
    ObjectRegister<Xapian::Document>            m_document_store;
    ObjectRegister<Xapian::Enquire>             m_enquire_store;
    ObjectRegister<Xapian::MSet>                m_mset_store;
    ObjectRegister<QlcTable>                    m_qlc_store;
    ObjectRegister<const Xapian::Weight>        m_weight_store;
    ObjectRegister<Xapian::KeyMaker>            m_key_maker_store;
    ObjectRegister<const Xapian::Query>         m_query_store;
    ObjectRegister<const Xapian::MatchDecider>  m_match_decider_store;
    ObjectRegister<const Xapian::Stem>          m_stem_store;
    ObjectRegister<const Xapian::ExpandDecider> m_expand_decider_store;
    ObjectRegister<const Xapian::DateValueRangeProcessor> 
        m_date_value_range_processor_store;
    ObjectRegister<SpyController>               m_match_spy_store;

    /**
     * It is global.
     * It knows how to create user customized resources.
     */
    ResourceGenerator&  m_generator;

    /**
     * It is different for each port.
     * It is a manager of ObjectRegisters.
     */
    ResourceManager     m_stores;
    unsigned            m_number_of_databases;

    public:
    friend class MSetQlcTable;
    friend class TermQlcTable;

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
        QUERY_PAGE                  = 8,
        SET_DEFAULT_STEMMER         = 9,
        SET_DEFAULT_PREFIXES        = 10,
        ENQUIRE                     = 11,
        RELEASE_RESOURCE            = 12,
        MATCH_SET                   = 13,
        QLC_INIT                    = 14,
        QLC_NEXT_PORTION            = 15,
        QLC_LOOKUP                  = 16,
        GET_RESOURCE_INFO           = 17,
        CREATE_RESOURCE             = 18,
        MSET_INFO                   = 19,
        DB_INFO                     = 20,
        DELETE_DOCUMENT             = 21,
        REPLACE_DOCUMENT            = 22,
        SET_METADATA                = 23,
        UPDATE_DOCUMENT             = 24,
        UPDATE_OR_CREATE_DOCUMENT   = 25,
        DOCUMENT                    = 26,
        OPEN_PROG                   = 27,
        OPEN_TCP                    = 28
    };


    // Error prefix tags.
    // used in the control function
    enum errorCode {
        SUCCESS                     = 0,
        ERROR                       = 1
    };

    // see fun xapian_common:append_unique_document_id/2
    enum uniqueIdType {
        UNIQUE_DOCID                = 1,
        UNIQUE_TERM                 = 2
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

    /// Types of fields.
    /// Used in the @ref applyDocument function.
    enum fieldTypeIn {
        STEMMER                     = 1,  /// Set a stemmer.
        DATA                        = 2,  /// Set data.
        DELTA                       = 3,  /// Add delta.
        TEXT                        = 4,  /// Set text.
                                           
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
    enum fieldTypeOut {
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

    enum encodedValueType {
        STRING_TYPE = 0,
        DOUBLE_TYPE = 1
    };

    /// Numbers of tests.
    /// Used in the test function.
    enum testNumber {
        TEST_RESULT_ENCODER         = 1,
        TEST_EXCEPTION              = 2
    };

    enum queryType {
        QUERY_GROUP                 = 1,
        QUERY_VALUE                 = 2,
        QUERY_VALUE_RANGE           = 3,
        QUERY_TERM                  = 4,
        QUERY_PARSER                = 5,
        QUERY_SCALE_WEIGHT          = 6  /// query, double
    };

    enum queryParserCommand {
        QP_STEMMER                  = 1,
        QP_STEMMING_STRATEGY        = 2,
        QP_MAX_WILDCARD_EXPANSION   = 3,
        QP_DEFAULT_OP               = 4,
        QP_PARSER_TYPE              = 5,
        QP_PREFIX                   = 6
    };

    enum queryParserType {
        QP_TYPE_DEFAULT             = 0,
        QP_TYPE_EMPTY               = 1
    };

    /// see `xapian_enquire:encode'
    enum enquireCommand {
        EC_STOP             = 0,
        EC_QUERY            = 1,
        EC_QUERY_LEN        = 2,
        EC_ORDER            = 3,
        EC_DOCID_ORDER      = 4,
        EC_WEIGHTING_SCHEME = 5,
        EC_CUTOFF           = 6,
        EC_COLLAPSE_KEY     = 7
    };

    enum enquireOrderTypes {
        OT_KEY              = 1,
        OT_VALUE            = 2,
        OT_KEY_RELEVANCE    = 3,
        OT_RELEVANCE_KEY    = 4,
        OT_RELEVANCE_VALUE  = 5,
        OT_VALUE_RELEVANCE  = 6
    };

    enum msetInfoParams {
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

    enum dbInfoParams {
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

    enum termInfoFields {
        TERM_VALUE                          = 1,
        TERM_WDF                            = 2,
        TERM_FREQ                           = 3,
        TERM_POSITIONS                      = 4,
        TERM_POS_COUNT                      = 5
    };

    enum decoderTypeFunIds {
        DEC_DOCUMENT                        = 0,
        DEC_ITERATOR                        = 1,
        DEC_BOTH                            = 2
    };

    static const unsigned
    PARSER_FEATURES[];

    static const uint8_t
    PARSER_FEATURE_COUNT;

    static const uint8_t
    STEM_STRATEGY_COUNT;

    static const Xapian::QueryParser::stem_strategy
    STEM_STRATEGIES[];

    static const uint8_t
    DOCID_ORDER_TYPE_COUNT;

    static const Xapian::Enquire::docid_order
    DOCID_ORDER_TYPES[];


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

    ResultEncoder* getResultEncoder();

    /**
     * A constructor.
     */
    Driver(ResourceGenerator&);

    ~Driver();

    void setDefaultStemmer(const Xapian::Stem& stemmer);

    size_t setDefaultStemmer(ParamDecoder& params);

    size_t setDefaultPrefixes(ParamDecoder& params);

    ObjectBaseRegister&
    getRegisterByType(uint8_t type);

    size_t open(uint8_t mode, const std::string& dbpath);
    size_t open(uint8_t mode, const std::string& host, uint16_t port, 
                uint32_t timeout, uint32_t connect_timeout);
    size_t open(uint8_t mode, const std::string& prog, const std::string& args, 
                uint32_t timeout);

    int openWriteMode(uint8_t mode);

    size_t getLastDocId();

    size_t addDocument(ParamDecoder& params);
    size_t replaceDocument(ParamDecoder& params);
    size_t updateDocument(ParamDecoder& params, bool create);
    void deleteDocument(ParamDecoder& params);
    void setMetadata(ParamDecoder& params);

    /**
     * `query_page'
     */
    size_t query(ParamDecoder& params);

    /**
     * Return a resource.
     */
    size_t enquire(ParamDecoder& params);

    Xapian::Document
    getDocument(ParamDecoder& params);

    /**
     * Return a resource 
     */
    size_t document(ParamDecoder& params);

    /**
     * Erase the stored object.
     */
    size_t releaseResource(ParamDecoder& params);

    /**
     * Converts an enquire into a match set.
     * Return a resource.
     */
    size_t matchSet(ParamDecoder& params);

    size_t qlcInit(ParamDecoder& params);

    size_t qlcNext(ParamDecoder& params);

    size_t qlcLookup(ParamDecoder& params);

    /** 
     * Gets a copy of params.
     *
     * `params' is a clone.
     */
    void retrieveDocument(ParamDecoder, Xapian::Document&, Xapian::MSetIterator&);
    void retrieveDocument(ParamDecoder, Xapian::Document&);
    void retrieveDocument(ParamDecoder, Xapian::MSetIterator&);
    void retrieveDocuments(ParamDecoder, 
        Xapian::MSetIterator, Xapian::MSetIterator);
    void selectEncoderAndRetrieveDocument(ParamDecoder&, Xapian::MSetIterator&);

    ParamDecoderController
    retrieveDocumentSchema(ParamDecoder&) const;


    void retrieveTerm(ParamDecoder params, const Xapian::TermIterator& iter);
    
    static void 
    retrieveTerm(
        ParamDecoder params,  
        ResultEncoder& result,
        const Xapian::TermIterator& iter);


    ParamDecoderController
    retrieveTermSchema(ParamDecoder& params) const; 


    /**
     * Read commands, encoded by `xapian_document:encode'.
     * Used in update, replace, add document functions.
     */
    void applyDocument(ParamDecoder& params, Xapian::Document& doc);

    ParamDecoderController
    applyDocumentSchema(ParamDecoder& params) const;
    

    Xapian::Query 
    buildQuery(ParamDecoder& params);

    void fillEnquire(Xapian::Enquire& enquire, ParamDecoder& params);

    void fillEnquireOrder(Xapian::Enquire& enquire, 
        const uint8_t type, const uint32_t value, const bool reverse);

    /**
     * Throws error if the database was opened only for reading.
     */
    void assertWriteable() const;

    size_t startTransaction();

    size_t cancelTransaction();

    size_t commitTransaction();

    size_t getDocumentById(ParamDecoder& params);

    size_t test(ParamDecoder& params);

    size_t testResultEncoder(Xapian::docid from, Xapian::docid to);

    size_t testException();

    size_t getResourceInfo();

    /**
     * Create a resource object using an user function.
     */
    size_t createResource(ParamDecoder& params);
    size_t msetInfo(ParamDecoder& params);
    size_t databaseInfo(ParamDecoder& params);

    static unsigned
    idToParserFeature(uint8_t type);

    static unsigned 
    decodeParserFeatureFlags(ParamDecoder& params);

    static Xapian::QueryParser::stem_strategy
    readStemmingStrategy(ParamDecoder& params);

    Xapian::QueryParser
    readParser(ParamDecoder& params);

    Xapian::QueryParser 
    selectParser(ParamDecoder& params);

    void addPrefix(Xapian::QueryParser& qp, ParamDecoder& params);



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
    
    TermIteratorGenerator*
    termGenerator(ParamDecoder& params, 
        int8_t qlc_type, int8_t resource_type, uint32_t resource_num);

    static Xapian::termcount
    getTermFrequency(Xapian::Document&  doc, const std::string& tname);

    static Xapian::termcount
    getExistedTermFrequency(Xapian::Document&  doc, const std::string& tname);

    
    static void
    tryRemoveValue(
        Xapian::Document& doc, Xapian::valueno slot_no, bool ignoreErrors);


    static void
    tryRemoveTerm(
        Xapian::Document& doc, const std::string& tname, bool ignoreErrors);


    static void
    tryRemovePosting(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termpos tpos, 
        Xapian::termcount wdf_inc,
        bool ignoreErrors);


    static void
    tryDecreaseWDF(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termcount wdf, 
        bool ignoreErrors);


    static void
    trySetWDF(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termcount wdf, 
        bool ignoreErrors);


    static void
    tryClearTermPositions(
        Xapian::Document& doc, 
        const std::string& tname, 
        bool ignoreErrors);


    static void
    clearTermPositions(
        Xapian::Document& doc, 
        const std::string& tname);


    friend class HellTermPosition;

    static void clearTermPositions(Xapian::Document& doc);

    static bool isValueExist(Xapian::Document& doc, Xapian::valueno slot_no);

    static bool isTermExist(Xapian::Document& doc, const std::string& tname);

    static bool
    isPostingExist(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termpos term_pos);

    
    static void
    handlePosting(uint8_t command,
        ParamDecoder& params, 
        Xapian::Document& doc);

    static void
    handleValue(uint8_t command,
        ParamDecoder& params, 
        Xapian::Document& doc);

    static void
    handleTerm(uint8_t command,
        ParamDecoder& params, 
        Xapian::Document& doc);


    static void
    qlcTermIteratorLookup(
        ParamDecoder& driver_params, 
        const ParamDecoder& schema_params, 
        ResultEncoder& result,
        Xapian::TermIterator iter,
        const Xapian::TermIterator end);
    /*! \} */
};

XAPIAN_ERLANG_NS_END
#endif
