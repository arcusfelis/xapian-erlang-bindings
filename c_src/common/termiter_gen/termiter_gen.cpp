#include <stdint.h>
#include <xapian.h>

#include "termiter_gen.h"

#include "termiter_qp_gen.h"
#include "termiter_spy_gen.h"
#include "termiter_doc_gen.h"
#include "termiter_db_gen.h"
#include "xapian_exception.h"
#include "param_decoder.h"

#include "xapian_config.h"
XAPIAN_TERM_GEN_NS_BEGIN

enum ObjectType
{
    VALUES           = 0,
    TOP_VALUES       = 1,
    UNSTEM           = 0,
    STOP_LIST        = 1,
    SYNONYMS         = 0,
    SPELLINGS        = 1,
    SYNONYM_KEYS     = 2,
    ALL_TERMS        = 3,
    METADATA_KEYS    = 4
};

Iterator*
Iterator::create(
        ParamDecoder& params,
        Xapian::ValueCountMatchSpy& spy)
{
    switch (uint8_t type = params)
    {
        case VALUES:
            return new ValueCountMatchSpy::Values(spy);

        case TOP_VALUES:
        {
            const uint32_t max_values = params;
            if (!max_values)
                throw BadArgumentDriverError();
            return new ValueCountMatchSpy::
                        TopValues(spy, max_values);
        }

        default:
            throw BadCommandDriverError(type);
    }
}


Iterator*
Iterator::create(Xapian::Document& doc)
{
    return new Document::Terms(doc);
}


Iterator*
Iterator::create(
        ParamDecoder& params,
        Xapian::QueryParser& qp)
{
    switch (uint8_t type = params)
    {
        case UNSTEM:
        {
            std::string term = params;
            return new QueryParser::Unstem(qp, term);
        }

        case STOP_LIST:
        {
            return new QueryParser::StopList(qp);
        }

        default:
            throw BadCommandDriverError(type);
    }
}


Iterator*
Iterator::create(
        ParamDecoder& params,
        Xapian::Database& db)
{
    switch (uint8_t type = params)
    {
        case SYNONYMS:
        {
            const std::string& term = params;
            return new Database::Synonyms(db, term);
        }

        case SPELLINGS:
        {
            return new Database::Spellings(db);
        }

        case SYNONYM_KEYS:
        {
            const std::string& prefix = params;
            return new Database::SynonymKeys(db, prefix);
        }

        case ALL_TERMS:
        {
            const std::string& prefix = params;
            return new Database::AllTerms(db, prefix);
        }

        case METADATA_KEYS:
        {
            const std::string& prefix = params;
            return new Database::MetadataKeys(db, prefix);
        }

        default:
            throw BadCommandDriverError(type);
    }
}

XAPIAN_TERM_GEN_NS_END
