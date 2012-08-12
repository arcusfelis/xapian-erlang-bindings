#include <stdint.h>
#include <xapian.h>

#include "termiter_qp_gen.h"
#include "termiter_spy_gen.h"
#include "termiter_doc_gen.h"
#include "xapian_exception.h"
#include "param_decoder.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

enum ObjectType
{
    VALUES           = 0,
    TOP_VALUES       = 1,
    UNSTEM           = 0,
    STOP_LIST        = 1
};

TermIteratorGenerator*
TermIteratorGenerator::create(
        ParamDecoder& params,
        Xapian::ValueCountMatchSpy& spy)
{
    switch (uint8_t type = params)
    {
        case VALUES:
            return new SpyValueIteratorGenerator(spy);

        case TOP_VALUES:
        {
            const uint32_t max_values = params;
            if (!max_values)
                throw BadArgumentDriverError();
            return new TopSpyValueIteratorGenerator(spy, max_values);
        }

        default:
            throw BadCommandDriverError(type);
    }
}


TermIteratorGenerator*
TermIteratorGenerator::create(Xapian::Document& doc)
{
    return new DocumentTermIteratorGenerator(doc);
}

TermIteratorGenerator*
TermIteratorGenerator::create(
        ParamDecoder& params,
        Xapian::QueryParser& qp)
{
    switch (uint8_t type = params)
    {
        case UNSTEM:
        {
            std::string term = params;
            return new UnstemQueryParserIteratorGenerator(qp, term);
        }

        case STOP_LIST:
        {
            return new StopListQueryParserIteratorGenerator(qp);
        }

        default:
            throw BadCommandDriverError(type);
    }
}

XAPIAN_ERLANG_NS_END
