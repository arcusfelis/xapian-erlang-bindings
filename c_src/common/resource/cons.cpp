#include "resource/constructor.h"
#include "resource/generator.h"
#include "resource/element.h"
#include "resource/register.h"
#include "param_decoder.h"
#include "xapian_core.h"
#include "xapian.h"

/**
 * This file describes how to create resources
 */

#include "xapian_config.h"
XAPIAN_RESOURCE_NS_BEGIN

using namespace Resource;

Element
createBoolWeight(Register& /*manager*/, ParamDecoder& /*params*/)
{
    return Element::wrap(new Xapian::BoolWeight());
}


Element
createBM25Weight(Register& /*manager*/, ParamDecoder& params)
{
    double k1 = params; 
    double k2 = params; 
    double k3 = params;
    double b  = params; 
    double min_normlen = params;
    return Element::wrap(new Xapian::BM25Weight(k1, k2, k3, b, min_normlen));
}


Element
createTradWeight(Register& /*manager*/, ParamDecoder& params)
{
    double k = params; 
    return Element::wrap(new Xapian::TradWeight(k));
}


Element
createValueCountMatchSpy(Register& /*manager*/, ParamDecoder& params)
{
    uint32_t slot = params; 
    return Element::wrap(slot, new Xapian::ValueCountMatchSpy(slot));
}


Element
createMultiValueKeyMaker(Register& /*manager*/, ParamDecoder& params)
{
    Xapian::MultiValueKeyMaker* p_km = new Xapian::MultiValueKeyMaker();
    uint32_t slot;
    uint8_t  is_reversed;
    while ( (slot = params) != Xapian::BAD_VALUENO )
    { 
        is_reversed = params;
        p_km->add_value(slot, is_reversed);
    }
    return Element::wrap(p_km);
}


Element
createDateValueRangeProcessor3(Register& /*manager*/, ParamDecoder& params)
{
    uint32_t slot       = params;
     int32_t epoch_year = params;
    uint8_t  prefer_mdy = params;
    return Element::wrap(
            new Xapian::DateValueRangeProcessor(slot, prefer_mdy, epoch_year));
}


Element
createDateValueRangeProcessor5(Register& /*manager*/, ParamDecoder& params)
{
    uint32_t    slot          = params;
    const std::string& str    = params;
    uint8_t     prefix        = params;
     int32_t    epoch_year    = params;
    uint8_t     prefer_mdy    = params;
    return Element::wrap(
                new Xapian::DateValueRangeProcessor(
                    slot, str, prefix, prefer_mdy, epoch_year));
}


Element
createNumberValueRangeProcessor1(Register& /*m*/, ParamDecoder& params)
{
    uint32_t slot         = params;
    return Element::wrap(
                new Xapian::NumberValueRangeProcessor(slot));
}


Element
createNumberValueRangeProcessor3(Register& /*m*/, ParamDecoder& params)
{
    uint32_t slot          = params;
    const std::string& str = params;
    uint8_t  prefix        = params;
    return Element::wrap(
                new Xapian::NumberValueRangeProcessor(slot, str, prefix));
}


Element
createStringValueRangeProcessor1(Register& /*m*/, ParamDecoder& params)
{
    uint32_t slot         = params;
    return Element::wrap(
                new Xapian::StringValueRangeProcessor(slot));
}


Element
createStringValueRangeProcessor3(Register& /*m*/, ParamDecoder& params)
{
    uint32_t slot          = params;
    const std::string& str = params;
    uint8_t  prefix        = params;
    return Element::wrap(
                new Xapian::StringValueRangeProcessor(slot, str, prefix));
}


Element
createSimpleStopper(Register& /*m*/, ParamDecoder& params)
{
    Xapian::SimpleStopper* p_stopper = new Xapian::SimpleStopper();
    for(;;)
    {
        const std::string& str = params;
        if (str == "")
            break;
        p_stopper->add(str);
    }
    return Element::wrap(p_stopper);
}


Element
createSimpleStemmer(Register& /*m*/, ParamDecoder& params)
{
    const std::string& language = params;
    Xapian::Stem* p_stemmer = new Xapian::Stem(language);
    return Element::wrap(p_stemmer);
}


Element
createEnquire(Driver& driver, Register& /*m*/, ParamDecoder& params)
{
    // Use an Enquire object on the database to run the query.
    // Create a new context.
    Resource::Element elem =
        Resource::Element::wrap(new Xapian::Enquire(driver.getDatabase()));
    Xapian::Enquire& enquire = elem;

    // Use elem as a context.
    driver.fillEnquire(elem, params, enquire);

    return elem;
}


void
Generator::registerCallbacks(Driver& driver)
{
    // Collect matadata about functions
    add(Constructor::create("bool_weight", 
                            &createBoolWeight));

    add(Constructor::create(std::string("bm25_weight"), 
                            &createBM25Weight));

    add(Constructor::create(std::string("trad_weight"), 
                            &createTradWeight));

    add(Constructor::create(std::string("value_count_match_spy"), 
                            &createValueCountMatchSpy));

    add(Constructor::create(std::string("value_count_match_spy"), 
                            &createValueCountMatchSpy));

    add(Constructor::create(std::string("multi_value_key_maker"), 
                            &createMultiValueKeyMaker));

    add(Constructor::create(std::string("date_value_range_processor3"), 
                            &createDateValueRangeProcessor3));

    add(Constructor::create(std::string("date_value_range_processor5"), 
                            &createDateValueRangeProcessor5));

    add(Constructor::create(std::string("number_value_range_processor1"), 
                            &createNumberValueRangeProcessor1));

    add(Constructor::create(std::string("number_value_range_processor3"), 
                            &createNumberValueRangeProcessor3));

    add(Constructor::create(std::string("string_value_range_processor1"), 
                            &createStringValueRangeProcessor1));

    add(Constructor::create(std::string("string_value_range_processor3"), 
                            &createStringValueRangeProcessor3));

    add(Constructor::create(std::string("simple_stopper"), 
                            &createSimpleStopper));

    add(Constructor::create(std::string("simple_stemmer"), 
                            &createSimpleStemmer));

    add(Constructor::create(driver,
                            std::string("enquire"), 
                            &createEnquire));
}


XAPIAN_RESOURCE_NS_END
