#include "user_resources.h"
#include "spy_value_ctrl.h"
#include "key_maker_ctrl.h"
#include "val_range_ctrl.h"
#include "xapian.h"

/**
 * This file describes how to create resources
 */

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

ResourceObjectP
createBoolWeight(ResourceManager& /*manager*/, ParamDecoder& /*params*/)
{
    return new Xapian::BoolWeight();
}


ResourceObjectP
createBM25Weight(ResourceManager& /*manager*/, ParamDecoder& params)
{
    double k1 = params; 
    double k2 = params; 
    double k3 = params;
    double b  = params; 
    double min_normlen = params;
    return new Xapian::BM25Weight(k1, k2, k3, b, min_normlen);
}


ResourceObjectP
createTradWeight(ResourceManager& /*manager*/, ParamDecoder& params)
{
    double k = params; 
    return new Xapian::TradWeight(k);
}


ResourceObjectP
createValueCountMatchSpy(ResourceManager& /*manager*/, ParamDecoder& params)
{
    uint32_t slot = params; 
    SpyController* controller = 
    new ValueCountSpyController(slot, 
        new Xapian::ValueCountMatchSpy(slot));
    return controller;
}


ResourceObjectP
createMultiValueKeyMaker(ResourceManager& /*manager*/, ParamDecoder& params)
{
    Xapian::MultiValueKeyMaker* km = new Xapian::MultiValueKeyMaker();
    uint32_t slot;
    uint8_t  is_reversed;
    while ( (slot = params) != Xapian::BAD_VALUENO )
    { 
        is_reversed = params;
        km->add_value(slot, is_reversed);
    }
    return new KeyMakerController(km);
}


ResourceObjectP
createDateValueRangeProcessor3(ResourceManager& /*manager*/, ParamDecoder& params)
{
    uint32_t slot       = params;
     int32_t epoch_year = params;
    uint8_t  prefer_mdy = params;
    return new ValueRangeProcessorController(
            new Xapian::DateValueRangeProcessor(slot, prefer_mdy, epoch_year));
}


ResourceObjectP
createDateValueRangeProcessor5(ResourceManager& /*manager*/, ParamDecoder& params)
{
    uint32_t    slot          = params;
    const std::string& str    = params;
    uint8_t     prefix        = params;
     int32_t    epoch_year    = params;
    uint8_t     prefer_mdy    = params;
    return  new ValueRangeProcessorController(
                new Xapian::DateValueRangeProcessor(
                    slot, str, prefix, prefer_mdy, epoch_year));
}


ResourceObjectP
createNumberValueRangeProcessor1(ResourceManager& /*m*/, ParamDecoder& params)
{
    uint32_t slot         = params;
    return  new ValueRangeProcessorController(
                new Xapian::NumberValueRangeProcessor(slot));
}


ResourceObjectP
createNumberValueRangeProcessor3(ResourceManager& /*m*/, ParamDecoder& params)
{
    uint32_t slot          = params;
    const std::string& str = params;
    uint8_t  prefix        = params;
    return  new ValueRangeProcessorController(
                new Xapian::NumberValueRangeProcessor(slot, str, prefix));
}


ResourceObjectP
createStringValueRangeProcessor1(ResourceManager& /*m*/, ParamDecoder& params)
{
    uint32_t slot         = params;
    return  new ValueRangeProcessorController(
                new Xapian::StringValueRangeProcessor(slot));
}


ResourceObjectP
createStringValueRangeProcessor3(ResourceManager& /*m*/, ParamDecoder& params)
{
    uint32_t slot          = params;
    const std::string& str = params;
    uint8_t  prefix        = params;
    return  new ValueRangeProcessorController(
                new Xapian::StringValueRangeProcessor(slot, str, prefix));
}


void
registerUserCallbacks(ResourceGenerator& generator)
{
    // Collect matadata about functions
    generator.add(new UserResource(ResourceType::WEIGHT, 
        std::string("bool_weight"), &createBoolWeight));

    generator.add(new UserResource(ResourceType::WEIGHT, 
        std::string("bm25_weight"), &createBM25Weight));

    generator.add(new UserResource(ResourceType::WEIGHT, 
        std::string("trad_weight"), &createTradWeight));

    generator.add(new UserResource(ResourceType::MATCH_SPY, 
        std::string("value_count_match_spy"), &createValueCountMatchSpy));

    generator.add(new UserResource(ResourceType::MATCH_SPY, 
        std::string("value_count_match_spy"), &createValueCountMatchSpy));

    generator.add(new UserResource(ResourceType::KEY_MAKER, 
        std::string("multi_value_key_maker"), &createMultiValueKeyMaker));

    generator.add(new UserResource(ResourceType::VALUE_RANGE_PROCESSOR, 
        std::string("date_value_range_processor3"), 
        &createDateValueRangeProcessor3));

    generator.add(new UserResource(ResourceType::VALUE_RANGE_PROCESSOR, 
        std::string("date_value_range_processor5"), 
        &createDateValueRangeProcessor5));

    generator.add(new UserResource(ResourceType::VALUE_RANGE_PROCESSOR, 
        std::string("number_value_range_processor1"), 
        &createNumberValueRangeProcessor1));

    generator.add(new UserResource(ResourceType::VALUE_RANGE_PROCESSOR, 
        std::string("number_value_range_processor3"), 
        &createNumberValueRangeProcessor3));

    generator.add(new UserResource(ResourceType::VALUE_RANGE_PROCESSOR, 
        std::string("string_value_range_processor1"), 
        &createStringValueRangeProcessor1));

    generator.add(new UserResource(ResourceType::VALUE_RANGE_PROCESSOR, 
        std::string("string_value_range_processor3"), 
        &createStringValueRangeProcessor3));
}


XAPIAN_ERLANG_NS_END
