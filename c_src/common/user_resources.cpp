#include "user_resources.h"
#include "spy_value_ctrl.h"
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


void
registerUserCallbacks(ResourceGenerator& generator)
{
    generator.add(new UserResource(ResourceType::WEIGHT, 
        std::string("bool_weight"), &createBoolWeight));

    generator.add(new UserResource(ResourceType::WEIGHT, 
        std::string("bm25_weight"), &createBM25Weight));

    generator.add(new UserResource(ResourceType::WEIGHT, 
        std::string("trad_weight"), &createTradWeight));

    generator.add(new UserResource(ResourceType::MATCH_SPY, 
        std::string("value_count_match_spy"), &createValueCountMatchSpy));
}

XAPIAN_ERLANG_NS_END
