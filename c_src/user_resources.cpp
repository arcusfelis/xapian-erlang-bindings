#include "user_resources.h"
#include "xapian.h"

/**
 * This file describes how to create resources
 */

ResourceObjectP
createMyMset(ResourceManager& manager, ParamDecoder& params)
{
    return new Xapian::MSet();
}

void
registerUserCallbacks(ResourceGenerator& generator)
{
    generator.add(new UserResource(ResourceType::MSET, 
        std::string("my_mset"), createMyMset));
}
