#ifndef USER_RESOURCES_H
#define USER_RESOURCES_H

#include "object_register.h"
#include "param_decoder.h"
#include <stdint.h>

#include "xapian_exception.h"

typedef void*       ResourceObjectP;
typedef uint8_t     ResourceObjectType;
typedef uint32_t    ResourceObjectNum;

class ResourceManager;
class ResourceGenerator;

namespace Xapian
{
class Database;
}


// Use the class to form a namespace for the enum
class ResourceType
{
public:
    enum ResourceValidObjectType
    {
        ENQUIRE         = 0,
        MSET            = 1,
        QLC_TABLE       = 2,
        WEIGHT          = 3,
        KEY_MAKER       = 4,
        QUERY           = 5,
        MATCH_DECIDER   = 6,
        STEM            = 7,
        EXPAND_DECIDER  = 8,
        DATE_VALUE_RANGE_PROCESSOR = 9,
        MATCH_SPY       = 10,
        DOCUMENT        = 11,
        LAST_TYPE       = 11
    };

    /**
     * Check ResourceObjectType value
     */
    static void
    validateType(ResourceObjectType type)
    {
        if (type > LAST_TYPE)
            throw ElementNotFoundDriverError(type);
    }
};


/**
 * It is a number for each class, which can be handled as a resource.
 */
typedef ResourceType::ResourceValidObjectType ResourceValidObjectType;

/**
 * CreateUserResourceFn is a function for calling new Constructor of a resource.
 */
typedef ResourceObjectP (*CreateUserResourceFn)
    (ResourceManager& manager, ParamDecoder& params);


#include <iostream>
/**
 * Encapsulate information about how to create new resource.
 */
class UserResource
{
    ResourceValidObjectType m_type;
    std::string m_name;
    CreateUserResourceFn m_creator;

    public:
    UserResource(ResourceValidObjectType type, 
        const std::string& name, 
        CreateUserResourceFn creator)
        : m_type(type), m_name(name), m_creator(creator)
    {
    }

    ResourceObjectP
    create(ResourceManager& manager, ParamDecoder& params)
    {
        return m_creator(manager, params);
    }

    const std::string& getName()
    {
        return m_name;
    }

    uint8_t getType()
    {
        return m_type;
    }
};


/**
 * This class is single for each opened driver.
 * It contains users' resource definitions.
 */
class ResourceGenerator
{
    /**
     * Stores resource creator functions
     */
    ObjectRegister<UserResource>
    m_resources;


    public:
    ResourceGenerator()
    {}

    ResourceGenerator(const ResourceGenerator& /*generator*/)
    {
        // copy?
        assert(false);
    }

    ResourceGenerator(ResourceGenerator& /*generator*/)
    {
        // copy?
        assert(false);
    }

    /**
     * Register callback
     */
    void
    add(UserResource* resource)
    {
        uint32_t num = m_resources.put(resource);
        assert(resource == m_resources.get(num));
    }


    /**
     * Create an object for driver
     * Return a pointer on new object
     */
    ResourceObjectP
    create(ResourceManager& manager, ParamDecoder& params)
    {
        ResourceObjectNum   num = params;

        // res contains information about how to create the object
        UserResource&       res = *m_resources.get(num);
        return res.create(manager, params);
    }


    /**
     * Get added resources for next registration.
     * Don't call it, it is called from driver_open
     */
    ObjectRegister<UserResource>&
    getRegister()
    {
        return m_resources;   
    }
};



/**
 * Resource manager is single for each port (for each gen_server).
 * It contains function for searching among active resources.
 */
class ResourceManager
{
    ResourceGenerator& 
    m_generator;

    Xapian::Database*
    mp_db;

    /**
     * Pointers on stores of opened resources objects.
     * If LAST_TYPE is 2, then the size of this array is 3.
     */
    ObjectBaseRegister*
    m_stores[ResourceType::LAST_TYPE + 1];

    public:

    ResourceManager(ResourceGenerator& generator) 
        : m_generator(generator) 
    {
        mp_db = NULL;
    }

    void
    set_database(Xapian::Database& db)
    {
        mp_db = &db;
    }

    Xapian::Database&
    get_database()
    {
        if (mp_db == NULL)
            throw DbIsNotReadyDriverError();

        return *mp_db;
    }

    /**
     * Get a resource object.
     */
    ResourceObjectP
    get(ResourceValidObjectType type, ParamDecoder& params)
    {
        ResourceObjectNum   num = params;
        ResourceType::validateType(type);
        return m_stores[type]->getVoidPointer(num);
    }


    /**
     * Get a resource manager.
     * used by the driver.
     */
    ObjectBaseRegister&
    get(uint8_t type)
    {
        ResourceType::validateType(type);
        return get(static_cast<ResourceValidObjectType>(type));
    }


    /**
     * Return an object which contains resources.
     */
    ObjectBaseRegister&
    get(ResourceValidObjectType type)
    {
        return * m_stores[type];
    }

    /**
     * Add an object register.
     * Called by the driver.
     */
    void
    add(ResourceValidObjectType type, ObjectBaseRegister* reg)
    {
        m_stores[type] = reg;
    }

    
    /**
     * Create and register an object for driver.
     * Return an id of new object.
     */
    ResourceObjectNum
    createAndRegister(ParamDecoder& params)
    {
        // Get type of resource
        ResourceObjectType          type_ = params;
        ResourceType::validateType(type_);
        ResourceValidObjectType     type  = static_cast<ResourceValidObjectType>( type_ );

        // Create an object
        ResourceObjectP resource_object = m_generator.create(*this, params);

        // Get a store for the object
        ObjectBaseRegister& store = get(type);

        // Register the object in the store and return its number
        return store.putVoidPointer(resource_object);
    }
};


void
registerUserCallbacks(ResourceGenerator& generator);


#endif
