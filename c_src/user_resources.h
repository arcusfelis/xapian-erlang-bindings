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

// Use the class to form a namespace for the enum
class ResourceType
{
public:
    enum ResourceValidObjectType
    {
        ENQUIRE     = 0,
        MSET        = 1,
        QLC_TABLE   = 2,
        LAST_TYPE   = 2
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


/**
 * Encapsulate information about how to create new resource.
 */
class UserResource
{
    CreateUserResourceFn m_creator;
    std::string m_name;
    ResourceValidObjectType m_type;

    public:
    UserResource(ResourceValidObjectType type, 
        const std::string& name, 
        CreateUserResourceFn creator)
        : m_creator(creator), m_name(name), m_type(type)
    {}

    ResourceObjectP
    create(ResourceManager& manager, ParamDecoder& params)
    {
        return m_creator(manager, params);
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

    /**
     * Register callback
     */
    void
    add(UserResource* resource)
    {
        m_resources.put(resource);
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
//  ObjectRegister<UserResource>()
};



/**
 * Resource manager is single for each port (for each gen_server).
 * It contains function for searching among active resources.
 */
class ResourceManager
{
    /**
     * Pointers on stores of opened resources objects.
     */
    ObjectBaseRegister*
    m_stores[ResourceType::LAST_TYPE];

    ResourceGenerator& 
    m_generator;

    Xapian::Database*
    mp_db;

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


ResourceObjectP
createMyMset(ResourceManager& manager, ParamDecoder& params);

void
registerUserCallbacks(ResourceGenerator& generator);

#endif
