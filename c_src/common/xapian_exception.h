#ifndef XAPIAN_EXCEPTION_H
#define XAPIAN_EXCEPTION_H

#include <stdexcept>
#include <stdint.h>
#include <string>

// -------------------------------------------------------------------
// Exceptions
// -------------------------------------------------------------------

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class DriverRuntimeError: public std::runtime_error
{
    const char* m_type;

    public:
    DriverRuntimeError(const char * type, const std::string& str);

    const char* 
    get_type() const;
};

class MemoryAllocationDriverError: public DriverRuntimeError
{
    static const char TYPE[];
    public:

    MemoryAllocationDriverError(size_t size);

    static const std::string 
    buildString(size_t size);
};


class BadCommandDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    BadCommandDriverError(int command_id);

    static const std::string 
    buildString(int command_id);
};


class NotImplementedCommandDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    NotImplementedCommandDriverError(int command_id);

    static const std::string 
    buildString(int command_id);
};


class BadArgumentDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    BadArgumentDriverError();
};


class EmptySetDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    EmptySetDriverError();
};


class OverflowDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    OverflowDriverError();
};

class NotWritableDatabaseError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    NotWritableDatabaseError();
};

class DbIsNotReadyDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    DbIsNotReadyDriverError();
};

class ElementNotFoundDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    ElementNotFoundDriverError(uint32_t num);

    static const std::string 
    buildString(uint32_t num);
};

class GroupResourceTypeMismatchDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    GroupResourceTypeMismatchDriverError(uint32_t passed, uint32_t expected);

    static const std::string 
    buildString(uint32_t passed, uint32_t expected);
};

class MatchSpyFinalizedDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    MatchSpyFinalizedDriverError();
};

class ResourceTypeMismatchDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    ResourceTypeMismatchDriverError(const std::string& passed, 
                                    const std::string& expected);

    static const std::string 
    buildString(const std::string& passed, const std::string& expected);
};


class AbstractMethodDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    AbstractMethodDriverError(const std::string& object_type, 
                              const std::string& method_name);

    static const std::string
    buildString(const std::string& object_type, const std::string& method_name);
};


class AlreadyAttachedDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    AlreadyAttachedDriverError(const std::string& parent_type, 
                               const std::string& child_type);

    static const std::string 
    buildString(const std::string& parent_type, const std::string& child_type);
};

XAPIAN_ERLANG_NS_END
#endif
