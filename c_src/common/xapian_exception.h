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
    const char* m_file;
    uint32_t m_line;

    public:
    DriverRuntimeError(GET_POS, const char * type, const std::string& str);

    const char* 
    get_type() const;

    const char* 
    get_file() const;

    uint32_t
    get_line() const;
};

class MemoryAllocationDriverError: public DriverRuntimeError
{
    static const char TYPE[];
    public:

    MemoryAllocationDriverError(GET_POS, size_t size);

    static const std::string 
    buildString(size_t size);
};


class BadCommandDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    BadCommandDriverError(GET_POS, int command_id);

    static const std::string 
    buildString(int command_id);
};


class NotImplementedCommandDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    NotImplementedCommandDriverError(GET_POS, int command_id);

    static const std::string 
    buildString(int command_id);
};


class BadArgumentDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    BadArgumentDriverError(GET_POS);
};


class EmptySetDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    EmptySetDriverError(GET_POS);
};


class OverflowDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    OverflowDriverError(GET_POS);
};

class NotWritableDatabaseError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    NotWritableDatabaseError(GET_POS);
};

class DbIsNotReadyDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    DbIsNotReadyDriverError(GET_POS);
};

class ElementNotFoundDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    ElementNotFoundDriverError(GET_POS, uint32_t num);

    static const std::string 
    buildString(uint32_t num);
};

class GroupResourceTypeMismatchDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    GroupResourceTypeMismatchDriverError(GET_POS, uint32_t passed, uint32_t expected);

    static const std::string 
    buildString(uint32_t passed, uint32_t expected);
};

class MatchSpyFinalizedDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    MatchSpyFinalizedDriverError(GET_POS);
};

class ResourceTypeMismatchDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    ResourceTypeMismatchDriverError(GET_POS, const std::string& passed, 
                                    const std::string& expected);

    static const std::string 
    buildString(const std::string& passed, const std::string& expected);
};


class AbstractMethodDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    AbstractMethodDriverError(GET_POS, const std::string& object_type, 
                              const std::string& method_name);

    static const std::string
    buildString(const std::string& object_type, const std::string& method_name);
};


class AlreadyAttachedDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    AlreadyAttachedDriverError(GET_POS, const std::string& parent_type, 
                               const std::string& child_type);

    static const std::string 
    buildString(const std::string& parent_type, const std::string& child_type);
};

XAPIAN_ERLANG_NS_END
#endif
