#ifndef XAPIAN_EXCEPTION_H
#define XAPIAN_EXCEPTION_H

#include <stdexcept>
#include <xapian.h>
#include <stdint.h>
#include <cstring>
#include <string>

// -------------------------------------------------------------------
// Exceptions
// -------------------------------------------------------------------

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

class DbAlreadyOpenedDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    DbAlreadyOpenedDriverError();
};

class ElementNotFoundDriverError: public DriverRuntimeError
{
    static const char TYPE[];

    public:
    ElementNotFoundDriverError(uint32_t num);

    static const std::string 
    buildString(uint32_t num);
};

#endif
