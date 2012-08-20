#include "xapian_exception.h"
#include <sstream>

/* These macroses help to transform an argument into a string. */
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)
#define REG_TYPE(CLASS) const char CLASS::TYPE[] = STR(CLASS);

// ===================================================================
// Exceptions
// ===================================================================

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

// -------------------------------------------------------------------
// DriverRuntimeError
// -------------------------------------------------------------------
DriverRuntimeError::DriverRuntimeError(
    GET_POS, const char * type, const std::string& str):
    runtime_error(str) { m_type = type; m_file = file; m_line = line; }

const char* 
DriverRuntimeError::get_type() const
{
   return m_type;
}

const char* 
DriverRuntimeError::get_file() const
{
   return m_file;
}

uint32_t 
DriverRuntimeError::get_line() const
{
   return m_line;
}


// -------------------------------------------------------------------
// MemoryAllocationDriverError
// -------------------------------------------------------------------
MemoryAllocationDriverError::MemoryAllocationDriverError(GET_POS, size_t size) :
    DriverRuntimeError(SET_POS, TYPE, buildString(size)) {}

const std::string 
MemoryAllocationDriverError::buildString(size_t size)
{
    std::stringstream ss;
    ss << "Cannot allocate " << size << " bytes.";
    return ss.str();
}


// -------------------------------------------------------------------
// BadCommandDriverError
// -------------------------------------------------------------------
BadCommandDriverError::BadCommandDriverError(GET_POS, int command_id) : 
    DriverRuntimeError(SET_POS, TYPE, buildString(command_id)) {}

const std::string 
BadCommandDriverError::buildString(int command_id)
{
    std::stringstream ss;
    ss << "Unknown command with id = " << command_id << ".";
    return ss.str();
}


// -------------------------------------------------------------------
// NotImplementedCommandDriverError
// -------------------------------------------------------------------
NotImplementedCommandDriverError::NotImplementedCommandDriverError(
        GET_POS, int command_id) : 
    DriverRuntimeError(SET_POS, TYPE, buildString(command_id)) {}

const std::string 
NotImplementedCommandDriverError::buildString(int command_id)
{
    std::stringstream ss;
    ss << "The command with id = " << command_id << " is not implemented.";
    return ss.str();
}


// -------------------------------------------------------------------
// BadArgDriverError
// -------------------------------------------------------------------
BadArgumentDriverError::BadArgumentDriverError(GET_POS) : 
    DriverRuntimeError(SET_POS, TYPE, 
        "Bad Argument was passed.") {}


// -------------------------------------------------------------------
// BadArgDriverError
// -------------------------------------------------------------------
EmptySetDriverError::EmptySetDriverError(GET_POS) : 
    DriverRuntimeError(SET_POS, TYPE, 
        "Operation is not define for empty sets.") {}


// -------------------------------------------------------------------
// OverflowDriverError
// -------------------------------------------------------------------
OverflowDriverError::OverflowDriverError(GET_POS) : 
    DriverRuntimeError(SET_POS, TYPE, "Too short binary.") {}


// -------------------------------------------------------------------
// NotWritableDatabaseError
// -------------------------------------------------------------------
NotWritableDatabaseError::NotWritableDatabaseError(GET_POS) : 
     DriverRuntimeError(SET_POS, TYPE, "The database is open as read only.") {}


// -------------------------------------------------------------------
// DbIsNotReadyDriverError
// -------------------------------------------------------------------
DbIsNotReadyDriverError::DbIsNotReadyDriverError(GET_POS) : 
    DriverRuntimeError(SET_POS, TYPE, 
        "Call xapian_server:port_open.") {}


// -------------------------------------------------------------------
// ElementNotFoundDriverError
// -------------------------------------------------------------------
ElementNotFoundDriverError::ElementNotFoundDriverError(GET_POS, uint32_t num) : 
    DriverRuntimeError(SET_POS, TYPE, buildString(num)) {}

const std::string 
ElementNotFoundDriverError::buildString(uint32_t num)
{
    std::stringstream ss;
    ss << "Element with number = " << num << " is not found.";
    return ss.str();
}


// -------------------------------------------------------------------
// GroupResourceTypeMismatchDriverError
// -------------------------------------------------------------------
GroupResourceTypeMismatchDriverError::GroupResourceTypeMismatchDriverError(
    GET_POS, uint32_t passed, uint32_t expected) : 
    DriverRuntimeError(SET_POS, TYPE, buildString(passed, expected)) {}

const std::string 
GroupResourceTypeMismatchDriverError::buildString(uint32_t passed, uint32_t expected)
{
    std::stringstream ss;
    ss << "Element with the group type = " << passed 
       << " was passed, but the " << expected << " group type was expected.";
    return ss.str();
}


// -------------------------------------------------------------------
// ResourceTypeMismatchDriverError
// -------------------------------------------------------------------
ResourceTypeMismatchDriverError::ResourceTypeMismatchDriverError(GET_POS, 
        const std::string& passed, const std::string& expected) : 
    DriverRuntimeError(SET_POS, TYPE, buildString(passed, expected)) {}

const std::string 
ResourceTypeMismatchDriverError::buildString(const std::string& passed, 
                                             const std::string& expected)
{
    std::stringstream ss;
    ss << "Resource::Controller with the type = " << passed 
       << " was passed, but the " << expected << " type was expected.";
    return ss.str();
}


// -------------------------------------------------------------------
// AbstractMethodDriverError
// -------------------------------------------------------------------
AbstractMethodDriverError::AbstractMethodDriverError(GET_POS, 
        const std::string& object_type, const std::string& method_name) : 
    DriverRuntimeError(SET_POS, TYPE, buildString(object_type, method_name)) {}

const std::string 
AbstractMethodDriverError::buildString(const std::string& object_type, 
                                       const std::string& method_name)
{
    std::stringstream ss;
    ss << "Non-implemented method " << method_name
       << " was called from " << object_type << ".";
    return ss.str();
}


// -------------------------------------------------------------------
// AlreadyAttachedDriverError
// -------------------------------------------------------------------
AlreadyAttachedDriverError::AlreadyAttachedDriverError(GET_POS, 
        const std::string& parent_type, const std::string& child_type) : 
    DriverRuntimeError(SET_POS, TYPE, buildString(parent_type, child_type)) {}

const std::string 
AlreadyAttachedDriverError::buildString(const std::string& parent_type, 
                                        const std::string& child_type)
{
    std::stringstream ss;
    ss << "Cannot attach " << child_type << " to " << parent_type << ". "
       << " The parent context already attached.";
    return ss.str();
}

// -------------------------------------------------------------------
// MatchSpyFinalizedDriverError
// -------------------------------------------------------------------
MatchSpyFinalizedDriverError::MatchSpyFinalizedDriverError(GET_POS) : 
    DriverRuntimeError(SET_POS, TYPE, 
        "Xapian::MatchSet can be used just once.") {}


REG_TYPE(MemoryAllocationDriverError)
REG_TYPE(BadCommandDriverError)
REG_TYPE(NotImplementedCommandDriverError)
REG_TYPE(BadArgumentDriverError)
REG_TYPE(EmptySetDriverError)
REG_TYPE(OverflowDriverError)
REG_TYPE(NotWritableDatabaseError)
REG_TYPE(DbIsNotReadyDriverError)
REG_TYPE(ElementNotFoundDriverError)
REG_TYPE(MatchSpyFinalizedDriverError)
REG_TYPE(GroupResourceTypeMismatchDriverError)
REG_TYPE(ResourceTypeMismatchDriverError)
REG_TYPE(AbstractMethodDriverError)
REG_TYPE(AlreadyAttachedDriverError)

XAPIAN_ERLANG_NS_END
