#include <iostream>
#include <stdint.h>
#include <assert.h>

#include "xapian_core.h"
#include "memory_manager.h"
#include "param_decoder.h"
#include "result_encoder.h"


// -------------------------------------------------------------------
// Globals
// -------------------------------------------------------------------

XAPIAN_ERLANG_NS_BEGIN

/* Packets consist of a header specifying the number of bytes in the 
   packet, followed by that number of bytes. The length of header can 
   be one, two, or four bytes; the order of the bytes is big-endian. 
   The header will be stripped off when the packet is returned.
*/

int is_big_endian()
{
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01020304};

    return bint.c[0] == 1; 
}

uint32_t swapByteOrder(uint32_t ui)
{
    ui = (ui >> 24) |
         ((ui<<8) & 0x00FF0000) |
         ((ui>>8) & 0x0000FF00) |
         (ui << 24);
    return ui;
}

uint32_t read_packet_length(std::istream& s)
{
    uint32_t len;
    s.read(reinterpret_cast<char*>(&len), sizeof(len));
    if (!is_big_endian()) 
        len = swapByteOrder(len);
    return len;
}

std::ostream&  write_packet_length(std::ostream& s, uint32_t len)
{
    if (!is_big_endian()) 
        len = swapByteOrder(len);
    s.write(reinterpret_cast<char*>(&len), sizeof(len));
    return s;
}



/**
 * Create global variables
 */
void run()
{
    MemoryManager mm;
    ResourceGenerator generator;
    registerUserCallbacks(generator);
    Driver drv = Driver(mm, generator);
    ResultEncoder result(mm);

    // Place to collect result, can be extended
    const size_t result_buf_len = 1024;
    char result_buf[1024];

    // Read buffer's length
    while(true)
    {
        // read len, 4 bytes
        const uint32_t len = read_packet_length(std::cin);

        // read data, len bytes
        char* buf = new char[len];
        std::cin.read(buf, len);
        
        // Handle a command
        ParamDecoder params(buf, len); 
        result.setBuffer(result_buf, result_buf_len);
        const uint32_t command = params;
        drv.handleCommand(params, result, command);
        delete[] buf;

        /* The real length of data */
        const uint32_t result_len = static_cast<uint32_t>( result.finalSize() );

        write_packet_length(std::cout, result_len);
        /* It is too long */
        if (result.isExtended())
        {
            char* large_buf = new char[result_len];
            result.finalize(large_buf);
            std::cout.write(large_buf, result_len);
            delete[] large_buf;
        } else {
            std::cout.write(result_buf, result_len);
        }
        std::cout.flush();
        result.clear();
    }
}

XAPIAN_ERLANG_NS_END


int main(void)
{
    std::cin.exceptions  ( std::istream::failbit | std::istream::badbit );
    std::cout.exceptions ( std::ostream::failbit | std::ostream::badbit );

    try
    {
        XapianErlang::run();
    } catch (std::ios_base::failure e)
    {
//      std::cerr << "Caught an exception: " << e.what() << std::endl;
//      std::cout.flush();
        return 1;
    }
    return 0;
}
