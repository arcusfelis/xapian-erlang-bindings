#ifndef XAPIAN_HELPERS_H
#define XAPIAN_HELPERS_H
#include <xapian.h>
#include <string>
#include <stdint.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/*! \name Private static helpers. */
class Helpers
{
    public:
    static Xapian::termcount
    getTermFrequency(Xapian::Document&  doc, const std::string& tname);

    static Xapian::termcount
    getExistedTermFrequency(Xapian::Document&  doc, const std::string& tname);

    
    static void
    tryRemoveValue(
        Xapian::Document& doc, Xapian::valueno slot_no, bool ignoreErrors);


    static void
    tryRemoveTerm(
        Xapian::Document& doc, const std::string& tname, bool ignoreErrors);


    static void
    tryRemovePosting(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termpos tpos, 
        Xapian::termcount wdf_inc,
        bool ignoreErrors);


    static void
    tryDecreaseWDF(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termcount wdf, 
        bool ignoreErrors);


    static void
    trySetWDF(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termcount wdf, 
        bool ignoreErrors);


    static void
    tryClearTermPositions(
        Xapian::Document& doc, 
        const std::string& tname, 
        bool ignoreErrors);


    static void
    clearTermPositions(
        Xapian::Document& doc, 
        const std::string& tname);


    static void clearTermPositions(Xapian::Document& doc);

    static bool isValueExist(Xapian::Document& doc, Xapian::valueno slot_no);

    static bool isTermExist(Xapian::Document& doc, const std::string& tname);

    static bool
    isPostingExist(
        Xapian::Document& doc, 
        const std::string& tname, 
        Xapian::termpos term_pos);
};

XAPIAN_ERLANG_NS_END
#endif
