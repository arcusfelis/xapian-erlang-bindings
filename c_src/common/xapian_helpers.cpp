#include "xapian_helpers.h"
#include "xapian_exception.h"
#include "wdf_manipulation.h"

#include <assert.h>
#include <cstdlib>


XAPIAN_ERLANG_NS_BEGIN

// Helper for getting frequency (wdf) of the passed term.
// If term is not found, then return 0.
Xapian::termcount
Helpers::getTermFrequency(
    Xapian::Document& doc, const std::string& tname)
{
    Xapian::TermIterator 
        iter = doc.termlist_begin(),
        end = doc.termlist_end();

    if (iter == end)
        return 0;
    
    iter.skip_to(tname);
    // Current element is a term and its value is tname.
    return ((iter != end) && (tname == (*iter))) 
        ? iter.get_wdf() : 0;
}


// If term is not found, then BadArgumentDriverError will be thrown.
Xapian::termcount
Helpers::getExistedTermFrequency(
    Xapian::Document& doc, const std::string& tname)
{
    Xapian::TermIterator 
        iter = doc.termlist_begin(),
        end = doc.termlist_end();

    if (iter == end)
        throw BadArgumentDriverError();

    iter.skip_to(tname);

    if ((iter == end) || (tname != (*iter))) 
            throw BadArgumentDriverError();

    return iter.get_wdf();
}


void
Helpers::tryRemoveValue(
    Xapian::Document& doc, Xapian::valueno slot_no, bool ignoreErrors)
{
    if (ignoreErrors)
        try {
            doc.remove_value(slot_no);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        doc.remove_value(slot_no);
}


void
Helpers::tryRemoveTerm(
    Xapian::Document& doc, const std::string& tname, bool ignoreErrors)
{
    if (ignoreErrors)
        try {
            doc.remove_term(tname);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        doc.remove_term(tname);
}


void
Helpers::tryRemovePosting(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termpos tpos, 
    Xapian::termcount wdf_inc,
    bool ignoreErrors)
{
    if (ignoreErrors)
        try {
            doc.remove_posting(tname, tpos, wdf_inc);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        doc.remove_posting(tname, tpos, wdf_inc);
}



void
Helpers::tryDecreaseWDF(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termcount wdf, 
    bool ignoreErrors)
{
    if (ignoreErrors)
    {
        try 
        {
            WdfManipulation hpos(doc, tname);
            hpos.dec_wdf(wdf);
        } 
        catch (Xapian::InvalidArgumentError& e) {}
        catch (BadCommandDriverError& e) {}
    }
    else
    {
        WdfManipulation hpos(doc, tname);
        hpos.dec_wdf(wdf);
    }
}


void
Helpers::trySetWDF(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termcount wdf, 
    bool ignoreErrors)
{
    if (ignoreErrors)
    {
        try 
        {
            WdfManipulation hpos(doc, tname);
            hpos.set_wdf(wdf);
        } 
        catch (Xapian::InvalidArgumentError& e) {}
        catch (BadCommandDriverError& e) {}
    }
    else
    {
        WdfManipulation hpos(doc, tname);
        hpos.set_wdf(wdf);
    }
}


void
Helpers::tryClearTermPositions(
    Xapian::Document& doc, 
    const std::string& tname, 
    bool ignoreErrors)
{
    if (ignoreErrors)
        try {
            clearTermPositions(doc, tname);
        } catch (Xapian::InvalidArgumentError& e) {}
    else
        clearTermPositions(doc, tname);
}


void
Helpers::clearTermPositions(
    Xapian::Document& doc, 
    const std::string& tname)
{
    Xapian::termcount old_wdf = getExistedTermFrequency(doc, tname);
    doc.remove_term(tname);
    doc.add_term(tname, old_wdf);
}


void
Helpers::clearTermPositions(
    Xapian::Document& doc)
{
    typedef std::map<std::string, Xapian::termcount> map_type;
    map_type map;

    for (Xapian::TermIterator i = doc.termlist_begin(), 
            e = doc.termlist_end(); i != e; i++)
    {
        map[*i] = i.get_wdf();
    }
    doc.clear_terms();
    for (map_type::iterator i = map.begin(); i != map.end(); i++)
    {
        doc.add_term(i->first, i->second);
    }
}


bool
Helpers::isValueExist(Xapian::Document& doc, Xapian::valueno slot_no)
{
    Xapian::ValueIterator    
        iter = doc.values_begin(),
        end = doc.values_end();

    if (iter == end)
        return 0;

    iter.skip_to(slot_no);
    return (iter != end) && (iter.get_valueno() == slot_no);
}


bool
Helpers::isTermExist(Xapian::Document& doc, const std::string& tname)
{
    Xapian::TermIterator    
        iter = doc.termlist_begin(),
        end = doc.termlist_end();

    if (iter == end)
        return false;

    iter.skip_to(tname);
    return (iter != end) && ((*iter) == tname);
}


bool
Helpers::isPostingExist(
    Xapian::Document& doc, 
    const std::string& tname, 
    Xapian::termpos term_pos)
{
    Xapian::TermIterator    
        titer = doc.termlist_begin(),
        tend = doc.termlist_end();

    if (titer == tend)
        return false;

    titer.skip_to(tname);

    if ((titer != tend) && ((*titer) == tname))
    {
        // term exist
        Xapian::PositionIterator    
            piter = titer.positionlist_begin(), 
            pend = titer.positionlist_end();

        if (piter == pend)
            return false;

        piter.skip_to(term_pos);
        return (piter != pend) && ((*piter) == term_pos);
    }
    return false;
}

XAPIAN_ERLANG_NS_END
