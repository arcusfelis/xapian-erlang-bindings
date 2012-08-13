#include <xapian.h>
#include "xapian_helpers.h"
#include "xapian_exception.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

/**
 * Set, decrease, increase WDF of the document term.
 * It is a tiny dirty class.
 *
 * Because Xapian does not allow set the WDF of the term directly, 
 * we use this class.
 *
 * For manipulations, we add the term position (posting), then remove it.
 *
 * NewWDF = CurrentWDF + IncWDF - DecWDF;
 *
 * The combination of IncWDF and DecWDF values produces the required WDF NewWDF:
 * add_posting(term, pos, IncWDF);
 * remove_posting(term, pos, DecWDF);
 */
class WdfManipulation
{ 
    /**
     * The term position, used for manipulation.
     * It can or cannot exist.
     */
    static const Xapian::termpos POS = 666;
    bool m_is_exist;
    Xapian::Document& m_doc;
    const std::string& m_tname;
    
    public:
    WdfManipulation(
        Xapian::Document& doc, 
        const std::string& tname)
    : m_doc(doc), m_tname(tname)
    {
        if (!Helpers::isTermExist(doc, tname))
            throw BadArgumentDriverError();

        m_is_exist = Helpers::isPostingExist(doc, tname, POS);
    }

    void
    dec_wdf(const Xapian::termcount wdf)
    {
        m_doc.add_posting(m_tname, POS, 0);
        m_doc.remove_posting(m_tname, POS, wdf);
    }

    void
    inc_wdf(const Xapian::termcount wdf)
    {
        m_doc.add_term(m_tname, wdf);
    }

    void
    set_wdf(const Xapian::termcount wdf)
    {
        const Xapian::termcount old_wdf = 
            Helpers::getTermFrequency(m_doc, m_tname);
        if (old_wdf < wdf) // inc
            inc_wdf(wdf - old_wdf);
        else 
        if (old_wdf > wdf) // dec
            dec_wdf(old_wdf - wdf);
    }

    ~WdfManipulation()
    {
        // Add the posting back
        if (m_is_exist)
            m_doc.add_posting(m_tname, POS, 0);
    }
};

XAPIAN_ERLANG_NS_END
