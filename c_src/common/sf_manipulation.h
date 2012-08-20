#include <xapian.h>
#include "xapian_helpers.h"
#include "xapian_exception.h"

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

class SpellingFreqManipulation
{ 
    /**
     * The term position, used for manipulation.
     * It can or cannot exist.
     */
    bool m_is_exist;
    Xapian::WritableDatabase& m_wdb;
    const std::string& m_tname;
    
    public:
    SpellingFreqManipulation(
        Xapian::WritableDatabase& wdb, 
        const std::string& tname)
    : m_wdb(wdb), m_tname(tname)
    {
        if (!Helpers::isSpellingExist(wdb, tname))
            throw BadArgumentDriverError(POS);
    }
    Xapian::termcount
    current_frequency()
    {
        return Helpers::getSpellingFrequency(m_wdb, m_tname);
    }

    void
    dec_frequency(const Xapian::termcount frequency)
    {
       m_wdb.remove_spelling(m_tname, frequency);
    }

    void
    inc_frequency(const Xapian::termcount frequency)
    {
        m_wdb.add_spelling(m_tname, frequency);
    }

    void
    set_frequency(const Xapian::termcount frequency)
    {
        const Xapian::termcount old_frequency = current_frequency();
        if (old_frequency < frequency) // inc
            inc_frequency(frequency - old_frequency);
        else 
        if (old_frequency > frequency) // dec
            dec_frequency(old_frequency - frequency);
    }

    void
    clear_frequency()
    {
        set_frequency(0);
    }
};

XAPIAN_ERLANG_NS_END
