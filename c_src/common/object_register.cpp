#include "qlc_table.h"
#include "spy_ctrl.h"
#include "termiter_gen.h"
#include "termiter_doc_gen.h"
#include "user_resources.h"
#include "object_register.h"
#include "enquire_ctrl.h"
#include "key_maker_ctrl.h"
#include <xapian.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

template class ObjectRegister<Xapian::Document>;
template class ObjectRegister<KeyMakerController>;
template class ObjectRegister<EnquireController>;
template class ObjectRegister<Xapian::MSet>;
template class ObjectRegister<QlcTable>;
template class ObjectRegister<const Xapian::Weight>;
template class ObjectRegister<const Xapian::Query>;
template class ObjectRegister<const Xapian::MatchDecider>;
template class ObjectRegister<const Xapian::Stem>;
template class ObjectRegister<const Xapian::ExpandDecider>;
template class ObjectRegister<SpyController>;
template class ObjectRegister<Xapian::ValueRangeProcessor>;

// used in user_resources
template class ObjectRegister<UserResource>;

XAPIAN_ERLANG_NS_END
