#include "qlc_table.h"
#include "spy_ctrl.h"
#include "termiter_gen.h"
#include "termiter_doc_gen.h"
#include "user_resources.h"
#include "object_register.h"
#include "enquire_ctrl.h"
#include "key_maker_ctrl.h"
#include "stopper_ctrl.h"
#include "stem_ctrl.h"
#include "val_range_ctrl.h"
#include "match_decider_ctrl.h"
#include "expand_decider_ctrl.h"
#include "query_parser_ctrl.h"
#include <xapian.h>

#include "xapian_config.h"
XAPIAN_ERLANG_NS_BEGIN

template class ObjectRegister<Xapian::Document>;
template class ObjectRegister<KeyMakerController>;
template class ObjectRegister<EnquireController>;
template class ObjectRegister<Xapian::MSet>;
template class ObjectRegister<QlcTable>;
template class ObjectRegister<Xapian::Weight>;
template class ObjectRegister<Xapian::Query>;
template class ObjectRegister<MatchDeciderController>;
template class ObjectRegister<ExpandDeciderController>;
template class ObjectRegister<SpyController>;
template class ObjectRegister<ValueRangeProcessorController>;
template class ObjectRegister<QueryParserController>;
template class ObjectRegister<StopperController>;
template class ObjectRegister<StemController>;

// used in user_resources
template class ObjectRegister<UserResource>;

XAPIAN_ERLANG_NS_END
