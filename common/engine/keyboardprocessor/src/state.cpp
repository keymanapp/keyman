/*
  Copyright:    © 2018 SIL International.
  Description:  Internal state class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE), Marc Durdin
*/

#include "state.hpp"
#include "processor.hpp"

using namespace km::kbp;

state::state(km::kbp::keyboard const & kb, km_kbp_option_item const * env)
  : _options(kb.default_options), _kb(kb)
{
  const_cast<km::kbp::abstract_processor&>(_kb.processor()).init_state(&_env);
  _options.set_default_env(_env.data());

  for (; env && env->key != nullptr; env++) {
    //assert(env->scope == KM_KBP_OPT_ENVIRONMENT); // todo do we need scope? or can we find a way to eliminate it?
    assert(_options.assign(static_cast<km_kbp_state *>(this), (km_kbp_option_scope) KM_KBP_OPT_ENVIRONMENT, env->key, env->value) != nullptr);
  }
}
