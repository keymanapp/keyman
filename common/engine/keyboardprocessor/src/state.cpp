/*
  Copyright:    © 2018 SIL International.
  Description:  Internal state class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE), Marc Durdin
*/

#include "state.hpp"
#include "processor.hpp"

using namespace km::kbp;

void actions::push_persist(option const &opt) {
  assert(empty() || back().type != KM_KBP_IT_END);
  _option_items_stack.emplace_back(opt);
  km_kbp_action_item ai = {KM_KBP_IT_PERSIST_OPT, {0,}, {0}};
  ai.option = &_option_items_stack.back();
  emplace_back(std::move(ai));
}

void actions::push_persist(option const &&opt) {
  assert(empty() || back().type != KM_KBP_IT_END);
  _option_items_stack.emplace_back(opt);
  km_kbp_action_item ai = {KM_KBP_IT_PERSIST_OPT, {0,}, {0}};
  ai.option = &_option_items_stack.back();
  emplace_back(std::move(ai));
}


state::state(km::kbp::abstract_processor & ap, km_kbp_option_item const *env)
  : _options(ap.keyboard().default_options),
    _processor(ap)
{
  ap.init_state(_env);
  _options.set_default_env(_env.data());

  for (; env && env->key != nullptr; env++) {
    //assert(env->scope == KM_KBP_OPT_ENVIRONMENT); // todo do we need scope? or can we find a way to eliminate it?
    assert(_options.assign(static_cast<km_kbp_state *>(this), (km_kbp_option_scope) KM_KBP_OPT_ENVIRONMENT, env->key, env->value) != nullptr);
  }
}
