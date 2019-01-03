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
  : _processor(ap)
{
  for (; env && env->key != nullptr; env++) {
    //assert(env->scope == KM_KBP_OPT_ENVIRONMENT); // todo do we need scope? or can we find a way to eliminate it?
    ap.update_option(env->scope
                        ? km_kbp_option_scope(env->scope)
                        : KM_KBP_OPT_ENVIRONMENT, 
                     env->key,
                     env->value);
  }
}
