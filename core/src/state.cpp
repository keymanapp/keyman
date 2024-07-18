/*
  Copyright:    © 2018 SIL International.
  Description:  Internal state class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE), Marc Durdin
*/

#include "state.hpp"
#include "action.hpp"
#include "processor.hpp"
#include <keyman/keyman_core_api_consts.h>
#include <cstring>

using namespace km::core;

void actions::push_persist(option const &opt) {
  assert(empty() || back().type != KM_CORE_IT_END);
  _option_items_stack.emplace_back(opt);
  km_core_action_item ai = {KM_CORE_IT_PERSIST_OPT, {0,}, {0}};
  ai.option = &_option_items_stack.back();
  emplace_back(std::move(ai));
}

void actions::push_persist(option const &&opt) {
  assert(empty() || back().type != KM_CORE_IT_END);
  _option_items_stack.emplace_back(opt);
  km_core_action_item ai = {KM_CORE_IT_PERSIST_OPT, {0,}, {0}};
  ai.option = &_option_items_stack.back();
  emplace_back(std::move(ai));
}

void actions::push_capslock(bool turnOn) {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  km_core_action_item ai = {KM_CORE_IT_CAPSLOCK, {0,}, {0}};
  ai.capsLock           = turnOn;
  emplace_back(std::move(ai));
}


state::state(km::core::abstract_processor & ap, km_core_option_item const *env)
  : _processor(ap)
{
  // The app context will never have markers, because it is an exact
  // copy of the context passed in from the application, in whatever
  // normalization form that the app is using.
  _app_ctxt.has_markers = false;
  for (; env && env->key != nullptr; env++) {
    //assert(env->scope == KM_CORE_OPT_ENVIRONMENT); // todo do we need scope? or can we find a way to eliminate it?
    ap.update_option(env->scope
                        ? km_core_option_scope(env->scope)
                        : KM_CORE_OPT_ENVIRONMENT,
                     env->key,
                     env->value);
  }
  _imx_callback = nullptr;
  _imx_object = nullptr;
  memset(const_cast<km_core_actions*>(&_action_struct), 0, sizeof(km_core_actions));
}

void state::imx_register_callback(
  km_core_keyboard_imx_platform imx_callback_fp,
  void *callback_object
) {
  assert(imx_callback_fp);
  if(!imx_callback_fp) {
    return;
  }
  _imx_callback = imx_callback_fp;
  _imx_object = callback_object;
}

void state::imx_deregister_callback() {
  _imx_callback = nullptr;
  _imx_object = nullptr;
}

void state::imx_callback(uint32_t imx_id) {
  if (_imx_callback==nullptr) {
    return;
  }
  _imx_callback(static_cast<km_core_state *>(this), imx_id, _imx_object);
}

state::~state() {
  km::core::actions_dispose(this->_action_struct);
}

void state::apply_actions_and_merge_app_context() {
  auto action_items = this->_actions.data();

  km::core::actions_dispose(this->_action_struct);

  action_item_list_to_actions_object(action_items, &this->_action_struct);

  // We keep a copy of the app context before normalization, as the
  // code_points_to_delete value can be updated by normalization, but by the
  // time we get it back from actions_normalize or
  // actions_update_app_context_nfu,  app_context has already been updated to
  // remove the necessary codepoints
  km::core::context& app_context = this->app_context();
  km::core::context const& cached_context = this->context();
  km::core::context app_context_for_deletion;
  std::copy(app_context.begin(), app_context.end(), std::back_inserter(app_context_for_deletion));

  if(this->processor().supports_normalization()) {
    // Normalize to NFC for those keyboard processors that support it
    if(!km::core::actions_normalize(&cached_context, &app_context, this->_action_struct)) {
      km::core::actions_dispose(this->_action_struct);
      return;
    }
  } else {
    // For all other keyboard processors, we just copy the cached_context to the app_context
    if(!km::core::actions_update_app_context_nfu(&cached_context, &app_context)) {
      km::core::actions_dispose(this->_action_struct);
      return;
    }
  }

  this->_action_struct.deleted_context = km::core::get_deleted_context(app_context_for_deletion, this->_action_struct.code_points_to_delete);
}