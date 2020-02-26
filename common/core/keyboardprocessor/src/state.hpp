/*
  Copyright:    © 2018 SIL International.
  Description:  Internal context class and adaptor class for the API.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Refactored out of km_kbp_state_api.cpp
*/

#pragma once

#include <cassert>
#include <vector>

#include <keyman/keyboardprocessor.h>

#include "context.hpp"
#include "option.hpp"


namespace km {
namespace kbp
{
//Forward declarations
class abstract_processor;

using action = km_kbp_action_item;

class actions : public std::vector<action>
{
  std::vector<option> _option_items_stack;

  template<km_kbp_action_type V>
  void _push_vkey(km_kbp_virtual_key);

public:
  template<typename... Args>
  actions(Args&&... args);

  void push_character(km_kbp_usv usv);
  void push_marker(uintptr_t marker);
  void push_alert();
  void push_backspace();
  void push_persist(option const &);
  void push_persist(option const &&);
  void push_emit_keystroke(km_kbp_virtual_key vk=0);
  void push_invalidate_context();

  void commit();
  void clear();
};


template<typename... Args>
actions::actions(Args&&... args)
: std::vector<action>(std::forward<Args>(args)...)
{
  // Ensure the action items list is terminated in case the client calls
  // km_kbp_state_action_items before they call process_event.
  commit();
}

inline
void actions::push_character(km_kbp_usv usv) {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item{ KM_KBP_IT_CHAR, {0,}, {usv} });
}


inline
void actions::push_marker(uintptr_t marker) {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item {KM_KBP_IT_MARKER, {0,}, {marker}});
}


inline
void actions::push_alert() {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item {KM_KBP_IT_ALERT, {0,}, {0}});
}


inline
void actions::push_backspace() {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item {KM_KBP_IT_BACK, {0,}, {0}});
}


inline
void actions::push_emit_keystroke(km_kbp_virtual_key vk) {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item {KM_KBP_IT_EMIT_KEYSTROKE, {0,}, {vk}});
}


inline
void actions::push_invalidate_context() {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item {KM_KBP_IT_INVALIDATE_CONTEXT, {0,}, {0}});
}


inline
void actions::commit() {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item {KM_KBP_IT_END, {0,}, {0}});
}


inline
void actions::clear() {
  std::vector<action>::clear();
  _option_items_stack.clear();
}



class state
{
protected:
    kbp::context              _ctxt;
    kbp::abstract_processor & _processor;
    kbp::actions              _actions;

public:
    state(kbp::abstract_processor & kb, km_kbp_option_item const *env);

    state(state const &) = default;
    state(state const &&) = delete;

    kbp::context       &  context() noexcept            { return _ctxt; }
    kbp::context const &  context() const noexcept      { return _ctxt; }

    kbp::abstract_processor const & processor() const noexcept { return _processor; }
    kbp::abstract_processor &       processor() noexcept { return _processor; }

    kbp::actions        & actions() noexcept        { return _actions; }
    kbp::actions const  & actions() const noexcept  { return _actions; }
};

} // namespace kbp
} // namespace km

struct km_kbp_state : public km::kbp::state
{
  template<typename... Args>
  km_kbp_state(Args&&... args) : km::kbp::state(std::forward<Args>(args)...)
  {}
};
