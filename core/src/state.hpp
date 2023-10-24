/*
  Copyright:    © 2018 SIL International.
  Description:  Internal context class and adaptor class for the API.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Refactored out of km_core_state_api.cpp
*/

#pragma once

#include <cassert>
#include <vector>

#include <keyman/keyman_core_api.h>

#include "context.hpp"
#include "option.hpp"
#include "debug.hpp"

namespace km {
namespace core
{
//Forward declarations
class abstract_processor;

using action = km_core_action_item;

class actions : public std::vector<action>
{
  std::vector<option> _option_items_stack;

  template<km_core_action_type V>
  void _push_vkey(km_core_virtual_key);

public:
  template<typename... Args>
  actions(Args&&... args);

  void push_character(km_core_usv usv);
  void push_marker(uint32_t marker);
  void push_alert();
  void push_backspace(km_core_backspace_type expected_type, uintptr_t expected_value = 0);
  void push_persist(option const &);
  void push_persist(option const &&);
  void push_emit_keystroke(km_core_virtual_key vk=0);
  void push_capslock(bool);
  void push_invalidate_context();

  void commit();
  void clear();
};


template<typename... Args>
actions::actions(Args&&... args)
: std::vector<action>(std::forward<Args>(args)...)
{
  // Ensure the action items list is terminated in case the client calls
  // km_core_state_action_items before they call process_event.
  commit();
}

inline
void actions::push_character(km_core_usv usv) {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  emplace_back(km_core_action_item{ KM_CORE_IT_CHAR, {0,}, {usv} });
}


inline
void actions::push_marker(uint32_t marker) {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  emplace_back(km_core_action_item {KM_CORE_IT_MARKER, {0,}, {marker}});
}


inline
void actions::push_alert() {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  emplace_back(km_core_action_item {KM_CORE_IT_ALERT, {0,}, {0}});
}


inline
void actions::push_backspace(km_core_backspace_type expected_type, uintptr_t expected_value) {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  km_core_action_item item = {KM_CORE_IT_BACK, {}, {}};
  item.backspace.expected_type = expected_type;
  item.backspace.expected_value = expected_value;
  emplace_back(item);
}


inline
void actions::push_emit_keystroke(km_core_virtual_key vk) {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  emplace_back(km_core_action_item {KM_CORE_IT_EMIT_KEYSTROKE, {0,}, {vk}});
}

inline
void actions::push_invalidate_context() {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  emplace_back(km_core_action_item {KM_CORE_IT_INVALIDATE_CONTEXT, {0,}, {0}});
}


inline
void actions::commit() {
  assert(empty() || (!empty() && back().type != KM_CORE_IT_END));
  emplace_back(km_core_action_item {KM_CORE_IT_END, {0,}, {0}});
}


inline
void actions::clear() {
  std::vector<action>::clear();
  _option_items_stack.clear();
}



class state
{
protected:
    core::context              _ctxt;
    core::abstract_processor & _processor;
    core::actions              _actions;
    core::debug_items          _debug_items;
    km_core_keyboard_imx_platform _imx_callback;
    void *_imx_object;

public:
    state(core::abstract_processor & kb, km_core_option_item const *env);

    state(state const &) = default;
    state(state const &&) = delete;

    core::context       &  context() noexcept            { return _ctxt; }
    core::context const &  context() const noexcept      { return _ctxt; }

    core::abstract_processor const & processor() const noexcept { return _processor; }
    core::abstract_processor &       processor() noexcept { return _processor; }

    core::actions        & actions() noexcept        { return _actions; }
    core::actions const  & actions() const noexcept  { return _actions; }

    core::debug_items        & debug_items() noexcept        { return _debug_items; }
    core::debug_items const  & debug_items() const noexcept  { return _debug_items; }

    void imx_register_callback(km_core_keyboard_imx_platform imx_callback, void *callback_object);

    void imx_deregister_callback();

    void imx_callback(uint32_t imx_id);
};

} // namespace core
} // namespace km

struct km_core_state : public km::core::state
{
  template<typename... Args>
  km_core_state(Args&&... args) : km::core::state(std::forward<Args>(args)...)
  {}
};
