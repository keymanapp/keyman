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

#include "keyman_core.h"

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
    core::context              _app_ctxt;
    core::abstract_processor & _processor;
    core::actions              _actions;
    km_core_actions            _action_struct;
    core::debug_items          _debug_items;
    km_core_keyboard_imx_platform _imx_callback;
    void *_imx_object;

public:
    state(core::abstract_processor & kb, km_core_option_item const *env);

    state(state const &) = default;
    state(state const &&) = delete;

    ~state();

    core::context       &  context() noexcept            { return _ctxt; }
    core::context const &  context() const noexcept      { return _ctxt; }

    core::context       &  app_context() noexcept            { return _app_ctxt; }
    core::context const &  app_context() const noexcept      { return _app_ctxt; }

    core::abstract_processor const & processor() const noexcept { return _processor; }
    core::abstract_processor &       processor() noexcept { return _processor; }

    core::actions        & actions() noexcept        { return _actions; }
    core::actions const  & actions() const noexcept  { return _actions; }

    km_core_actions       & action_struct() noexcept       { return _action_struct; }
    km_core_actions const & action_struct() const noexcept { return _action_struct; }

    core::debug_items        & debug_items() noexcept        { return _debug_items; }
    core::debug_items const  & debug_items() const noexcept  { return _debug_items; }

    void imx_register_callback(km_core_keyboard_imx_platform imx_callback, void *callback_object);

    void imx_deregister_callback();

    void imx_callback(uint32_t imx_id);

    // This is intended to be used to take the actions given in the actions
    // parameter, and load them into the _actions member of this class. Used by
    // keyboard processors to set the output actions, and is a long-term
    // replacement for the actions()::push_*() functions. Note that the
    // km_core_actions struct does not include information about markers, which
    // are maintained separately in the _ctxt member of this class, and the
    // corresponding marker-backspace action items are never used here.
    bool set_actions(
      km_core_actions const &actions
    );
    void apply_actions_and_merge_app_context();
  };
} // namespace core
} // namespace km

struct km_core_state : public km::core::state
{
  template<typename... Args>
  km_core_state(Args&&... args) : km::core::state(std::forward<Args>(args)...)
  {}
};


/**
 * Evaluate the state and vkey used.
 * Determine whether the context should be invalidated.
 * @param  state          A pointer to the opaque state object.
 * @param  vk             A virtual key that was processed.
 * @param  modifier_state The combinations of modifier keys set at the time key
 *                        `vk` was pressed, bitmask from the
 *                        km_core_modifier_state enum.
 * @param  is_key_down    1 if it was a key-down event
 * @param  event_flags    Event level flags, see km_core_event_flags
 * @return true if this is a state which should clear the context
 */
bool
state_should_invalidate_context(km_core_state *state,
                     km_core_virtual_key vk,
                     uint16_t modifier_state,
                     uint8_t is_key_down,
                     uint16_t event_flags);
