/*
  Copyright:    © 2018 SIL International.
  Description:  Internal context class and adaptor class for the API.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Refactored out of km_kbp_state_api.cpp
*/

#pragma once
#include <vector>

#include <keyman/keyboardprocessor.h>

#include "context.hpp"
#include "option.hpp"
#include "keyboard.hpp"


namespace km {
namespace kbp
{
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

  void push_vkeydown(km_kbp_virtual_key vk) { return _push_vkey<KM_KBP_IT_VKEYDOWN>(vk); }
  void push_vkeyup(km_kbp_virtual_key vk) { return _push_vkey<KM_KBP_IT_VKEYUP>(vk); }
  void push_vkeyshiftdown(km_kbp_virtual_key vk) { return _push_vkey<KM_KBP_IT_VSHIFTDOWN>(vk); }
  void push_vkeyshiftup(km_kbp_virtual_key vk) { return _push_vkey<KM_KBP_IT_VSHIFTUP>(vk); }

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


template<km_kbp_action_type V>
inline
void actions::_push_vkey(km_kbp_virtual_key vk) {
  assert(empty() || (!empty() && back().type != KM_KBP_IT_END));
  emplace_back(km_kbp_action_item {V, {0,}, {vk}});
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
    kbp::context          _ctxt;
    kbp::options          _options;
    kbp::keyboard const & _kb;
    std::vector<km_kbp_option_item> _env;
    kbp::actions          _actions;

public:
    state(kbp::keyboard const & kb, km_kbp_option_item const * env);
    state(state const &) = default;
    state(state const &&) = delete;

    kbp::context       &  context() noexcept        { return _ctxt; }
    kbp::context const &  context() const noexcept  { return _ctxt; }

    kbp::options       &  options() noexcept        { return _options; }
    kbp::options const &  options() const noexcept  { return _options; }

    kbp::keyboard const & keyboard() const noexcept { return _kb; }

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
