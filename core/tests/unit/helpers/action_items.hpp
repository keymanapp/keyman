#pragma once

#include <string>
#include <iostream>
#include "keyman_core.h"
#include "printers.hpp"

namespace km::tests {

inline
bool operator==(km_core_option_item const & lhs, km_core_option_item const & rhs) {
  return lhs.scope == rhs.scope
      && std::u16string(lhs.key) == rhs.key
      && std::u16string(lhs.value) == rhs.value;
}

inline
bool operator==(
  km_core_action_item const & lhs,
  km_core_action_item const & rhs
) {
  auto result = (lhs.type == rhs.type);
  if(result) {
    switch(lhs.type) {
      case KM_CORE_IT_END:                break;
      case KM_CORE_IT_CHAR:               result = lhs.character == rhs.character; break;
      case KM_CORE_IT_MARKER:             result = lhs.marker == rhs.marker; break;
      case KM_CORE_IT_ALERT:              break;
      case KM_CORE_IT_BACK:               result = lhs.backspace.expected_type == rhs.backspace.expected_type &&
                                                  lhs.backspace.expected_value == rhs.backspace.expected_value; break;
      case KM_CORE_IT_PERSIST_OPT:        result = *lhs.option == *rhs.option; break;
      case KM_CORE_IT_EMIT_KEYSTROKE:     break;
      case KM_CORE_IT_CAPSLOCK:           result = lhs.capsLock == rhs.capsLock; break;
      case KM_CORE_IT_INVALIDATE_CONTEXT: break;
      default: std::cout << "unexpected type" << std::endl; return false; //
    }
  }

  if(!result) {
    km::tests::print_action_item("actual", lhs);
    km::tests::print_action_item("expected", rhs);
  }

  return result;
}

// TODO-WEB-CORE: avoid cout; instead use google test reporting once all tests are google test
inline
bool action_items(
  km_core_state const * state,
  std::initializer_list<km_core_action_item> const & expected
) {
  size_t n = 0;
  auto act = km_core_state_action_items(state, &n);

  for (auto &rhs: expected) {
    if ((int)--n < 0) {
      std::cout << "expected longer than actual" << std::endl;
      km::tests::print_action_item("next expected item:", rhs);
      return false;
    }
    if (!(*act++ == rhs)) {
      return false;
    }
  }

  if(n != 0) {
    std::cout << "actual longer than expected" << std::endl;
    km::tests::print_action_item("next actual item:", *act);
    return false;
  }

  return true;
}


const km_core_action_item alert_action_item();
const km_core_action_item bksp_action_item(uint8_t type, uintptr_t value);
const km_core_action_item caps_action_item(uint8_t capsLock);
const km_core_action_item char_action_item(km_core_usv chr);
const km_core_action_item emit_keystroke_action_item();
const km_core_action_item persist_opt_action_item(km_core_option_item const *option);
const km_core_action_item end_action_item();
const km_core_action_item invalidate_context_action_item();
const km_core_action_item marker_action_item(uint32_t marker);


} // namespace
