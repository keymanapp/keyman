#include "keyman_core.h"
#include <string>
#include <iostream>

#include <test_assert.h>

namespace
{

inline
bool operator==(km_core_option_item const & lhs, km_core_option_item const & rhs) {
  return lhs.scope == rhs.scope
      && std::u16string(lhs.key) == rhs.key
      && std::u16string(lhs.value) == rhs.value;
}

const char *action_item_types[] = {
  "KM_CORE_IT_END", //         = 0,  // Marks end of action items list.
  "KM_CORE_IT_CHAR", //        = 1,  // A Unicode character has been generated.
  "KM_CORE_IT_MARKER", //      = 2,  // Correlates to kmn's "deadkey" markers.
  "KM_CORE_IT_ALERT", //       = 3,  // The keyboard has triggered a alert/beep/bell.
  "KM_CORE_IT_BACK", //        = 4,  // Delete the codepoint preceding the insertion point.
  "KM_CORE_IT_PERSIST_OPT", // = 5,  // The indicated option needs to be stored.
  "KM_CORE_IT_EMIT_KEYSTROKE", // = 6,  // Emit the current keystroke to the application
  "KM_CORE_IT_INVALIDATE_CONTEXT", // = 7,
  "KM_CORE_IT_CAPSLOCK",//     = 8, Enable or disable capsLock
};

void print_action_item(const char *title, km_core_action_item const & item) {
  std::cout
    << "action_item " << title << std::endl
    << "  type:    " << action_item_types[item.type] << std::endl;

  switch(item.type) {
  case KM_CORE_IT_MARKER:
    std::cout << "  marker:  " << item.marker << std::endl;
    break;
  case KM_CORE_IT_CHAR:
    std::cout << "  char:    '" << std::u32string(1, item.character) << "' (" << item.character << ")" << std::endl;
    break;
  case KM_CORE_IT_BACK:
    std::cout << "  delete:  " <<
      (item.backspace.expected_type == KM_CORE_BT_CHAR ? "char" :
      item.backspace.expected_type == KM_CORE_BT_MARKER ? "marker" :
      "unknown") << " (" <<
      item.backspace.expected_value << ")" << std::endl;
    break;
  case KM_CORE_IT_PERSIST_OPT:
    std::cout
      << "  option:  key:   " << item.option->key << std::endl
      << "           value: " << item.option->value << std::endl
      << "           scope: " << item.option->scope << std::endl;
    break;
  case KM_CORE_IT_CAPSLOCK:
    std::cout << "  caps lock:  " <<
      (item.capsLock == 0 ? "off" :
      item.capsLock == 1 ? "on" :
      "invalid value") << " (" <<
      item.capsLock << ")" << std::endl;
    break;
  }
}

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
    print_action_item("actual", lhs);
    print_action_item("expected", rhs);
  }

  return result;
}

bool action_items(
  km_core_state const * state,
  std::initializer_list<km_core_action_item> const & expected
) {
  size_t n = 0;
  auto act = km_core_state_action_items(state, &n);

  for (auto &rhs: expected) {
    if ((int)--n < 0) {
      std::cout << "expected longer than actual" << std::endl;
      print_action_item("next expected item:", rhs);
      return false;
    }
    if (!(*act++ == rhs)) {
      return false;
    }
  }

  if(n != 0) {
    std::cout << "actual longer than expected" << std::endl;
    print_action_item("next actual item:", *act);
    return false;
  }

  return true;
}

} // namespace
