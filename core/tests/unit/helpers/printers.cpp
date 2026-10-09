/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Helpers for printing struct details to console for unit tests
 */


#include <string>
#include <iostream>
#include "utfcodec.hpp"
#include "printers.hpp"
#include "debug_items.hpp"

namespace km::tests {

  // TODO-WEB-CORE: combine with action_items.cpp

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

void print_all_action_items(km_core_state const* state) {
  size_t n = 0;
  auto act = km_core_state_action_items(state, &n);
  std::cout << "Action items:" << std::endl;
  for (size_t i = 0; i < n; i++) {
    print_action_item("", *act++);
  }
  std::cout << "---------------" << std::endl;
}

};
