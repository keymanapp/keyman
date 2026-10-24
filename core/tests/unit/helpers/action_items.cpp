/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Helper functions for creating action items in unit tests
 */

#include "action_items.hpp"

namespace km::tests {

//-------------------------------------------------------------------------------------
// Helper functions
//-------------------------------------------------------------------------------------

const km_core_action_item alert_action_item() {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_ALERT;
  return res;
}

const km_core_action_item bksp_action_item(uint8_t type, uintptr_t value) {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_BACK;
  res.backspace.expected_type = type;
  res.backspace.expected_value = value;
  return res;
}

const km_core_action_item caps_action_item(uint8_t capsLock) {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_CAPSLOCK;
  res.capsLock = capsLock;
  return res;
}

const km_core_action_item char_action_item(km_core_usv chr) {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_CHAR;
  res.character = chr;
  return res;
}

const km_core_action_item emit_keystroke_action_item() {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_EMIT_KEYSTROKE;
  return res;
}

const km_core_action_item persist_opt_action_item(km_core_option_item const *option) {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_PERSIST_OPT;
  res.option = option;
  return res;
}

const km_core_action_item end_action_item() {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_END;
  return res;
}

const km_core_action_item invalidate_context_action_item() {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_INVALIDATE_CONTEXT;
  return res;
}

const km_core_action_item marker_action_item(uint32_t marker) {
  km_core_action_item res = {0};
  res.type = KM_CORE_IT_MARKER;
  res.character = marker;
  return res;
}

};

