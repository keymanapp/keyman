/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin
  History:      23 Oct 2023 - MCD - Initial implementation.
*/
#include <string>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"

#include <test_assert.h>
#include "../emscripten_filesystem.h"

const km_core_action_item alert_action_item();
const km_core_action_item bksp_action_item(uint8_t type, uintptr_t value);
const km_core_action_item caps_action_item(uint8_t capsLock);
const km_core_action_item char_action_item(km_core_usv chr);
const km_core_action_item emit_keystroke_action_item();
const km_core_action_item persist_opt_action_item(km_core_option_item const *option);
const km_core_action_item end_action_item();
const km_core_action_item invalidate_context_action_item();
const km_core_action_item marker_action_item(uint32_t marker);

//-------------------------------------------------------------------------------------

void test_two_backspaces() {
  const km_core_action_item action_items[] = {
    char_action_item('D'),
    bksp_action_item(KM_CORE_BT_CHAR, 'D'),
    bksp_action_item(KM_CORE_BT_CHAR, 'E'),
    end_action_item()
  };

  km_core_actions const *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 1);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);

  assert(actions->do_alert == false);
  assert(actions->emit_keystroke == false);
  assert(actions->new_caps_lock_state == -1);
  assert(actions->deleted_context == nullptr);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_marker_text_interleaved() {
  const km_core_action_item action_items[] = {
    char_action_item('A'),
    marker_action_item(1),
    char_action_item('B'),
    marker_action_item(2),
    char_action_item('C'),
    bksp_action_item(KM_CORE_BT_CHAR, 'C'),
    bksp_action_item(KM_CORE_BT_MARKER, 2),
    char_action_item('D'),
    end_action_item()
  };

  km_core_actions const *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"ABD");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == false);
  assert(actions->emit_keystroke == false);
  assert(actions->new_caps_lock_state == -1);
  assert(actions->deleted_context == nullptr);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_alert() {
  const km_core_action_item action_items[] = {
    alert_action_item(),
    end_action_item()
  };

  km_core_actions const *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == KM_CORE_TRUE);
  assert(actions->emit_keystroke == KM_CORE_FALSE);
  assert(actions->new_caps_lock_state == KM_CORE_CAPS_UNCHANGED);
  assert(actions->deleted_context == nullptr);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_emit_keystroke() {
  const km_core_action_item action_items[] = {
    emit_keystroke_action_item(),
    end_action_item()
  };

  km_core_actions const *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == KM_CORE_FALSE);
  assert(actions->emit_keystroke == KM_CORE_TRUE);
  assert(actions->new_caps_lock_state == KM_CORE_CAPS_UNCHANGED);
  assert(actions->deleted_context == nullptr);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_invalidate_context() {
  // note, this generates a no-op
  const km_core_action_item action_items[] = {
    invalidate_context_action_item(),
    end_action_item()
  };

  km_core_actions const *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == KM_CORE_FALSE);
  assert(actions->emit_keystroke == KM_CORE_FALSE);
  assert(actions->new_caps_lock_state == KM_CORE_CAPS_UNCHANGED);
  assert(actions->deleted_context == nullptr);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_persist_opt() {
  const km_core_option_item option = {
    u"key",
    u"value",
    KM_CORE_OPT_KEYBOARD
  };

  const km_core_action_item action_items[] = {
    persist_opt_action_item(&option),
    end_action_item()
  };

  km_core_actions const *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(std::u16string(actions->persist_options[0].key) == u"key");
  assert(std::u16string(actions->persist_options[0].value) == u"value");
  assert(actions->persist_options[0].scope == KM_CORE_OPT_KEYBOARD);

  // verify that data is copied
  assert(actions->persist_options[0].key != option.key);
  assert(actions->persist_options[0].value != option.value);

  // verify that we have a KM_CORE_OPTIONS_END term
  assert(actions->persist_options[1].key == nullptr);
  assert(actions->persist_options[1].value == nullptr);
  assert(actions->persist_options[1].scope == KM_CORE_OPT_UNKNOWN);

  assert(actions->do_alert == KM_CORE_FALSE);
  assert(actions->emit_keystroke == KM_CORE_FALSE);
  assert(actions->new_caps_lock_state == KM_CORE_CAPS_UNCHANGED);
  assert(actions->deleted_context == nullptr);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str = "\
action_api [--color] <SOURCE_PATH>\n\
\n\
  --color         Force color output\n\
  SOURCE_PATH     Path where debug_api.cpp is found; kmx files are\n\
                  located relative to this path.\n";

int error_args() {
  std::cerr << "debug_api: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {

  if(argc < 2) {
    return error_args();
  }

  auto arg_color = std::string(argv[1]) == "--color";
  if(arg_color && argc < 3) {
    return error_args();
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

  km_core_actions act = {0};

  std::cout << "sizeof(km_core_actions): " << sizeof(km_core_actions) << std::endl;
  std::cout << "&km_core_actions.code_points_to_delete: " << ((intptr_t)(&act.code_points_to_delete)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.output: " << ((intptr_t)(&act.output)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.persist_options: " << ((intptr_t)(&act.persist_options)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.do_alert: " << ((intptr_t)(&act.do_alert)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.emit_keystroke: " << ((intptr_t)(&act.emit_keystroke)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.new_caps_lock_state: " << ((intptr_t)(&act.new_caps_lock_state)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.deleted_context: " << ((intptr_t)(&act.deleted_context)-(intptr_t)(&act)) << std::endl;

  // actions
  test_two_backspaces();
  test_marker_text_interleaved();
  test_alert();
  test_emit_keystroke();
  test_invalidate_context();
  test_persist_opt();
}

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
