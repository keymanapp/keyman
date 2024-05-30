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

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

km_core_keyboard * test_kb = nullptr;
km_core_state * test_state = nullptr;
km_core_context_item * citems = nullptr;
km_core_usv test_empty_output[] = {0};
std::string arg_path;

void teardown() {
  if(citems) {
    km_core_context_items_dispose(citems);
    citems = nullptr;
  }
  if(test_state) {
    km_core_state_dispose(test_state);
    test_state = nullptr;
  }
  if(test_kb) {
    km_core_keyboard_dispose(test_kb);
    test_kb = nullptr;
  }
}

void setup(const char *keyboard, const km_core_cu* context) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, keyboard);
  try_status(km_core_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(context_items_from_utf16(context, &citems));
  try_status(km_core_context_set(km_core_state_context(test_state), citems));
  try_status(km_core_context_set(km_core_state_app_context(test_state), citems));
}

void run_test(km_core_action_item const * action_items, const km_core_actions &actions) {
  setup("k_000___null_keyboard.kmx", u"");
  test_state->set_actions(actions);
  auto set_actions = test_state->actions();

  int n = 0;
  for(auto act = set_actions.begin(); act != set_actions.end(); act++, n++) {
    assert(act->type == action_items[n].type);
    // TODO: all other fields
    switch(act->type) {
      case KM_CORE_IT_ALERT:
        break;
      case KM_CORE_IT_BACK:
        assert(act->backspace.expected_type == action_items[n].backspace.expected_type);
        assert(act->backspace.expected_value == action_items[n].backspace.expected_value);
        break;
      case KM_CORE_IT_CAPSLOCK:
        assert(act->capsLock == action_items[n].capsLock);
        break;
      case KM_CORE_IT_CHAR:
        assert(act->character == action_items[n].character);
        break;
      case KM_CORE_IT_EMIT_KEYSTROKE:
        break;
      case KM_CORE_IT_END:
        break;
      case KM_CORE_IT_INVALIDATE_CONTEXT:
        break;
      case KM_CORE_IT_MARKER:
        assert(act->marker == action_items[n].marker);
        break;
      case KM_CORE_IT_PERSIST_OPT:
        assert(act->option->scope == action_items[n].option->scope);
        assert(std::u16string(act->option->key) == action_items[n].option->key);
        assert(std::u16string(act->option->value) == action_items[n].option->value);
        break;
      default:
        // Invalid action type
        assert(false);
    }
  }
}

//-------------------------------------------------------------------------------------
// Set Action tests
//-------------------------------------------------------------------------------------

void test_two_backspaces() {
  puts("test_two_backspaces");
  const km_core_action_item action_items[] = {
    bksp_action_item(KM_CORE_BT_CHAR, 0),
    bksp_action_item(KM_CORE_BT_CHAR, 0),
    end_action_item()
  };

  const km_core_actions actions = {
    2, // unsigned int code_points_to_delete;
    test_empty_output, // km_core_usv* output;
    test_env_opts, // km_core_option_item* persist_options;
    KM_CORE_FALSE, // km_core_bool do_alert;
    KM_CORE_FALSE, // km_core_bool emit_keystroke;
    KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
    nullptr // km_core_usv* deleted_context;
  };

  run_test(action_items, actions);
}

void test_character() {
  puts("test_character");
  const km_core_action_item action_items[] = {
    char_action_item(u'A'),
    end_action_item()
  };

  const km_core_actions actions = {
    0, // unsigned int code_points_to_delete;
    (km_core_usv*) U"A", // km_core_usv* output;
    test_env_opts, // km_core_option_item* persist_options;
    KM_CORE_FALSE, // km_core_bool do_alert;
    KM_CORE_FALSE, // km_core_bool emit_keystroke;
    KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
    nullptr // km_core_usv* deleted_context;
  };

  run_test(action_items, actions);
}

//-------------------------------------------------------------------------------------

void test_alert() {
  puts("test_alert");
  const km_core_action_item action_items[] = {
    alert_action_item(),
    end_action_item()
  };

  const km_core_actions actions = {
    0, // unsigned int code_points_to_delete;
    test_empty_output, // km_core_usv* output;
    test_env_opts, // km_core_option_item* persist_options;
    KM_CORE_TRUE, // km_core_bool do_alert;
    KM_CORE_FALSE, // km_core_bool emit_keystroke;
    KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
    nullptr // km_core_usv* deleted_context;
  };

  run_test(action_items, actions);
}

//-------------------------------------------------------------------------------------

void test_emit_keystroke() {
  puts("test_emit_keystroke");
  const km_core_action_item action_items[] = {
    emit_keystroke_action_item(),
    end_action_item()
  };

  const km_core_actions actions = {
    0, // unsigned int code_points_to_delete;
    test_empty_output, // km_core_usv* output;
    test_env_opts, // km_core_option_item* persist_options;
    KM_CORE_FALSE, // km_core_bool do_alert;
    KM_CORE_TRUE, // km_core_bool emit_keystroke;
    KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
    nullptr // km_core_usv* deleted_context;
  };

  run_test(action_items, actions);
}

//-------------------------------------------------------------------------------------

void test_invalidate_context() {
  puts("test_invalidate_context");
  // note, this generates a no-op, DELETE?
  const km_core_action_item action_items[] = {
    // invalidate_context_action_item(),
    end_action_item()
  };

  const km_core_actions actions = {
    0, // unsigned int code_points_to_delete;
    test_empty_output, // km_core_usv* output;
    test_env_opts, // km_core_option_item* persist_options;
    KM_CORE_FALSE, // km_core_bool do_alert;
    KM_CORE_FALSE, // km_core_bool emit_keystroke;
    KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
    nullptr // km_core_usv* deleted_context;
  };

  run_test(action_items, actions);
}

//-------------------------------------------------------------------------------------

void test_persist_opt() {
  puts("test_persist_opt");
  const km_core_option_item option = {
    u"key",
    u"value",
    KM_CORE_OPT_KEYBOARD
  };

  /*TODO: const*/ km_core_option_item options[] = {{
    u"key",
    u"value",
    KM_CORE_OPT_KEYBOARD
  },
    KM_CORE_OPTIONS_END
  };

  const km_core_action_item action_items[] = {
    persist_opt_action_item(&option),
    end_action_item()
  };

  const km_core_actions actions = {
    0, // unsigned int code_points_to_delete;
    test_empty_output, // km_core_usv* output;
    options, // km_core_option_item* persist_options;
    KM_CORE_FALSE, // km_core_bool do_alert;
    KM_CORE_FALSE, // km_core_bool emit_keystroke;
    KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
    nullptr // km_core_usv* deleted_context;
  };

  run_test(action_items, actions);
}

//-------------------------------------------------------------------------------------

void test_caps_lock() {
  puts("test_caps_lock");

  const km_core_action_item action_items[] = {
    caps_action_item(KM_CORE_CAPS_ON),
    // invalidate_context_action_item(),
    end_action_item()
  };

  const km_core_actions actions = {
    0, // unsigned int code_points_to_delete;
    test_empty_output, // km_core_usv* output;
    test_env_opts, // km_core_option_item* persist_options;
    KM_CORE_FALSE, // km_core_bool do_alert;
    KM_CORE_FALSE, // km_core_bool emit_keystroke;
    KM_CORE_CAPS_ON, // new_caps_lock_state;
    nullptr // km_core_usv* deleted_context;
  };

  run_test(action_items, actions);
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str = "\
action_set_api [--color] <SOURCE_PATH>\n\
\n\
  --color         Force color output\n\
  SOURCE_PATH     Path where action_set_api.cpp is found; kmx files are\n\
                  located relative to this path.\n";

int error_args() {
  std::cerr << "action_set_api: Invalid arguments." << std::endl;
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

#ifdef __EMSCRIPTEN__
  arg_path = get_wasm_file_path(argv[arg_color ? 2 : 1]);
#else
  arg_path = argv[arg_color ? 2 : 1];
#endif

  // actions
  test_two_backspaces();
  test_character();
  test_alert();
  test_emit_keystroke();
  test_invalidate_context();
  test_persist_opt();
  test_caps_lock();
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
