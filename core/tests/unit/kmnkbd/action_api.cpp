/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin
  History:      23 Oct 2023 - MCD - Initial implementation.
*/
#include <string>
#include <keyman/keyman_core_api.h>

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

  km_core_actions *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 1);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);

  assert(actions->do_alert == false);
  assert(actions->emit_keystroke == false);
  assert(actions->new_caps_lock_state == -1);

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

  km_core_actions *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"ABD");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == false);
  assert(actions->emit_keystroke == false);
  assert(actions->new_caps_lock_state == -1);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_alert() {
  const km_core_action_item action_items[] = {
    alert_action_item(),
    end_action_item()
  };

  km_core_actions *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == KM_CORE_TRUE);
  assert(actions->emit_keystroke == KM_CORE_FALSE);
  assert(actions->new_caps_lock_state == KM_CORE_CAPS_UNCHANGED);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_emit_keystroke() {
  const km_core_action_item action_items[] = {
    emit_keystroke_action_item(),
    end_action_item()
  };

  km_core_actions *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == KM_CORE_FALSE);
  assert(actions->emit_keystroke == KM_CORE_TRUE);
  assert(actions->new_caps_lock_state == KM_CORE_CAPS_UNCHANGED);

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------

void test_invalidate_context() {
  // note, this generates a no-op
  const km_core_action_item action_items[] = {
    invalidate_context_action_item(),
    end_action_item()
  };

  km_core_actions *actions = km::core::action_item_list_to_actions_object(action_items);

  assert(actions->code_points_to_delete == 0);
  assert(std::u32string(actions->output) == U"");
  assert(actions->persist_options != nullptr);
  assert(actions->persist_options[0].key == nullptr);
  assert(actions->persist_options[0].value == nullptr);
  assert(actions->persist_options[0].scope == KM_CORE_OPT_UNKNOWN);
  assert(actions->do_alert == KM_CORE_FALSE);
  assert(actions->emit_keystroke == KM_CORE_FALSE);
  assert(actions->new_caps_lock_state == KM_CORE_CAPS_UNCHANGED);

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

  km_core_actions *actions = km::core::action_item_list_to_actions_object(action_items);

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

  try_status(km_core_actions_dispose(actions));
}

//-------------------------------------------------------------------------------------
// Context tests
//-------------------------------------------------------------------------------------

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

km_core_keyboard * test_kb = nullptr;
km_core_state * test_state = nullptr;
km_core_context_item * citems = nullptr;
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

void setup(const char *keyboard, const km_core_cp* context) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, keyboard);
  try_status(km_core_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_core_context_items_from_utf16(context, &citems));
  try_status(km_core_context_set(km_core_state_context(test_state), citems));
}

bool is_identical_context(km_core_cp const *cached_context) {
  size_t buf_size;
  try_status(km_core_context_get(km_core_state_context(test_state), &citems));
  try_status(km_core_context_items_to_utf16(citems, nullptr, &buf_size));
  km_core_cp* new_cached_context = new km_core_cp[buf_size];
  try_status(km_core_context_items_to_utf16(citems, new_cached_context, &buf_size));
  bool result = std::u16string(cached_context) == new_cached_context;
  delete[] new_cached_context;
  return result;
}

void test_context_set_if_needed_identical_context() {
  km_core_cp const *application_context = u"This is a test";
  km_core_cp const *cached_context =      u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UNCHANGED);
  assert(is_identical_context(cached_context));
  teardown();
}

void test_context_set_if_needed_different_context() {
  km_core_cp const *application_context = u"This is a    test";
  km_core_cp const *cached_context =      u"This isn't a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void test_context_set_if_needed_cached_context_cleared() {
  km_core_cp const *application_context = u"This is a test";
  km_core_cp const *cached_context =      u"";
  setup("k_000___null_keyboard.kmx", cached_context);
  km_core_state_context_clear(test_state);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void test_context_set_if_needed_application_context_empty() {
  km_core_cp const *application_context = u"";
  km_core_cp const *cached_context =      u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void test_context_set_if_needed_app_context_is_longer() {
  km_core_cp const *application_context = u"Longer This is a test";
  km_core_cp const *cached_context =             u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UNCHANGED);
  // Should be true -- longer, but what exists is identical to cached
  assert(is_identical_context(cached_context));
  teardown();
}

void test_context_set_if_needed_app_context_is_shorter() {
  km_core_cp const *application_context =      u"is a test";
  km_core_cp const *cached_context =      u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  // Should be false -- app ctxt is shorter, so doesn't matter that what we have
  // matches
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void test_context_set_if_needed_cached_context_has_markers() {
  km_core_cp const *application_context = u"123";
  km_core_cp const *cached_context =      u"123";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
    { KM_CORE_CT_MARKER, {0}, { 5 } },
    { KM_CORE_CT_CHAR, {0}, { '1' } },
    { KM_CORE_CT_MARKER, {0}, { 1 } },
    { KM_CORE_CT_CHAR, {0}, { '2' } },
    { KM_CORE_CT_MARKER, {0}, { 2 } },
    { KM_CORE_CT_CHAR, {0}, { '3' } },
    { KM_CORE_CT_MARKER, {0}, { 3 } },
    { KM_CORE_CT_MARKER, {0}, { 4 } },
    KM_CORE_CONTEXT_ITEM_END
  };

  try_status(km_core_context_set(km_core_state_context(test_state), citems));
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UNCHANGED);

  km_core_context_item* citems_new;

  try_status(km_core_context_get(km_core_state_context(test_state), &citems_new));

  for(int i = 0; citems[i].type || citems_new[i].type; i++) {
    assert(citems_new[i].type == citems[i].type);
    if(citems[i].type == KM_CORE_CT_CHAR) {
      assert(citems_new[i].character == citems[i].character);
    } else {
      assert(citems_new[i].marker == citems[i].marker);
    }
  }

  teardown();
}

void test_context_set_if_needed() {
  test_context_set_if_needed_identical_context();
  test_context_set_if_needed_different_context();
  test_context_set_if_needed_cached_context_cleared();
  test_context_set_if_needed_application_context_empty();
  test_context_set_if_needed_app_context_is_longer();
  test_context_set_if_needed_app_context_is_shorter();
  test_context_set_if_needed_cached_context_has_markers();
}

void test_context_clear() {
  km_core_cp const *cached_context =      u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  try_status(km_core_state_context_clear(test_state));
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(u""));
  teardown();
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

#ifdef __EMSCRIPTEN__
  arg_path = get_wasm_file_path(argv[arg_color ? 2 : 1]);
#else
  arg_path = argv[arg_color ? 2 : 1];
#endif

  // actions
  test_two_backspaces();
  test_marker_text_interleaved();
  test_alert();
  test_emit_keystroke();
  test_invalidate_context();
  test_persist_opt();

  // context -- todo move to another file
  test_context_set_if_needed();
  test_context_clear();
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
