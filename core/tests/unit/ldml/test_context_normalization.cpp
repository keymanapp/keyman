/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for normalization in the context API.
  Create Date:  15 Jan 2024
  Authors:      Marc Durdin
  History:      15 Jan 2024 - MCD - Initial implementation.
*/
#include <string>
#include <keyman/keyman_core_api.h>

#include "path.hpp"
#include "action.hpp"

#include <test_assert.h>
#include "../emscripten_filesystem.h"

//-------------------------------------------------------------------------------------
// Context normalization tests
//-------------------------------------------------------------------------------------

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

km_core_keyboard * test_kb = nullptr;
km_core_state * test_state = nullptr;
std::string arg_path;

void teardown() {
  if(test_state) {
    km_core_state_dispose(test_state);
    test_state = nullptr;
  }
  if(test_kb) {
    km_core_keyboard_dispose(test_kb);
    test_kb = nullptr;
  }
}

void setup(const char *keyboard) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, "keyboards", keyboard);
  try_status(km_core_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
}

bool is_identical_context(km_core_cp const *cached_context, km_core_debug_context_type context_type) {
  size_t buf_size;
  km_core_context_item * citems = nullptr;

  if(context_type == KM_CORE_DEBUG_CONTEXT_APP) {
    try_status(km_core_context_get(km_core_state_app_context(test_state), &citems));
  } else {
    try_status(km_core_context_get(km_core_state_context(test_state), &citems));
  }
  try_status(km_core_context_items_to_utf16(citems, nullptr, &buf_size));
  km_core_cp* new_cached_context = new km_core_cp[buf_size];
  try_status(km_core_context_items_to_utf16(citems, new_cached_context, &buf_size));

  km_core_context_items_dispose(citems);

  bool result = std::u16string(cached_context) == new_cached_context;
  delete[] new_cached_context;
  return result;
}

void test_context_set_if_needed_for_ldml_normalization() {
  km_core_cp const *application_context = u"This is a test À";
  km_core_cp const *cached_context =      u"This is a test A\u0300";
  setup("k_001_tiny.kmx");
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
  assert(is_identical_context(application_context, KM_CORE_DEBUG_CONTEXT_APP));
  teardown();
}

void test_context_set_if_needed() {
  test_context_set_if_needed_for_ldml_normalization();
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str = "\
context_normalization [--color]\n\
\n\
  --color         Force color output\n";

int error_args() {
  std::cerr << "context_normalization: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {
  auto arg_color = argc > 1 && std::string(argv[1]) == "--color";
  console_color::enabled = console_color::isaterminal() || arg_color;

  // Get the path of the current executable
  arg_path = argv[0];
  auto last = arg_path.find_last_of("/\\");
  if(last == std::string::npos) {
    std::cerr << "could not parse argv[0]: " << argv[0] << std::endl;
    return 1;
  }
  arg_path.resize(last+1);

#ifdef __EMSCRIPTEN__
  arg_path = get_wasm_file_path(arg_path);
#endif

  test_context_set_if_needed();
}
