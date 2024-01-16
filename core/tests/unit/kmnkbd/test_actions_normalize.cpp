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

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

km_core_keyboard * test_kb = nullptr;
km_core_state * test_state = nullptr;
km_core_actions * test_actions = nullptr;
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
  if(test_actions) {
    delete [] test_actions->output;
    delete test_actions;
    test_actions = nullptr;
  }
}

void setup(const km_core_cp *context, int actions_code_points_to_delete, const std::u32string actions_output) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, "..", "ldml", "keyboards", "k_001_tiny.kmx");
  try_status(km_core_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));

  assert(km_core_state_context_set_if_needed(test_state, context) == KM_CORE_CONTEXT_STATUS_UPDATED);

  test_actions = new km_core_actions;
  test_actions->code_points_to_delete = actions_code_points_to_delete;
  test_actions->output = new km_core_usv[actions_output.length() + 1];
  actions_output.copy(test_actions->output, actions_output.length());
  test_actions->output[actions_output.length()] = 0;
}

//-------------------------------------------------------------------------------------

void test_no_normalization() {
  setup(u"abc", 0, U"def");
  const int expected_delete = 0;
  const std::u32string expected_output = U"def";

  assert(km::core::actions_normalize(km_core_state_context(test_state), km_core_state_app_context(test_state), test_actions));

  assert(expected_delete == test_actions->code_points_to_delete);
  assert(expected_output == test_actions->output);

  teardown();
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str = "\
test_actions_normalize [--color] <BUILD_PATH>\n\
\n\
  --color         Force color output\n\
  BUILD_PATH      Path where test_actions_normalize.exe is found; kmx files are\n\
                  located relative to this path.\n";

int error_args() {
  std::cerr << "test_actions_normalize: Invalid arguments." << std::endl;
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
  test_no_normalization();
}

