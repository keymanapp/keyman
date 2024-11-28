/*
  Copyright:    © 2023 SIL International.
  Description:  Tests for external event processing for the kmx processor
  Create Date:  28 Jul 2023
  Authors:      Ross Cruickshank (RC)

*/

#include <kmx/kmx_processevent.h>

#include "path.hpp"
#include "state.hpp"
#include "../kmnkbd/action_items.hpp"
#include <test_assert.h>
#include <test_color.h>
#include "../emscripten_filesystem.h"

#include <map>
#include <iostream>

/**
 * This test will test the infrastructure around the external event processing
 * The functions tested are:
 * - km_core_event with the event KM_CORE_EVENT_KEYBOARD_ACTIVATED
 */

using namespace km::core::kmx;

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

int error_args() {
  std::cerr << "kmx: Not enough arguments." << std::endl;
  return 1;
}

void test_external_event(const km::core::path &source_file){

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;

  km::core::path full_path = source_file;

  try_status(km_core_keyboard_load(full_path.native().c_str(), &test_kb));

  // Setup state, environment
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));

  // Set up external events to test the processing.
  uint32_t event = KM_CORE_EVENT_KEYBOARD_ACTIVATED;
  // For this particular test we want to have the capslock state as on.
  // then after the km_core_event the caps lock should be off as the load
  // keyboard is a caps always off keyboard

  try_status(km_core_event(test_state, event, nullptr));
  // The action to turn capslock off must be in the actions list.
  test_assert(action_items(test_state, {{KM_CORE_IT_CAPSLOCK, {0,}, {0}}, {KM_CORE_IT_END}}));

  km_core_state_dispose(test_state);
  km_core_keyboard_dispose(test_kb);

}

int main(int argc, char *argv []) {
  int first_arg = 1;

  if (argc < 2) {
    return error_args();
  }

  auto arg_color = std::string(argv[1]) == "--color";
  if(arg_color) {
    first_arg++;
    if(argc < 3) {
      return error_args();
    }
  }
  console_color::enabled = console_color::isaterminal() || arg_color;
  km::core::kmx::g_debug_ToConsole = TRUE;

#ifdef __EMSCRIPTEN__
  test_external_event(get_wasm_file_path(argv[first_arg]));
#else
  test_external_event(argv[first_arg]);
#endif

  return 0;
}
