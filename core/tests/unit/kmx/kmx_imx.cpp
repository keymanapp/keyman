/*
  Copyright:    © 2021 SIL International.
  Description:  Tests for key rules list generation in the kmx processor
  Create Date:  30 Oct 2018
  Authors:      Ross Cruickshank (RC) Marc Durdin (MD), Tim Eves (TSE)

*/

#include <kmx/kmx_processevent.h>

#include "path.hpp"
#include "state.hpp"
#include "../kmnkbd/action_items.hpp"
#include <test_assert.h>
#include <test_color.h>
#include "../emscripten_filesystem.h"
#include "utfcodec.hpp"

#include <map>
#include <iostream>

/** This test will test the infrastructure around IMX third party librays
 *  and callback. The functions tested are:
 *  que_actions
 *  get_intermediate_context
 */

using namespace km::core::kmx;

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

std::map<uint16_t,std::string> expected_imx_map { {3, "imsample.dll:DF"},
                                      {4, "imsample.dll:func2"},
                                      {5, "another.dll:func3"},
                                      {6, "another.dll:func4"},
                                       };

std::map<uint16_t,std::string> g_extract_imx_map;

int error_args() {
    std::cerr << "kmx: Not enough arguments." << std::endl;
    return 1;
}



extern "C"
{
  /**
   * This callback will be used in place of a callback to the third party
   * library. It will then test adding to the action queue all the implemented action types.
   * KM_CORE_IT_END
   * KM_CORE_IT_CHAR
   * KM_CORE_IT_MARKER
   * KM_CORE_IT_ALERT
   * KM_CORE_IT_BACK
   * KM_CORE_IT_INVALIDATE_CONTEXT
   *
   *
   * @param state
   * @param imx_id
   * @param callback_object
   * @return uint8_t
   */
uint8_t test_imx_callback(km_core_state *state, uint32_t imx_id, void *callback_object) {

  std::cout << "test_imx_callback imx_id: " << imx_id << std::endl;
  if (callback_object==nullptr) {
    return FALSE;
  }

  // Test get intermediate context
  km_core_context_item *entry_context = nullptr;
  km_core_state_get_intermediate_context(state, &entry_context);

  size_t n = 0;
  try_status(context_items_to_utf16(entry_context, nullptr, &n))
  km_core_cu *buf = new km_core_cu[n];
  try_status(context_items_to_utf16(entry_context, buf, &n));

  std::cout << "imx entry context   : "  << " [" << buf << "]" << std::endl;
  km_core_context_items_dispose(entry_context);
  delete[] buf;

  // Test km_core_state_queue_action_items but also test identifying the unique
  // number assigned to each 3rd party library function name.
  std::map<uint16_t,std::string> *imx_map = static_cast<std::map<uint16_t,std::string>*>(callback_object);
  std::map<uint16_t,std::string>::iterator it;
  it = (*imx_map).find(imx_id);
  if (it == (*imx_map).end()) {
    std::cerr  << "Unique store number is not in the engines imx list" << std::endl;
    return FALSE;
  }

  std::cout << "test_imx_callback function name: " << it->second << std::endl;
  km_core_action_item *a_items = new km_core_action_item[10];
  switch (imx_id) {
    // this corresponds to store index; if these change, the values
    // here will need updating
  case 3:

    a_items[0].type      = KM_CORE_IT_CHAR;
    a_items[0].character = km_core_usv('X');
    a_items[1].type      = KM_CORE_IT_ALERT;
    a_items[2].type   = KM_CORE_IT_END;
    km_core_state_queue_action_items(state, a_items);
    break;
  case 4:

    a_items[0].type      = KM_CORE_IT_CHAR;
    a_items[0].character = km_core_usv('Y');
    a_items[1].type      = KM_CORE_IT_MARKER;
    a_items[1].marker = 1;
    a_items[2].type      = KM_CORE_IT_CHAR;
    a_items[2].character = km_core_usv('A');
    a_items[3].type   = KM_CORE_IT_END;
    km_core_state_queue_action_items(state, a_items);
    break;
  case 5:
    a_items[0].type      = KM_CORE_IT_BACK;
    a_items[0].backspace.expected_type = KM_CORE_BT_CHAR;
    a_items[0].backspace.expected_value = km_core_usv('A');
    a_items[1].type      = KM_CORE_IT_CHAR;
    a_items[1].character = km_core_usv('Z');
    a_items[2].type   = KM_CORE_IT_END;
    km_core_state_queue_action_items(state, a_items);
    break;
  case 6: {
      a_items[0].type   = KM_CORE_IT_INVALIDATE_CONTEXT;
      a_items[1].type   = KM_CORE_IT_END;
      km_core_state_queue_action_items(state, a_items);
    }
    break;
  default:
    break;
  }
  if (a_items) {
    delete[] a_items;
  }
  // Test Exit Context
  km_core_context_item *exit_context = nullptr;
  km_core_state_get_intermediate_context(state, &exit_context);

  n = 0;
  try_status(context_items_to_utf16(exit_context, nullptr, &n))
  km_core_cu *tmp_buf = new km_core_cu[n];
  try_status(context_items_to_utf16(exit_context, tmp_buf, &n));

  std::cout << "imx exit context   : "  << " [" << tmp_buf << "]" << std::endl;
  km_core_context_items_dispose(exit_context);
  delete[] tmp_buf;
  return 1;
}
}; // extern "C"

void test_imx_list(const km::core::path &source_file){

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_keyboard_imx * kb_imx_list;

  km::core::path full_path = source_file;

  try_status(km_core_keyboard_load(full_path.native().c_str(), &test_kb));

  // Setup state, environment
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_core_keyboard_get_imx_list(test_kb, &kb_imx_list));

  // This keyboard has 4 function names from 2 different libraries in the stores
  km_core_keyboard_imx *imx_rule_it = kb_imx_list;
  auto x = 0;
  for (; imx_rule_it->library_name; ++imx_rule_it) {
    std::u16string str;
    str.append(imx_rule_it->library_name);
    str.append(u":");
    str.append(imx_rule_it->function_name);
    auto extracted_library_function = convert<km_core_cu,char>(str);
    assert(extracted_library_function == expected_imx_map[imx_rule_it->imx_id] );
    g_extract_imx_map[imx_rule_it->imx_id] = extracted_library_function;
    ++x;
  }

  std::cout << " X Value is " << x << std::endl;

  assert(x==4);
  km_core_keyboard_imx_list_dispose(kb_imx_list);

  km_core_state_dispose(test_state);
  km_core_keyboard_dispose(test_kb);

}

// This tests both the registering callbacks and queuing actions.
// The sequence for this test will be to load a keyboard and register
// a callback - `test_imx_callback`.
// Then it will call `km_core_process_event` with a `key` that will cause
// the callback to be called. The callback will then use
// `km_core_state_queue_action_items` to queue action times so that
// the kmx processor will add this items to its action queue.
// Finally when the `process_event` call returns we verify the action
// queue is as expected.
void test_queue_actions (const km::core::path &source_keyboard) {

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_keyboard_imx * kb_imx_list;

  km::core::path full_path = source_keyboard;

  try_status(km_core_keyboard_load(full_path.native().c_str(), &test_kb));

  // Setup state, environment
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_core_keyboard_get_imx_list(test_kb, &kb_imx_list));
  km_core_state_imx_register_callback(test_state, test_imx_callback, (void*)&g_extract_imx_map);

  // Key Press that doesn't trigger a call back
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_S,KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('S')}}, {KM_CORE_IT_END}}));

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_BKSP, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('X')}}, {KM_CORE_IT_ALERT, {0,}, {0}}, {KM_CORE_IT_END}}));

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_ESC, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, { {KM_CORE_IT_CHAR, {0,}, {km_core_usv('Y')}},
                                    {KM_CORE_IT_MARKER, {0,}, {1}},
                                    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('A')}},
                                    {KM_CORE_IT_END}}));

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_1, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));

  km_core_action_item bksp_a = {KM_CORE_IT_BACK};
  bksp_a.backspace.expected_type = KM_CORE_BT_CHAR;
  bksp_a.backspace.expected_value = 'A';
  assert(action_items(test_state, {bksp_a,{KM_CORE_IT_CHAR, {0,}, {km_core_usv('Z')}}, {KM_CORE_IT_END}}));

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_2, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_INVALIDATE_CONTEXT, {0,}, {0}}, {KM_CORE_IT_END}}));

  km_core_state_imx_deregister_callback(test_state);
  km_core_keyboard_imx_list_dispose(kb_imx_list);

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
  test_imx_list(get_wasm_file_path(argv[first_arg]));
  test_queue_actions(get_wasm_file_path(argv[first_arg]));
#else
  test_imx_list(argv[first_arg]);
  test_queue_actions(argv[first_arg]);
#endif

  return 0;
}
