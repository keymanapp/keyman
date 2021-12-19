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
#include "../test_assert.h"
#include "../test_color.h"

#include <map>
#include<iostream>
#include<sstream>

/** This test will test the infrastructure around IMX third party librays
 *  and callback. The functions tested are:
 *  que_actions
 *  get_intermediate_context
 */

using namespace km::kbp::kmx;

km_kbp_option_item test_env_opts[] =
{
  KM_KBP_OPTIONS_END
};

std::map<uint16_t,std::string> expected_imx_map { {4, "imsample.dll:DF"},
                                      {5, "imsample.dll:func2"},
                                      {6, "another.dll:func3"},
                                      {7, "another.dll:func4"},
                                       };

std::map<uint16_t,std::string> g_extract_imx_map;

km_kbp_keyboard_imx * global_imx_list;

int error_args() {
    std::cerr << "kmx: Not enough arguments." << std::endl;
    return 1;
}



extern "C"
{
  /**
   * This callback will be used in place of a callback to the third party
   * library. It will then test adding to the action queue all the implemented action types.
   * KM_KBP_IT_END
   * KM_KBP_IT_CHAR
   * KM_KBP_IT_MARKER
   * KM_KBP_IT_ALERT
   * KM_KBP_IT_BACK
   * KM_KBP_IT_INVALIDATE_CONTEXT
   *
   *
   * @param state
   * @param store_no
   * @param callback_object
   * @return uint8_t
   */
  uint8_t test_imx_callback(km_kbp_state *state, uint32_t store_no, void *callback_object){

  std::cout << "test_imx_callback store_no: " << store_no << std::endl;
  if (callback_object==nullptr)
  {
    return FALSE;
  }

  // Test get intermediate context
  km_kbp_context_item *entry_context = nullptr;
  kbp_state_get_intermediate_context(state, &entry_context);

  size_t n = 0;
  try_status(km_kbp_context_items_to_utf16(entry_context, nullptr, &n))
  km_kbp_cp *buf = new km_kbp_cp[n];
  try_status(km_kbp_context_items_to_utf16(entry_context, buf, &n));

  std::cout << "imx entry context   : "  << " [" << buf << "]" << std::endl;
  km_kbp_context_items_dispose(entry_context);
  delete[] buf;

  // Test km_kbp_state_queue_action_items but also test identifying the unique
  // number assigned to each 3rd party library function name.
  std::map<uint16_t,std::string> *imx_map = static_cast<std::map<uint16_t,std::string>*>(callback_object);
  std::map<uint16_t,std::string>::iterator it;
  it = (*imx_map).find(store_no);
  if (it == (*imx_map).end())
  {
    std::cerr  << "Unique store number is not in the engines imx list" << std::endl;
    return FALSE;
  }
  std::cout << "test_imx_callback function name: " << it->second << std::endl;
  km_kbp_action_item *a_items = new km_kbp_action_item[10];
  switch (store_no)
  {
  case 4:

    a_items[0].type      = KM_KBP_IT_CHAR;
    a_items[0].character = km_kbp_usv('X');
    a_items[1].type      = KM_KBP_IT_ALERT;
    a_items[2].type   = KM_KBP_IT_END;
    km_kbp_state_queue_action_items(state, a_items);
    break;
    case 5:

    a_items[0].type      = KM_KBP_IT_CHAR;
    a_items[0].character = km_kbp_usv('Y');
    a_items[1].type      = KM_KBP_IT_MARKER;
    a_items[1].marker = 1;
    a_items[2].type      = KM_KBP_IT_CHAR;
    a_items[2].character = km_kbp_usv('A');
    a_items[3].type   = KM_KBP_IT_END;
    km_kbp_state_queue_action_items(state, a_items);
    break;
    case 6:
    a_items[0].type      = KM_KBP_IT_BACK;
    a_items[0].backspace.expected_type = KM_KBP_BT_CHAR;
    a_items[0].backspace.expected_value = km_kbp_usv('A');
    a_items[1].type      = KM_KBP_IT_CHAR;
    a_items[1].character = km_kbp_usv('Z');
    a_items[2].type   = KM_KBP_IT_END;
    km_kbp_state_queue_action_items(state, a_items);
    break;
    case 7:
    {
      a_items[0].type   = KM_KBP_IT_INVALIDATE_CONTEXT;
      a_items[1].type   = KM_KBP_IT_END;
      km_kbp_state_queue_action_items(state, a_items);
    }
    break;
  default:
    break;
  }
  if (a_items)
  {
    delete[] a_items;
  }
  // Test Exit Context
  km_kbp_context_item *exit_context = nullptr;
  kbp_state_get_intermediate_context(state, &exit_context);

  n = 0;
  try_status(km_kbp_context_items_to_utf16(exit_context, nullptr, &n))
  km_kbp_cp *tmp_buf = new km_kbp_cp[n];
  try_status(km_kbp_context_items_to_utf16(exit_context, tmp_buf, &n));

  std::cout << "imx exit context   : "  << " [" << tmp_buf << "]" << std::endl;
  km_kbp_context_items_dispose(exit_context);
  delete[] tmp_buf;
  return 1;
  }
};

void test_imx_list(const km::kbp::path &source_file){

  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr;
  km_kbp_keyboard_imx * kb_imx_list;


  km::kbp::path full_path = source_file;

  try_status(km_kbp_keyboard_load(full_path.native().c_str(), &test_kb));

  // Setup state, environment
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_kbp_keyboard_get_imx_list(test_kb, &kb_imx_list));


  // This keyboard has 4 function names from 2 different libraries in the stores
  km_kbp_keyboard_imx *imx_rule_it = kb_imx_list;
  std::stringstream extracted_library_function;
  auto x = 0;
  for (; imx_rule_it->library_name; ++imx_rule_it) {
    extracted_library_function << imx_rule_it->library_name << ":" << imx_rule_it->function_name;
    assert(extracted_library_function.str() == expected_imx_map[imx_rule_it->store_no] );
    g_extract_imx_map[imx_rule_it->store_no] = extracted_library_function.str();
    extracted_library_function.str("");
    ++x;
  }

  std::cout << " X Value is " << x << std::endl;

  assert(x==4);
  km_kbp_keyboard_imx_list_dispose(kb_imx_list);

  km_kbp_state_dispose(test_state);
  km_kbp_keyboard_dispose(test_kb);

}

// This tests both the registering callbacks and queuing actions.
// The sequence for this test will be to load a keyboard and register
// a callback - `test_imx_callback`.
// Then it will call `km_kbp_process_event` with a `key` that will cause
// the callback to be called. The callback will then use
// `km_kbp_state_queue_action_items` to queue action times so that
// the kmx processor will add this items to its action queue.
// Finally when the `process_event` call returns we verify the action
// queue is as expected.
void test_queue_actions (const km::kbp::path &source_keybard) {

  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr;
  km_kbp_keyboard_imx * kb_imx_list;

  km::kbp::path full_path = source_keybard;

  try_status(km_kbp_keyboard_load(full_path.native().c_str(), &test_kb));

  // Setup state, environment
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_kbp_keyboard_get_imx_list(test_kb, &kb_imx_list));
  km_kbp_state_imx_register_callback(test_state, test_imx_callback, (void*)&g_extract_imx_map);

  // Key Press that doesn't trigger a call back
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S,KM_KBP_MODIFIER_SHIFT, 1));
  assert(action_items(test_state, {{KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('S')}}, {KM_KBP_IT_END}}));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_BKSP, 0, 1));
  assert(action_items(test_state, {{KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('X')}}, {KM_KBP_IT_ALERT, {0,}, {0}}, {KM_KBP_IT_END}}));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_ESC, 0, 1));
  assert(action_items(test_state, { {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('Y')}},
                                    {KM_KBP_IT_MARKER, {0,}, {1}},
                                    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('A')}},
                                    {KM_KBP_IT_END}}));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_1, 0, 1));

  km_kbp_action_item bksp_a = {KM_KBP_IT_BACK};
  bksp_a.backspace.expected_type = KM_KBP_BT_CHAR;
  bksp_a.backspace.expected_value = 'A';
  assert(action_items(test_state, {bksp_a,{KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('Z')}}, {KM_KBP_IT_END}}));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_2, 0, 1));
  assert(action_items(test_state, {{KM_KBP_IT_INVALIDATE_CONTEXT, {0,}, {0}}, {KM_KBP_IT_END}}));

  km_kbp_state_imx_deregister_callback(test_state);
  km_kbp_keyboard_imx_list_dispose(kb_imx_list);

  km_kbp_state_dispose(test_state);
  km_kbp_keyboard_dispose(test_kb);
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
  km::kbp::kmx::g_debug_ToConsole = TRUE;
  test_imx_list(argv[first_arg]);
  test_queue_actions(argv[first_arg]);
  km_kbp_keyboard_imx_list_dispose(global_imx_list);
  return 0;
}
