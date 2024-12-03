/*
  Copyright:    © 2021 SIL International.
  Description:  Tests for key rules list generation in the kmx processor
  Create Date:  15 Nov 2021
  Authors:      Ross Cruickshank (RC) Marc Durdin (MD), Tim Eves (TSE)

*/

#include <kmx/kmx_processevent.h>

#include "path.hpp"
#include "state.hpp"
#include <map>

#include <test_assert.h>
#include <test_color.h>
#include "../emscripten_filesystem.h"
#include "../load_kmx_file.hpp"

using namespace km::core::kmx;

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

int error_args() {
    std::cerr << "kmx: Not enough arguments." << std::endl;
    return 1;
}

#define NO_MODIFIER_FLAGS 0
#define CHAR_CODE_LOWER_A 97
#define CHAR_CODE_LOWER_B 98


std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t> kb_key_expected_key_list {{{KM_CORE_VKEY_1, 0},0},
                                              {{KM_CORE_VKEY_2, 0},0},
                                              {{KM_CORE_VKEY_A, 0},0},
                                              {{KM_CORE_VKEY_B, 0},0},
                                              {{KM_CORE_VKEY_B, KM_CORE_MODIFIER_CTRL},KM_CORE_MODIFIER_CTRL},
                                              {{KM_CORE_VKEY_C, 0},0},
                                              {{KM_CORE_VKEY_C, KM_CORE_MODIFIER_ALT},KM_CORE_MODIFIER_ALT},
                                              };

/**
 * The purpose of this test is to verify that `km_core_keyboard_get_key_list`
 * returns list of all the keys used for a keyboard that core as loaded.
 *
 * @param source_file  Path to kmx keyboard file
 */
void test_key_list(const km::core::path &source_file){

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_keyboard_key * kb_key_list;

  km::core::path full_path = source_file;

  auto blob = km::tests::load_kmx_file(full_path.native().c_str());
  try_status(km_core_keyboard_load_from_blob(full_path.stem().c_str(), blob.data(), blob.size(), &test_kb));

  // Setup state, environment
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));

  try_status(km_core_keyboard_get_key_list(test_kb,&kb_key_list));

  km_core_keyboard_key *key_rule_it = kb_key_list;
  std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t> map_key_list;

  auto n = 0;
  // Internally the list is created using a map so we should not rely on order elemnts
  // in the returned array. Therefore we will put the array items back into a map to verify
  // against our expected map.
  for (; key_rule_it->key; ++key_rule_it) {
    std::cout << "key:" << key_rule_it->key << " modifer: " << key_rule_it->modifier_flag << std::endl;
    map_key_list[std::make_pair(key_rule_it->key, key_rule_it->modifier_flag)] = key_rule_it->modifier_flag;
    ++n;
  }
  test_assert(n==7);

  std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t>::iterator it_expected;
  std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t>::iterator it_key_list = map_key_list.begin();
  while (it_key_list != map_key_list.end()){

    it_expected = kb_key_expected_key_list.find(it_key_list->first);
    test_assert (it_expected != kb_key_expected_key_list.end());
    it_key_list++;
  }

  km_core_keyboard_key_list_dispose(kb_key_list);
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

#ifdef __EMSCRIPTEN__
  test_key_list(get_wasm_file_path(argv[first_arg]));
#else
  test_key_list(argv[first_arg]);
#endif

  return 0;
}
