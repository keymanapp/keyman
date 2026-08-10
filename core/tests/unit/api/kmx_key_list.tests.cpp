/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Ross Cruickshank on 2021-11-15
 *
 * Keyman Core - Tests for key rules list generation in the kmx processor
 */

#include <kmx/kmx_processevent.h>

#include "path.hpp"
#include "state.hpp"
#include <map>

#include "../helpers/core_test_helpers.h"

using namespace km::core::kmx;

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
TEST(KeyListTests, TestKeyList) {
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_keyboard_key * kb_key_list;

  km::core::path full_path = km::core::path(test_dir / "kmx_key_list.kmx");

  auto blob = km::tests::load_kmx_file(full_path.native().c_str());
  ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(full_path.stem().c_str(), blob.data(), blob.size(), &test_kb));

  // Setup state, environment
  ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));

  ASSERT_STATUS_OK(km_core_keyboard_get_key_list(test_kb,&kb_key_list));

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
  ASSERT_EQ(n, 7);

  std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t>::iterator it_expected;
  std::map<std::pair<km_core_virtual_key,uint32_t>, uint32_t>::iterator it_key_list = map_key_list.begin();
  while (it_key_list != map_key_list.end()){

    it_expected = kb_key_expected_key_list.find(it_key_list->first);
    ASSERT_NE(it_expected, kb_key_expected_key_list.end());
    it_key_list++;
  }

  km_core_keyboard_key_list_dispose(kb_key_list);
  km_core_state_dispose(test_state);
  km_core_keyboard_dispose(test_kb);
}
