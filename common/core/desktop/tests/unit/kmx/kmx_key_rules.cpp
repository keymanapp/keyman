/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for kmx integration key rules
  Create Date:  30 Oct 2018
  Authors:      Marc Durdin (MD), Tim Eves (TSE)

  Note: Exit codes will be 100*LINE + ERROR CODE, e.g. 25005 is code 5 on line 250
*/
#include <algorithm>
#include <cctype>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <iterator>
#include <list>
#include <sstream>
#include <string>
#include <type_traits>

#include <kmx/kmx_processevent.h>
#include <kmx/kmx_xstring.h>

#include "path.hpp"
#include "state.hpp"
#include "utfcodec.hpp"

#include "../test_assert.h"
#include "../test_color.h"


using namespace km::kbp::kmx;

km_kbp_option_item test_env_opts[] =
{
  KM_KBP_OPTIONS_END
};

int error_args() {
    std::cerr << "kmx: Not enough arguments." << std::endl;
    return 1;
}

void test_key_rules(const km::kbp::path &source_path){

  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr;
  km_kbp_keyboard_key_rules * kb_key_rules;

  km::kbp::path const source_keybard = "039 - generic ctrlalt.kmx";
  km::kbp::path full_path = km::kbp::path::join(source_path, source_keybard);

  try_status(km_kbp_keyboard_load(full_path.native().c_str(), &test_kb));

  // Setup state, environment
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));

  try_status(km_kbp_keyboard_get_key_rules(test_kb,&kb_key_rules));

  // Now see how many rules are there
  //#define KM_KBP_KEYBOARD_KEY_RULES_END { 0, 0 }
  //assert(kb_key_rules);
 DebugLog("key value: %d",  kb_key_rules[0].key);
  auto n = 0;
  for (; kb_key_rules->key; ++kb_key_rules) {
    ++n;
  }
   DebugLog("key_rules test count: %d",  n);
  assert(n==2);

  // Destroy them
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

  test_key_rules(argv[first_arg]);

  return 0;
}
