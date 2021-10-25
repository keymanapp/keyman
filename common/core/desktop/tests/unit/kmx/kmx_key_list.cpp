/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for key rules list generation in the kmx processor
  Create Date:  30 Oct 2018
  Authors:      Ross Cruickshank (RC) Marc Durdin (MD), Tim Eves (TSE)

*/

#include <kmx/kmx_processevent.h>

#include "path.hpp"
#include "state.hpp"

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

void test_key_list(const km::kbp::path &source_path){

  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr;
  km_kbp_keyboard_key * kb_key_list;

  km::kbp::path const source_keybard = "039 - generic ctrlalt.kmx";
  km::kbp::path full_path = km::kbp::path::join(source_path, source_keybard);

  try_status(km_kbp_keyboard_load(full_path.native().c_str(), &test_kb));

  // Setup state, environment
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));

  try_status(km_kbp_keyboard_get_key_list(test_kb,&kb_key_list));

  km_kbp_keyboard_key *key_rule_it = kb_key_list;
  auto n = 0;
  for (; key_rule_it->key; ++key_rule_it) {
    ++n;
  }
  assert(n==2);

  km_kbp_keyboard_key_list_dispose(kb_key_list);
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

  test_key_list(argv[first_arg]);

  return 0;
}
