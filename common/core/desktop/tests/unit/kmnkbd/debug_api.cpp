/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Debugger API unit tests
 */

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <keyman/keyboardprocessor.h>
#include "path.hpp"
#include "state.hpp"
#include "../test_assert.h"

namespace
{

// https://stackoverflow.com/a/29865/1836776
void hexdump(void const *ptr, int buflen) {
  unsigned char *buf = (unsigned char*)ptr;
  int i, j;
  for (i=0; i<buflen; i+=16) {
    printf("%06x: ", i);
    for (j=0; j<16; j++)
      if (i+j < buflen)
        printf("%02x ", buf[i+j]);
      else
        printf("   ");
    printf(" ");
    for (j=0; j<16; j++)
      if (i+j < buflen)
        printf("%c", isprint(buf[i+j]) ? buf[i+j] : '.');
    printf("\n");
  }
}

bool operator==(km_kbp_state_debug_item const & lhs,
                km_kbp_state_debug_item const & rhs)
{
  int result = memcmp(&lhs, &rhs, sizeof(km_kbp_state_debug_item));
  if(result != 0) {
    hexdump(&lhs, sizeof(km_kbp_state_debug_item));
    hexdump(&rhs, sizeof(km_kbp_state_debug_item));
  }
  return result == 0;
}


bool debug_items(km_kbp_state const * state,
                  std::initializer_list<km_kbp_state_debug_item> const & expected)
{
  size_t n = 0;
  auto act = km_kbp_state_debug_items(state, &n);

  for (auto &rhs: expected)
    if (!(*act++ == rhs)) return false;

  return true;
}

  km_kbp_option_item test_env_opts[] =
  {
    KM_KBP_OPTIONS_END
  };

} // namespace


km_kbp_keyboard * test_kb = nullptr;
km_kbp_state * test_state = nullptr;
km_kbp_context_item * citems = nullptr;

void setup() {
  try_status(km_kbp_context_set(km_kbp_state_context(test_state), citems));
}

void test_debugging_disabled() {
  try_status(km_kbp_state_debug_set(test_state, 0));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_END}
  }));
}

void test_debugging_no_rule_match() {
  try_status(km_kbp_state_debug_set(test_state, 1));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {{KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT, 'S'}}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END}
  }));
}

void test_debugging_function_key() {
  try_status(km_kbp_state_debug_set(test_state, 1));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_F1, 0));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {{KM_KBP_VKEY_F1, 0, 0}}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, KM_KBP_DEBUG_FLAG_OUTPUTKEYSTROKE}
  }));
}

constexpr const auto help_str = "\
debug_api [--color] <SOURCE_PATH>\n\
\n\
  --color      Force color output\n\
  SOURCE_PATH  Path where debug_api.cpp is found; kmx files are\n\
               located relative to this path.\n";

int error_args() {
  std::cerr << "debug_api: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {
  // TODO: choose/create a keyboard which has some rules
  // Global setup
  //std::string path(argv[1]);

  if(argc < 2) {
    return error_args();
  }

  auto arg_color = std::string(argv[1]) == "--color";
  if(arg_color && argc < 3) {
    return error_args();
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

  auto arg_path = argv[arg_color ? 2 : 1];

  km::kbp::path path = km::kbp::path::join(arg_path, "..", "kmx", "000 - null keyboard.kmx");

  try_status(km_kbp_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_kbp_context_items_from_utf16(u"Hello 😁", &citems));

  // Pre-test sanity: ensure debugging is disabled
  assert(km_kbp_state_debug_get(test_state) == 0);

  // Ensure the pre-run debug item state is not empty
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_END}
  }));

  // Test 1: Start with debugging disabled
  setup();
  test_debugging_disabled();

  // Test 2: Debugging enabled, no rule match
  setup();
  test_debugging_no_rule_match();

  // Test 3: Debugging enabled, function key pressed, no rule match
  setup();
  test_debugging_function_key();

  // Destroy them
  km_kbp_context_items_dispose(citems);
  km_kbp_state_dispose(test_state);
  km_kbp_keyboard_dispose(test_kb);

  return 0;
}
