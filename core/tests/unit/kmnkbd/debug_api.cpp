/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Debugger API unit tests
 */

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
#include <keyman/keyboardprocessor.h>
#include "path.hpp"
#include "state.hpp"
#include "kmx/kmx_base.h"

#include "action_items.hpp"
#include "debug_items.hpp"

#include "../test_assert.h"

using namespace km::kbp::kmx;

km_kbp_option_item test_env_opts[] =
{
  KM_KBP_OPTIONS_END
};

km_kbp_keyboard * test_kb = nullptr;
km_kbp_state * test_state = nullptr;
km_kbp_context_item * citems = nullptr;
char * arg_path;

void teardown() {
  if(citems) {
    km_kbp_context_items_dispose(citems);
    citems = nullptr;
  }
  if(test_state) {
    km_kbp_state_dispose(test_state);
    test_state = nullptr;
  }
  if(test_kb) {
    km_kbp_keyboard_dispose(test_kb);
    test_kb = nullptr;
  }
}

void setup(const char *keyboard) {
  teardown();

  km::kbp::path path = km::kbp::path::join(arg_path, "..", "kmx", keyboard);

  try_status(km_kbp_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_kbp_context_items_from_utf16(u"Hello 😁", &citems));

  // Pre-test sanity: ensure debugging is disabled
  assert(km_kbp_state_debug_get(test_state) == 0);

  // Ensure the pre-run debug item state is not empty
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, {}, {}, {}}
  }));

  try_status(km_kbp_context_set(km_kbp_state_context(test_state), citems));
}

/**
 * Test 1: Start with debugging disabled
 */
void test_debugging_disabled() {
  setup("000 - null keyboard.kmx");
  try_status(km_kbp_state_debug_set(test_state, 0));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_END}
  }));

  assert(action_items(test_state, {
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('S')}},
    {KM_KBP_IT_END}
  }));
}

/**
 * Test 2: Debugging enabled, no rule match
 */
void test_debugging_no_rule_match() {
  setup("000 - null keyboard.kmx");
  DEBUG_GROUP gp = {u"Main"};
  try_status(km_kbp_state_debug_set(test_state, 1));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT, 'S'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}
  }));

  assert(action_items(test_state, {
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('S')}},
    {KM_KBP_IT_END}
  }));
}

/**
 * Test 3: Debugging enabled, function key pressed, no rule match
 */
void test_debugging_function_key() {
  setup("000 - null keyboard.kmx");
  DEBUG_GROUP gp = {u"Main"};
  try_status(km_kbp_state_debug_set(test_state, 1));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_F1, 0, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_F1, 0, 0}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, KM_KBP_DEBUG_FLAG_OUTPUTKEYSTROKE, {}, {u"", nullptr, nullptr, {}, 1}}
  }));

  assert(action_items(test_state, {
    {KM_KBP_IT_EMIT_KEYSTROKE},
    {KM_KBP_IT_INVALIDATE_CONTEXT}, // It's a non character key that is not a modifier, so this is a hint that context may no longer be valid
    {KM_KBP_IT_END}
  }));
}

/**
 * Test 4: basic rule match
 */
void test_basic_rule_matches() {
  setup("002 - basic input Unicode.kmx");
  DEBUG_GROUP gp = {u"Main"};
  DEBUG_KEY kp = { 'F', /*line*/16, /*shift*/0 }; // vkey is a char
  try_status(km_kbp_state_debug_set(test_state, 1));

  // 'DE' + 'F' > U+0E04 U+0E05 U+0E06

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_D, KM_KBP_MODIFIER_SHIFT, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_D, KM_KBP_MODIFIER_SHIFT, 'D'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}, // action item will emit a default 'D'
  }));

  assert(action_items(test_state, {
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('D')}},
    {KM_KBP_IT_END}
  }));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_E, KM_KBP_MODIFIER_SHIFT, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_E, KM_KBP_MODIFIER_SHIFT, 'E'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}, // action item will emit a default 'E'
  }));

  assert(action_items(test_state, {
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('E')}},
    {KM_KBP_IT_END}
  }));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_F, KM_KBP_MODIFIER_SHIFT, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_F, KM_KBP_MODIFIER_SHIFT, 'F'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"DE", &gp, &kp, {0xFFFF}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"DE", &gp, &kp, {0xFFFF}, 5 /* bksp bksp E04 E05 E06 */ }},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 5 /* from above */ }},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 5 /* from above */ }},
  }));

  km_kbp_action_item bksp_d = {KM_KBP_IT_BACK};
  bksp_d.backspace.expected_type = KM_KBP_BT_CHAR;
  bksp_d.backspace.expected_value = 'D';

  km_kbp_action_item bksp_e = {KM_KBP_IT_BACK};
  bksp_e.backspace.expected_type = KM_KBP_BT_CHAR;
  bksp_e.backspace.expected_value = 'E';

  assert(action_items(test_state, {
    bksp_e,
    bksp_d,
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv(u'\u0E04')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv(u'\u0E05')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv(u'\u0E06')}},
    {KM_KBP_IT_END}
  }));

}

/**
 * Test 5: Multiple groups
 */
void test_multiple_groups() {
  setup("030 - multiple groups.kmx");
  DEBUG_GROUP gp = {u"Main"}, gpa = {u"a"}, gpb = {u"b"};
  DEBUG_KEY kp1 = { KM_KBP_VKEY_1, /*line*/12, /*shift*/KM_KBP_MODIFIER_VIRTUALKEY },
            kp2 = { KM_KBP_VKEY_2, /*line*/13, /*shift*/KM_KBP_MODIFIER_VIRTUALKEY },
            kpa = { 0, /*line*/19, /*shift*/0 },
            kpb = { 0, /*line*/23, /*shift*/0 };

  try_status(km_kbp_state_debug_set(test_state, 1));

  // '12' -> 'abc'

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_1, 0, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_1, 0, '1'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kp1, {0xFFFF}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kp1, {0xFFFF}, 1}},

      km_kbp_state_debug_item{KM_KBP_DEBUG_MATCH_ENTER, 0, {}, {u"", &gp, nullptr, {}, 1}},
        km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gpa, nullptr, {}, 1}},
          km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"a", &gpa, &kpa, {0xFFFF}, 1}},
          km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"a", &gpa, &kpa, {0xFFFF}, 3}},
        km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gpa, nullptr, {}, 3}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_MATCH_EXIT, 0, {}, {u"", &gp, nullptr, {}, 3}},

    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 3}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 3}}, // action item will emit a 'b'
  }));

  km_kbp_action_item bksp_a = {KM_KBP_IT_BACK};
  bksp_a.backspace.expected_type = KM_KBP_BT_CHAR;
  bksp_a.backspace.expected_value = 'a';

  assert(action_items(test_state, {
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('a')}},
    bksp_a,
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('b')}},
    {KM_KBP_IT_END}
  }));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_2, 0, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_2, 0, '2'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"b", &gp, &kp2, {0xFFFF}}},
        km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gpb}},
          km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"b", &gpb, &kpb, {0xFFFF}}},
          km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"b", &gpb, &kpb, {0xFFFF}, 4}},
        km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gpb, nullptr, {}, 4}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"b", &gp, &kp2, {0xFFFF}, 4}},

      //See #5440 for why match does not fire here:
      //km_kbp_state_debug_item{KM_KBP_DEBUG_MATCH_ENTER, 0, {}, {u"", &gp}},
      //  km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gpa}},
      //  km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gpa}},
      //km_kbp_state_debug_item{KM_KBP_DEBUG_MATCH_EXIT, 0, {}, {u"", &gp}},

    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 4}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 4}}, // action item will "abc"
  }));

  km_kbp_action_item bksp_b = {KM_KBP_IT_BACK};
  bksp_b.backspace.expected_type = KM_KBP_BT_CHAR;
  bksp_b.backspace.expected_value = 'b';

  assert(action_items(test_state, {
    bksp_b,
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('a')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('b')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('c')}},
    {KM_KBP_IT_END}
  }));

}

/**
 * Test 6: store offsets
 */
void test_store_offsets() {
  setup("044 - if and context.kmx");
  DEBUG_GROUP gp = {u"Main"};
  DEBUG_KEY kpa = { 'a', /*line*/15, },
            kpb = { 'b', /*line*/19, };

  try_status(km_kbp_state_debug_set(test_state, 1));

  // 'ab' -> 'ex'

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_A, 0, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_A, 0, 'a'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kpa, {0xFFFF}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kpa, {0xFFFF}, 4}},

    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 4}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 4}}, // action item will emit a 'exay'
  }));

  assert(action_items(test_state, {
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('e')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('x')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('a')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('y')}},
    {KM_KBP_IT_END}
  }));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_B, 0, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_B, 0, 'b'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      // store_offsets: 6 = store #, 1 = index into store, 6 = store #, 0 = index into store
      // store: store(diaeresisBase) 'ae'
      // context: exay
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"exay", &gp, &kpb, {6, 1, 6, 0, 0xFFFF}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"exay", &gp, &kpb, {6, 1, 6, 0, 0xFFFF}, 6}},

    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 6}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 6}},
  }));

  km_kbp_action_item bksp[] = {
      {KM_KBP_IT_BACK},
      {KM_KBP_IT_BACK},
      {KM_KBP_IT_BACK},
      {KM_KBP_IT_BACK}
  };
  bksp[0].backspace.expected_type = KM_KBP_BT_CHAR;
  bksp[1].backspace.expected_type = KM_KBP_BT_CHAR;
  bksp[2].backspace.expected_type = KM_KBP_BT_CHAR;
  bksp[3].backspace.expected_type = KM_KBP_BT_CHAR;
  bksp[0].backspace.expected_value = 'y';
  bksp[1].backspace.expected_value = 'a';
  bksp[2].backspace.expected_value = 'x';
  bksp[3].backspace.expected_value = 'e';

  assert(action_items(test_state, {
    bksp[0],
    bksp[1],
    bksp[2],
    bksp[3],
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('e')}},
    {KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('x')}},
    {KM_KBP_IT_END}
  }));
}

/**
 * Test 7: set option
 */
void test_set_option() {
  setup("021 - options.kmx");
  DEBUG_GROUP gp = {u"Main"};
  DEBUG_KEY kp1 = { '1', /*line*/16, };
  DEBUG_STORE sp = {0, u"foo", u"1"};

  try_status(km_kbp_state_debug_set(test_state, 1));

  // '1' -> set_option

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_1, 0, 1));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_1, 0, '1'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kp1, {0xFFFF}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_SET_OPTION, 0, {}, {u"", nullptr, nullptr, {}, 0, {&sp, u"1"}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kp1, {0xFFFF}, 0}},

    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 0}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 0}},
  }));
}


constexpr const auto help_str = "\
debug_api [--color] <SOURCE_PATH>|--print-sizeof\n\
\n\
  --color         Force color output\n\
  --print-sizeof  Emit structure sizes for interop debug\n\
  SOURCE_PATH     Path where debug_api.cpp is found; kmx files are\n\
                  located relative to this path.\n";

int error_args() {
  std::cerr << "debug_api: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int print_sizeof() {
  std::cout << std::endl;
  std::cout << "keyboardprocessor.h:" << std::endl;
  std::cout << "sizeof(km_kbp_context_item): " << sizeof(km_kbp_context_item) << std::endl;
  std::cout << "sizeof(km_kbp_action_item): " << sizeof(km_kbp_action_item) << std::endl;
  std::cout << "sizeof(km_kbp_option_item): " << sizeof(km_kbp_option_item) << std::endl;
  std::cout << "sizeof(km_kbp_keyboard_attrs): " << sizeof(km_kbp_keyboard_attrs) << std::endl;
  std::cout << "sizeof(km_kbp_attr): " << sizeof(km_kbp_attr) << std::endl;
  std::cout << std::endl;
  std::cout << "keyboardprocessor_debug.h:" << std::endl;
  std::cout << "sizeof(km_kbp_state_debug_item): " << sizeof(km_kbp_state_debug_item) << std::endl;
  std::cout << "sizeof(km_kbp_state_debug_key_info): " << sizeof(km_kbp_state_debug_key_info) << std::endl;
  std::cout << "sizeof(km_kbp_state_debug_kmx_info): " << sizeof(km_kbp_state_debug_kmx_info) << std::endl;
  std::cout << "sizeof(km_kbp_state_debug_kmx_option_info): " << sizeof(km_kbp_state_debug_kmx_option_info) << std::endl;
  std::cout << "sizeof([enum] km_kbp_debug_type): " << sizeof(km_kbp_debug_type) << std::endl;
  exit(1);
}

int main(int argc, char *argv []) {
  // TODO: choose/create a keyboard which has some rules
  // Global setup
  //std::string path(argv[1]);

  if(argc < 2) {
    return error_args();
  }

  if(std::string(argv[1]) == "--print-sizeof") {
    return print_sizeof();
  }

  auto arg_color = std::string(argv[1]) == "--color";
  if(arg_color && argc < 3) {
    return error_args();
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

  arg_path = argv[arg_color ? 2 : 1];

  test_debugging_disabled();
  test_debugging_no_rule_match();
  test_debugging_function_key();
  test_basic_rule_matches();
  test_multiple_groups();
  test_store_offsets();
  test_set_option();

  // Destroy them
  teardown();


  return 0;
}
