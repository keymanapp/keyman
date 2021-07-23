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
#include "../test_assert.h"
#include "kmx/kmx_base.h"
#include "kmx/kmx_xstring.h"

using namespace km::kbp::kmx;

namespace
{

const char *debug_item_types[] = {
  "KM_KBP_DEBUG_BEGIN", // = 0,
  "//KM_KBP_DEBUG_BEGIN_ANSI", // = 1, // not supported; instead rewrite ansi keyboards to Unicode with mcompile
  "KM_KBP_DEBUG_GROUP_ENTER", // = 2,
  "KM_KBP_DEBUG_GROUP_EXIT", // = 3,
  "KM_KBP_DEBUG_RULE_ENTER", // = 4,
  "KM_KBP_DEBUG_RULE_EXIT", // = 5,
  "KM_KBP_DEBUG_MATCH_ENTER", // = 6,
  "KM_KBP_DEBUG_MATCH_EXIT", // = 7,
  "KM_KBP_DEBUG_NOMATCH_ENTER", // = 8,
  "KM_KBP_DEBUG_NOMATCH_EXIT", // = 9,
  "KM_KBP_DEBUG_END" // = 10,
};

void print_debug_item(const char *title, km_kbp_state_debug_item const & item) {
  LPGROUP gp = static_cast<LPGROUP>(item.kmx_info.group);
  LPKEY rule = static_cast<LPKEY>(item.kmx_info.rule);

  std::cout
    << "debug_item " << title << std::endl
    << "  type:    " << debug_item_types[item.type] << std::endl
    << "  flags:   " << item.flags << std::endl
    << "  key_info: (vk: " << item.key_info.vk
    <<    " mod:   " << item.key_info.modifier_state
    <<    " char:  " << item.key_info.character << ")" << std::endl;

  std::cout
    << "  kmx_info: "<< std::endl
    << "    context: '" << item.kmx_info.context << "'" << std::endl
    << "    action:  " << item.kmx_info.first_action << std::endl;

  if(gp) std::cout
    << "    group:   " << gp->dpName << std::endl;
  if(rule) std::cout
    << "    rule: " << std::endl
    << "      key:    " << rule->Key << std::endl
    << "      shift:  " << rule->ShiftFlags << std::endl
    << "      line:   " << rule->Line << std::endl;
  std::cout
    << "    offsets:  ";
  std::copy(
    std::begin(item.kmx_info.store_offsets),
    std::end(item.kmx_info.store_offsets),
    std::ostream_iterator<uint16_t>(std::cout, " ")
  );
  std::cout << std::endl;
}

bool are_store_offsets_equal(const uint16_t (&lhs)[DEBUG_STORE_OFFSETS_SIZE], const uint16_t (&rhs)[DEBUG_STORE_OFFSETS_SIZE]) {
  return std::equal(std::begin(lhs), std::end(lhs), std::begin(rhs));
  //  !memcmp(lhs, rhs, sizeof(lhs));
}

bool operator==(km_kbp_state_debug_item const & lhs,
                km_kbp_state_debug_item const & rhs)
{
  auto result = (lhs.type == rhs.type && lhs.flags == rhs.flags && lhs.kmx_info.first_action == rhs.kmx_info.first_action);
  if(result) {
    LPGROUP lgp = static_cast<LPGROUP>(lhs.kmx_info.group), rgp = static_cast<LPGROUP>(rhs.kmx_info.group);
    LPKEY lrule = static_cast<LPKEY>(lhs.kmx_info.rule), rrule = static_cast<LPKEY>(rhs.kmx_info.rule);
    switch(lhs.type) {
      case KM_KBP_DEBUG_BEGIN:
        result = lhs.key_info.character == rhs.key_info.character &&
                 lhs.key_info.modifier_state == rhs.key_info.modifier_state &&
                 lhs.key_info.vk == rhs.key_info.vk;
        break;
      case KM_KBP_DEBUG_END:
        break;
      case KM_KBP_DEBUG_GROUP_ENTER:
      case KM_KBP_DEBUG_GROUP_EXIT:
      case KM_KBP_DEBUG_MATCH_ENTER:
      case KM_KBP_DEBUG_MATCH_EXIT:
      case KM_KBP_DEBUG_NOMATCH_ENTER:
      case KM_KBP_DEBUG_NOMATCH_EXIT:
        assert(lgp != nullptr);
        assert(rgp != nullptr);
        assert(lgp->dpName != nullptr);
        assert(rgp->dpName != nullptr);
        result = !u16cmp(lgp->dpName, rgp->dpName);
        break;
      case KM_KBP_DEBUG_RULE_ENTER:
      case KM_KBP_DEBUG_RULE_EXIT:
        assert(lgp != nullptr);
        assert(rgp != nullptr);
        assert(lgp->dpName != nullptr);
        assert(rgp->dpName != nullptr);
        assert(lrule != nullptr);
        assert(rrule != nullptr);
        result = !u16cmp(lgp->dpName, rgp->dpName) &&
          lrule->Line == rrule->Line &&
          lrule->Key == rrule->Key &&
          lrule->ShiftFlags == rrule->ShiftFlags &&
          u16cmp(lhs.kmx_info.context, rhs.kmx_info.context) == 0 &&
          are_store_offsets_equal(lhs.kmx_info.store_offsets, rhs.kmx_info.store_offsets);
        break;
      default:
        assert(false);
        result = false;
    }
  }

  if(!result) {
    print_debug_item("actual", lhs);
    print_debug_item("expected", rhs);
  }
  return result;
}


bool debug_items(km_kbp_state const * state,
                  std::initializer_list<km_kbp_state_debug_item> const & expected)
{
  size_t n = 0;
  auto act = km_kbp_state_debug_items(state, &n);

  for (auto &rhs: expected) {
    if(--n < 0) {
      std::cout << "expected longer than actual" << std::endl;
      return false;
    }
    if (!(*act++ == rhs)) return false;
  }

  if(n != 0) {
    std::cout << "actual longer than expected" << std::endl;
    return false;
  }

  return true;
}

  km_kbp_option_item test_env_opts[] =
  {
    KM_KBP_OPTIONS_END
  };

} // namespace


// TODO: consider using same const approach in kmx_base.h
//       but this ripples through a whole lot of code
typedef struct tagDEBUG_GROUP
{
  KMX_WCHAR const * dpName;
  KEY const * dpKeyArray;   // [LPKEY] address of first item in key array
  KMX_WCHAR const * dpMatch;
  KMX_WCHAR const * dpNoMatch;
  KMX_DWORD cxKeyArray;   // in array entries
  KMX_BOOL  fUsingKeys;   // group(xx) [using keys] <-- specified or not
} DEBUG_GROUP, *LPDEBUG_GROUP;

typedef struct tagDEBUG_KEY
{
  KMX_WCHAR Key;
  KMX_DWORD Line;
  KMX_DWORD ShiftFlags;
  KMX_WCHAR const * dpOutput;
  KMX_WCHAR const * dpContext;
} DEBUG_KEY, *LPDEBUG_KEY;


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
    km_kbp_state_debug_item{KM_KBP_DEBUG_END}
  }));

  try_status(km_kbp_context_set(km_kbp_state_context(test_state), citems));
}

/**
 * Test 1: Start with debugging disabled
 */
void test_debugging_disabled() {
  setup("000 - null keyboard.kmx");
  try_status(km_kbp_state_debug_set(test_state, 0));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_END}
  }));
}

/**
 * Test 2: Debugging enabled, no rule match
 */
void test_debugging_no_rule_match() {
  setup("000 - null keyboard.kmx");
  DEBUG_GROUP gp = {u"Main"};
  try_status(km_kbp_state_debug_set(test_state, 1));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_S, KM_KBP_MODIFIER_SHIFT, 'S'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}
  }));
  // assert(action_items(test_state, {
//TODO
  // }));
}

/**
 * Test 3: Debugging enabled, function key pressed, no rule match
 */
void test_debugging_function_key() {
  setup("000 - null keyboard.kmx");
  DEBUG_GROUP gp = {u"Main"};
  try_status(km_kbp_state_debug_set(test_state, 1));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_F1, 0));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_F1, 0, 0}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, KM_KBP_DEBUG_FLAG_OUTPUTKEYSTROKE, {}, {u"", nullptr, nullptr, {}, 1}}
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

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_D, KM_KBP_MODIFIER_SHIFT));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_D, KM_KBP_MODIFIER_SHIFT, 'D'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}, // action item will emit a default 'D'
  }));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_E, KM_KBP_MODIFIER_SHIFT));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_E, KM_KBP_MODIFIER_SHIFT, 'E'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, KM_KBP_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}, // action item will emit a default 'E'
  }));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_F, KM_KBP_MODIFIER_SHIFT));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_F, KM_KBP_MODIFIER_SHIFT, 'F'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"DE", &gp, &kp, {0xFFFF}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"DE", &gp, &kp, {0xFFFF}, 5 /* bksp bksp E04 E05 E06 */ }},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 5 /* from above */ }},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 5 /* from above */ }},
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

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_1, 0));
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

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_2, 0));
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

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_A, 0));
  assert(debug_items(test_state, {
    km_kbp_state_debug_item{KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_FLAG_UNICODE, {KM_KBP_VKEY_A, 0, 'a'}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kpa, {0xFFFF}}},
      km_kbp_state_debug_item{KM_KBP_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kpa, {0xFFFF}, 4}},

    km_kbp_state_debug_item{KM_KBP_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 4}},
    km_kbp_state_debug_item{KM_KBP_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 4}}, // action item will emit a 'exay'
  }));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_B, 0));
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

  arg_path = argv[arg_color ? 2 : 1];

  test_debugging_disabled();
  test_debugging_no_rule_match();
  test_debugging_function_key();
  test_basic_rule_matches();
  test_multiple_groups();
  test_store_offsets();

  // Destroy them
  teardown();


  return 0;
}
