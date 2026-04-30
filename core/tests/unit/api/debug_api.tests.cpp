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


#include "keyman_core.h"

#include "path.hpp"
#include "state.hpp"
#include "kmx/kmx_base.h"

#include "../helpers/core_test_helpers.h"

using namespace km::core::kmx;

class DebugApiTests : public testing::Test {
protected:
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_context_item * citems = nullptr;

  void Initialize(const char *keyboard) {
    km::core::path path = km::core::path(test_dir / ".." / "kmx" / keyboard);
    auto blob = km::tests::load_kmx_file(path.native().c_str());
    ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
    ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));
    ASSERT_STATUS_OK(context_items_from_utf16(u"Hello 😁", &citems));

    // Pre-test sanity: ensure debugging is disabled
    ASSERT_EQ(km_core_state_debug_get(test_state), 0);

    // Ensure the pre-run debug item state is not empty
    ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
      km_core_state_debug_item{KM_CORE_DEBUG_END, {}, {}, {}}
    }));

    ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));
    ASSERT_STATUS_OK(km_core_context_set(km_core_state_app_context(test_state), citems));
  }

  void TearDown() override {
    if(citems) {
      km_core_context_items_dispose(citems);
      citems = nullptr;
    }
    if(test_state) {
      km_core_state_dispose(test_state);
      test_state = nullptr;
    }
    if(test_kb) {
      km_core_keyboard_dispose(test_kb);
      test_kb = nullptr;
    }
  }
};

/**
 * Test 1: Start with debugging disabled
 */
TEST_F(DebugApiTests, StartWithDebuggingDisabled) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx"));
  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 0));
  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_S, KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_END}
  }));

  EXPECT_TRUE(km::tests::action_items(test_state, {
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('S')}},
    {KM_CORE_IT_END}
  }));
}

/**
 * Test 2: Debugging enabled, no rule match
 */
TEST_F(DebugApiTests, NoRuleMatches) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx"));

  km::tests::DEBUG_GROUP gp = {u"Main"};
  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));
  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_S, KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_S, KM_CORE_MODIFIER_SHIFT, 'S'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, KM_CORE_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}
  }));

  EXPECT_TRUE(km::tests::action_items(test_state, {
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('S')}},
    {KM_CORE_IT_END}
  }));
}

/**
 * Test 3: Debugging enabled, function key pressed, no rule match
 */
TEST_F(DebugApiTests, FunctionKeyNoRuleMatches) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx"));
  km::tests::DEBUG_GROUP gp = {u"Main"};
  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));
  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_F1, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_F1, 0, 0}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, KM_CORE_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 0}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, KM_CORE_DEBUG_FLAG_OUTPUTKEYSTROKE, {}, {u"", nullptr, nullptr, {}, 0}}
  }));

  EXPECT_TRUE(km::tests::action_items(test_state, {
    {KM_CORE_IT_INVALIDATE_CONTEXT}, // It's a non character key that is not a modifier, so this is a hint that context may no longer be valid
    {KM_CORE_IT_EMIT_KEYSTROKE},
    {KM_CORE_IT_END}
  }));
}

/**
 * Test 4: basic rule match
 */
TEST_F(DebugApiTests, BasicRuleMatches) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0101___basic_input_unicode.kmx"));
  km::tests::DEBUG_GROUP gp = {u"Main"};
  km::tests::DEBUG_KEY kp = { 'F', /*line*/17, /*shift*/0 }; // vkey is a char
  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));

  // 'DE' + 'F' > U+0E04 U+0E05 U+0E06

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_D, KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_D, KM_CORE_MODIFIER_SHIFT, 'D'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, KM_CORE_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}, // action item will emit a default 'D'
  }));

  EXPECT_TRUE(km::tests::action_items(test_state, {
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('D')}},
    {KM_CORE_IT_END}
  }));

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_E, KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_E, KM_CORE_MODIFIER_SHIFT, 'E'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, KM_CORE_DEBUG_FLAG_NOMATCH, {}, {u"", &gp, nullptr, {}, 1}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}}, // action item will emit a default 'E'
  }));

  EXPECT_TRUE(km::tests::action_items(test_state, {
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('E')}},
    {KM_CORE_IT_END}
  }));

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_F, KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_F, KM_CORE_MODIFIER_SHIFT, 'F'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"DE", &gp, &kp, {0xFFFF}}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"DE", &gp, &kp, {0xFFFF}, 5 /* bksp bksp E04 E05 E06 */ }},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 5 /* from above */ }},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 5 /* from above */ }},
  }));

  km_core_action_item bksp_d = {KM_CORE_IT_BACK};
  bksp_d.backspace.expected_type = KM_CORE_BT_CHAR;
  bksp_d.backspace.expected_value = 'D';

  km_core_action_item bksp_e = {KM_CORE_IT_BACK};
  bksp_e.backspace.expected_type = KM_CORE_BT_CHAR;
  bksp_e.backspace.expected_value = 'E';

  EXPECT_TRUE(km::tests::action_items(test_state, {
    bksp_e,
    bksp_d,
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv(u'\u0E04')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv(u'\u0E05')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv(u'\u0E06')}},
    {KM_CORE_IT_END}
  }));

}

/**
 * Test 5: Multiple groups
 */
TEST_F(DebugApiTests, MultipleGroups) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0401___multiple_groups.kmx"));
  km::tests::DEBUG_GROUP gp = {u"Main"}, gpa = {u"a"}, gpb = {u"b"};
  km::tests::DEBUG_KEY kp1 = { KM_CORE_VKEY_1, /*line*/13, /*shift*/KM_CORE_MODIFIER_VIRTUALKEY },
            kp2 = { KM_CORE_VKEY_2, /*line*/14, /*shift*/KM_CORE_MODIFIER_VIRTUALKEY },
            kpa = { 0, /*line*/20, /*shift*/0 },
            kpb = { 0, /*line*/24, /*shift*/0 };

  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));

  // '12' -> 'abc'

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_1, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_1, 0, '1'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kp1, {0xFFFF}}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kp1, {0xFFFF}, 1}},

      km_core_state_debug_item{KM_CORE_DEBUG_MATCH_ENTER, 0, {}, {u"", &gp, nullptr, {}, 1}},
        km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gpa, nullptr, {}, 1}},
          km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"a", &gpa, &kpa, {0xFFFF}, 1}},
          km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"a", &gpa, &kpa, {0xFFFF}, 3}},
        km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gpa, nullptr, {}, 3}},
      km_core_state_debug_item{KM_CORE_DEBUG_MATCH_EXIT, 0, {}, {u"", &gp, nullptr, {}, 3}},

    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 3}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 3}}, // action item will emit a 'b'
  }));

  km_core_action_item bksp_a = {KM_CORE_IT_BACK};
  bksp_a.backspace.expected_type = KM_CORE_BT_CHAR;
  bksp_a.backspace.expected_value = 'a';

  EXPECT_TRUE(km::tests::action_items(test_state, {
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('a')}},
    bksp_a,
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('b')}},
    {KM_CORE_IT_END}
  }));

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_2, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_2, 0, '2'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"b", &gp, &kp2, {0xFFFF}}},
        km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gpb}},
          km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"b", &gpb, &kpb, {0xFFFF}}},
          km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"b", &gpb, &kpb, {0xFFFF}, 4}},
        km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gpb, nullptr, {}, 4}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"b", &gp, &kp2, {0xFFFF}, 4}},

      //See #5440 for why match does not fire here:
      //km_core_state_debug_item{KM_CORE_DEBUG_MATCH_ENTER, 0, {}, {u"", &gp}},
      //  km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gpa}},
      //  km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, KM_CORE_DEBUG_FLAG_NOMATCH, {}, {u"", &gpa}},
      //km_core_state_debug_item{KM_CORE_DEBUG_MATCH_EXIT, 0, {}, {u"", &gp}},

    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 4}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 4}}, // action item will "abc"
  }));

  km_core_action_item bksp_b = {KM_CORE_IT_BACK};
  bksp_b.backspace.expected_type = KM_CORE_BT_CHAR;
  bksp_b.backspace.expected_value = 'b';

  EXPECT_TRUE(km::tests::action_items(test_state, {
    bksp_b,
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('a')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('b')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('c')}},
    {KM_CORE_IT_END}
  }));

}

/**
 * Test 6: store offsets
 */
TEST_F(DebugApiTests, StoreOffsets) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0803___if_and_context.kmx"));
  km::tests::DEBUG_GROUP gp = {u"Main"};
  km::tests::DEBUG_KEY kpa = { 'a', /*line*/16, },
            kpb = { 'b', /*line*/20, };

  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));

  // 'ab' -> 'ex'

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_A, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_A, 0, 'a'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kpa, {0xFFFF}}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kpa, {0xFFFF}, 4}},

    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 4}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 4}}, // action item will emit a 'exay'
  }));

  EXPECT_TRUE(km::tests::action_items(test_state, {
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('e')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('x')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('a')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('y')}},
    {KM_CORE_IT_END}
  }));

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_B, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_B, 0, 'b'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      // store_offsets: 6 = store #, 1 = index into store, 6 = store #, 0 = index into store
      // store: store(diaeresisBase) 'ae'
      // context: exay
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"exay", &gp, &kpb, {6, 1, 6, 0, 0xFFFF}}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"exay", &gp, &kpb, {6, 1, 6, 0, 0xFFFF}, 6}},

    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 6}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 6}},
  }));

  km_core_action_item bksp[] = {
      {KM_CORE_IT_BACK},
      {KM_CORE_IT_BACK},
      {KM_CORE_IT_BACK},
      {KM_CORE_IT_BACK}
  };
  bksp[0].backspace.expected_type = KM_CORE_BT_CHAR;
  bksp[1].backspace.expected_type = KM_CORE_BT_CHAR;
  bksp[2].backspace.expected_type = KM_CORE_BT_CHAR;
  bksp[3].backspace.expected_type = KM_CORE_BT_CHAR;
  bksp[0].backspace.expected_value = 'y';
  bksp[1].backspace.expected_value = 'a';
  bksp[2].backspace.expected_value = 'x';
  bksp[3].backspace.expected_value = 'e';

  EXPECT_TRUE(km::tests::action_items(test_state, {
    bksp[0],
    bksp[1],
    bksp[2],
    bksp[3],
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('e')}},
    {KM_CORE_IT_CHAR, {0,}, {km_core_usv('x')}},
    {KM_CORE_IT_END}
  }));
}

/**
 * Test 7: set option
 */
TEST_F(DebugApiTests, SetOption) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0500___options.kmx"));
  km::tests::DEBUG_GROUP gp = {u"Main"};
  km::tests::DEBUG_KEY kp1 = { '1', /*line*/17, };
  km::tests::DEBUG_STORE sp = {0, u"foo", u"1"};

  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));

  // '1' -> set_option

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_1, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_1, 0, '1'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kp1, {0xFFFF}}},
      km_core_state_debug_item{KM_CORE_DEBUG_SET_OPTION, 0, {}, {u"", nullptr, nullptr, {}, 0, {&sp, u"1"}}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kp1, {0xFFFF}, 0}},

    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 0}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 0}},
  }));
}

/**
 * Test 8: save option
 */
TEST_F(DebugApiTests, SaveOption) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0502___options_with_save.kmx"));
  km::tests::DEBUG_GROUP gp = {u"Main"};
  km::tests::DEBUG_KEY kp1 = { '2', /*line*/21, };
  // DEBUG_STORE sp = {0, u"foo", u"0"};
  km_core_option_item opt = {u"foo", u"0", KM_CORE_OPT_KEYBOARD};

  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));

  // '2' -> save_option

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_2, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_2, 0, '2'}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},

      km_core_state_debug_item{KM_CORE_DEBUG_RULE_ENTER, 0, {}, {u"", &gp, &kp1, {0xFFFF}}},
      km_core_state_debug_item{KM_CORE_DEBUG_RULE_EXIT, 0, {}, {u"", &gp, &kp1, {0xFFFF}, 1}},

    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 0, {}, {u"", &gp, nullptr, {}, 1}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 0, {}, {u"", nullptr, nullptr, {}, 1}},
  }));

  km_core_action_item action = {KM_CORE_IT_PERSIST_OPT, {0,}, };
  action.option = &opt;

  EXPECT_TRUE(km::tests::action_items(test_state, {
    action,
    {KM_CORE_IT_END}
  }));
}

/*
 * Test 9: backspace + deadkey markers -- should delete two markers and then emit
 * an 'unknown' backspace because we are at start-of-context
 */
TEST_F(DebugApiTests, BackspaceMarkers) {
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx"));
  km_core_context_item marker_context[] = {
    {KM_CORE_CT_MARKER, {0,}, {1}},
    {KM_CORE_CT_MARKER, {0,}, {1}},
    {KM_CORE_CT_END}
  };
  km_core_context_item app_context[] = {
    {KM_CORE_CT_END}
  };
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), marker_context));
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_app_context(test_state), app_context));

  km::tests::DEBUG_GROUP gp = {u"Main"};

  ASSERT_STATUS_OK(km_core_state_debug_set(test_state, 1));

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_BKSP, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_NO_FATAL_FAILURE(km::tests::compare_debug_items(test_state, {
    km_core_state_debug_item{KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_FLAG_UNICODE, {KM_CORE_VKEY_BKSP, 0, 0}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_ENTER, 0, {}, {u"", &gp}},
    km_core_state_debug_item{KM_CORE_DEBUG_GROUP_EXIT, 2, {}, {u"", &gp, nullptr, {}, 2}},
    km_core_state_debug_item{KM_CORE_DEBUG_END, 1, {}, {u"", nullptr, nullptr, {}, 2}},
  }));

  km_core_action_item bksp = {KM_CORE_IT_BACK};
  bksp.backspace.expected_type = KM_CORE_BT_MARKER;
  bksp.backspace.expected_value = 1;

  EXPECT_TRUE(km::tests::action_items(test_state, {
    bksp,
    bksp,
    {KM_CORE_IT_INVALIDATE_CONTEXT},
    {KM_CORE_IT_EMIT_KEYSTROKE},
    {KM_CORE_IT_END}
  }));
}

/**
 * Note: this is just to print size of structures for cross-platform reference
 */
TEST(DebugApiMetaInformation, MetaPrintSizeofTest) {
  std::cout << std::endl;
  std::cout << "keyman_core_api.h:" << std::endl;
  std::cout << "sizeof(km_core_context_item): " << sizeof(km_core_context_item) << std::endl;
  std::cout << "sizeof(km_core_action_item): " << sizeof(km_core_action_item) << std::endl;
  std::cout << "sizeof(km_core_option_item): " << sizeof(km_core_option_item) << std::endl;
  std::cout << "sizeof(km_core_keyboard_attrs): " << sizeof(km_core_keyboard_attrs) << std::endl;
  std::cout << "sizeof(km_core_attr): " << sizeof(km_core_attr) << std::endl;
  std::cout << std::endl;
  std::cout << "keyman_core_api_debug.h:" << std::endl;
  std::cout << "sizeof(km_core_state_debug_item): " << sizeof(km_core_state_debug_item) << std::endl;
  std::cout << "sizeof(km_core_state_debug_key_info): " << sizeof(km_core_state_debug_key_info) << std::endl;
  std::cout << "sizeof(km_core_state_debug_kmx_info): " << sizeof(km_core_state_debug_kmx_info) << std::endl;
  std::cout << "sizeof(km_core_state_debug_kmx_option_info): " << sizeof(km_core_state_debug_kmx_option_info) << std::endl;
  std::cout << "sizeof([enum] km_core_debug_type): " << sizeof(km_core_debug_type) << std::endl;
  SUCCEED();
}
