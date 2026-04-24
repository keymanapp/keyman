/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Tim Eves on 2018-10-30
 *
 * Tests for the context API family of functions.
 */
#include <cstdlib>
#include <string>

#include "keyman_core.h"

#include "option.hpp"
#include "state.hpp"
#include "../../../src/kmx/kmx_xstring.h" //u16cmp
#include "mock/mock_processor.hpp"
#include <gtest/gtest.h>

km::core::path const test_kb_path = "/a/dummy/keyboard.mock";

TEST(km_core_options, verify_list_size) {
  km_core_option_item const api_mock_options[] = {
    {u"hello",   u"world", KM_CORE_OPT_KEYBOARD},
    {u"isdummy",     u"yes", KM_CORE_OPT_ENVIRONMENT},
    {u"testing",     u"maybe", KM_CORE_OPT_ENVIRONMENT},
    KM_CORE_OPTIONS_END
  };

  EXPECT_EQ(km_core_options_list_size(api_mock_options), 3);
}

TEST(km_core_options, test_empty_list) {
  km::core::mock_processor mock_processor(test_kb_path);

  km_core_option_item const empty_options_list[] = {KM_CORE_OPTIONS_END};

  // Simple sanity test on an empty options list.
  EXPECT_EQ(km_core_options_list_size(empty_options_list), 0);

  km_core_state empty_state(mock_processor, empty_options_list);
  km_core_cu const *value;
  EXPECT_EQ(km_core_state_option_lookup(&empty_state, KM_CORE_OPT_ENVIRONMENT, u"isdummy", &value), KM_CORE_STATUS_KEY_ERROR);
}

#define expect_opt_matches(state, scope, key, expected) {\
  km_core_cu const * actual = nullptr; \
  auto s = km_core_state_option_lookup(state, scope, key, &actual); \
  ASSERT_EQ(s, KM_CORE_STATUS_OK); \
  EXPECT_EQ(std::u16string(actual), std::u16string(expected)); \
}

TEST(km_core_options, test_setting_state) {
  km::core::mock_processor mock_processor(test_kb_path);

  km_core_option_item const test_env[] = {
    {u"test_env_option",   u"yes", KM_CORE_OPT_ENVIRONMENT},
    {u"testing",   u"maybe", KM_CORE_OPT_ENVIRONMENT},
    KM_CORE_OPTIONS_END
  };

  km_core_option_item const new_opts[] = {
    {u"test_env_option",   u"no", KM_CORE_OPT_ENVIRONMENT},
    {u"test_keyboard_option",     u"globe", KM_CORE_OPT_KEYBOARD},
    KM_CORE_OPTIONS_END
  };

  km_core_state test_state(mock_processor, test_env);

  expect_opt_matches(&test_state, KM_CORE_OPT_ENVIRONMENT, u"test_env_option", u"yes");
  expect_opt_matches(&test_state, KM_CORE_OPT_KEYBOARD, u"test_keyboard_option", u"default_value");
  ASSERT_EQ(km_core_state_options_update(&test_state, new_opts), KM_CORE_STATUS_OK);
  expect_opt_matches(&test_state, KM_CORE_OPT_ENVIRONMENT, u"test_env_option", u"no");
  expect_opt_matches(&test_state, KM_CORE_OPT_KEYBOARD, u"test_keyboard_option", u"globe");

  km_core_option_item bad_scope_opts[] = {
    {u"hello",   u"!", KM_CORE_OPT_UNKNOWN},
    KM_CORE_OPTIONS_END
  };

  // The option "hello" has a scope KM_CORE_OPT_UNKNOWN, which is not permitted
  EXPECT_EQ(km_core_state_options_update(&test_state, bad_scope_opts), KM_CORE_STATUS_INVALID_ARGUMENT);
}

GTEST_API_ int
main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
