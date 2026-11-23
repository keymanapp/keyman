/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Tim Eves on 2018-10-30
 *
 * Tests for the Keyboard API family of functions.
 */

#include <string>

#include "keyman_core.h"
#include "path.hpp"
#include "mock/mock_processor.hpp"
#include "../../../src/kmx/kmx_xstring.h" //u16cmp
#include <gtest/gtest.h>

km::core::path const test_kb_path = "/a/dummy/keyboard.mock";

TEST(KeyboardApi, PassesBasicSniffTestWithMockProcessor) {
  km_core_keyboard * test_kb = nullptr;
  km_core_keyboard_attrs const * kb_attrs = nullptr;
  km_core_keyboard_key * kb_key_list = nullptr;
  km_core_keyboard_imx * kb_imx_list = nullptr;

  test_kb = (km_core_keyboard *)new km::core::mock_processor(test_kb_path);
  EXPECT_EQ(km_core_keyboard_get_attrs(test_kb, &kb_attrs), KM_CORE_STATUS_OK);
  EXPECT_EQ(km_core_keyboard_get_key_list(test_kb,&kb_key_list), KM_CORE_STATUS_OK);
  EXPECT_EQ(km_core_keyboard_get_imx_list(test_kb,&kb_imx_list), KM_CORE_STATUS_OK);

  // Note: 3.145 is a hard-coded version string in mock_processor
  EXPECT_EQ(km::core::kmx::u16cmp(kb_attrs->version_string, u"3.145"), 0);

  // cleanup
  km_core_keyboard_dispose(test_kb);
  km_core_keyboard_key_list_dispose(kb_key_list);
  km_core_keyboard_imx_list_dispose(kb_imx_list);
}

