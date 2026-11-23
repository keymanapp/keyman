#pragma once
#include <string>
#include <vector>
#include <gtest/gtest.h>
#include "keyman_core.h"

struct ActionsTestData {
  const char* test_name;

  /**
   * the app context stored in the state, _before_ transform is applied -- NFU
   */
  const km_core_cu *initial_app_context;

  /**
   * cached context _after_ actions have been applied -- guaranteed NFD
   * (essentially, this is initial_cached_context -
   * actions_code_points_to_delete + actions_output) - no markers supported
   */
  const km_core_cu *final_cached_context_string;

  /**
   * cached context _after_ actions have been applied -- guaranteed NFD
   * (essentially, this is initial_cached_context -
   * actions_code_points_to_delete + actions_output) - markers supported
   */
  const km_core_context_item *final_cached_context_items;

  /**
   * number of NFD code points that the keyboard processor has asked to remove in its actions
   */
  int actions_code_points_to_delete;

  /**
   * NFD string that the keyboard processor has asked to insert in its actions
   */
  const std::u32string actions_output;

  /**
   * expected: NFU code points to ask app to remove
   */
  const unsigned int expected_delete;

  /**
   * expected: adjusted NFC output to insert into the app
   */
  const std::u32string expected_output;

  /**
   * expected: NFU adjusted final app context, which will be NFC from the
   * boundary of the transform, but will not have been modified prior to that.
   * Should match char-for-char what the app ends up with in its text buffer.
   */
  const km_core_cu *expected_final_app_context;

  /**
   * expected: the characters deleted from the context
   */
  const std::u32string expected_deleted_context;
};

extern const std::vector<ActionsTestData> actionsTestData;

std::string GenerateTestName(const testing::TestParamInfo<ActionsTestData>& info);
