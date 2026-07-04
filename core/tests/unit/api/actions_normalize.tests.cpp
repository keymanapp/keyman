/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Marc Durdin on 2023-10-23
 *
 * Keyman Core - Tests for the context normalization APIs
 */

#include <string>
#include <memory>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"
#include "context.hpp"

#include "../helpers/core_test_helpers.h"
#include "./actions_test_data.h"

class ActionsNormalizeApiTest : public testing::TestWithParam<ActionsTestData> {
protected:
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_actions test_actions = {0};

  void Initialize(ActionsTestData const& data) {
    km::core::path path = km::core::path::join(test_dir, "..", "ldml", "fixtures", "keyboards", "17.0", "k_001_tiny.kmx");
    auto blob = km::tests::load_kmx_file(path.native().c_str());
    ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
    ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));

    if(data.final_cached_context_string) {
      ASSERT_STATUS_OK(set_context_from_string(km_core_state_context(test_state), data.final_cached_context_string));
    } else {
      // has markers
      ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), data.final_cached_context_items));
    }
    ASSERT_STATUS_OK(set_context_from_string(km_core_state_app_context(test_state), data.initial_app_context));

    memset(&test_actions, 0, sizeof(km_core_actions));
    test_actions.code_points_to_delete = data.actions_code_points_to_delete;
    std::unique_ptr<km_core_usv[]> buf(new km_core_usv[data.actions_output.length() + 1]);
    data.actions_output.copy(buf.get(), data.actions_output.length());
    // terminate the buffer
    buf.get()[data.actions_output.length()] = 0;
    test_actions.output = buf.release();
    test_actions.deleted_context = nullptr;
  }

  void TearDown() override {
    if(test_state) {
      km_core_state_dispose(test_state);
      test_state = nullptr;
    }
    if(test_kb) {
      km_core_keyboard_dispose(test_kb);
      test_kb = nullptr;
    }
    km::core::actions_dispose(test_actions);
  }
};

//-------------------------------------------------------------------------------------

/**
 * Run a single test on actions_normalize. This is quite nuanced, because the
 * input state is more than a little complex. We have inputs in NFU and NFD, and
 * outputs counting NFU and inserting NFC. Be careful!
 */
TEST_P(ActionsNormalizeApiTest, TestActionsNormalize) {
  auto data = GetParam();
  ASSERT_NO_FATAL_FAILURE(Initialize(data));

  ASSERT_TRUE(km::core::actions_normalize(km_core_state_context(test_state), km_core_state_app_context(test_state), test_actions));

  std::cout << "test_actions_normalize: (" << data.test_name << "): delete: " << test_actions.code_points_to_delete << " output: |" << std::u32string(test_actions.output) << "|" << std::endl;
  std::u32string o(test_actions.output);
  for(auto i = o.begin(); i < o.end(); i++) {
    std::cout << "U+" << std::hex << (int)(*i) << " ";
  }
  std::cout << std::endl;

  ASSERT_EQ(test_actions.code_points_to_delete, data.expected_delete);
  ASSERT_EQ(test_actions.output, data.expected_output);

  auto debug = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_APP);
  std::cout << " final app context: " << debug << std::endl;
  km_core_cu_dispose(debug);

  ASSERT_NO_FATAL_FAILURE(km::tests::compare_context(km_core_state_app_context(test_state), data.expected_final_app_context));
}

INSTANTIATE_TEST_SUITE_P(KeymanCore, ActionsNormalizeApiTest, testing::ValuesIn(actionsTestData), GenerateTestName);
