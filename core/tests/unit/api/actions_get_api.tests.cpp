/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Marc Durdin on 2024-01-29
 *
 * Keyman Core - Tests for the kmn_core_state_get_actions API
 */

#include <string>
#include <memory>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"
#include "context.hpp"

#include "../helpers/core_test_helpers.h"
#include "./actions_test_data.h"

class GetActionApiTest : public testing::TestWithParam<ActionsTestData> {
protected:
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_actions * test_actions = nullptr;

  void Initialize(ActionsTestData const& data) {
    if(data.final_cached_context_string == nullptr) {
      // We skip the shared tests that don't have a input context string
      return;
    }

    km::core::path path = km::core::path::join(test_dir, "..", "ldml", "fixtures", "keyboards", "17.0", "k_001_tiny.kmx");
    auto blob = km::tests::load_kmx_file(path.native().c_str());
    ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
    ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));

    ASSERT_STATUS_OK(set_context_from_string(km_core_state_context(test_state), data.final_cached_context_string));
    ASSERT_STATUS_OK(set_context_from_string(km_core_state_app_context(test_state), data.initial_app_context));

    test_actions = new km_core_actions;
    test_actions->code_points_to_delete = data.actions_code_points_to_delete;
    std::unique_ptr<km_core_usv[]> buf(new km_core_usv[data.actions_output.length() + 1]);
    data.actions_output.copy(buf.get(), data.actions_output.length());
    // terminate the buffer
    buf.get()[data.actions_output.length()] = 0;
    test_actions->output = buf.release();

    test_actions->persist_options = new km_core_option_item[1];
    test_actions->persist_options[0] = KM_CORE_OPTIONS_END;
    test_actions->do_alert = KM_CORE_FALSE;
    test_actions->emit_keystroke = KM_CORE_FALSE;
    test_actions->new_caps_lock_state = KM_CORE_CAPS_UNCHANGED;

    test_actions->deleted_context = nullptr;
    test_state->set_actions(*test_actions);
    test_state->actions().commit();
    test_state->apply_actions_and_merge_app_context();
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
    if(test_actions) {
      delete [] test_actions->output;
      delete [] test_actions->persist_options;
      delete test_actions;
      test_actions = nullptr;
    }
  }
};

//-------------------------------------------------------------------------------------

/**
 * Run a single test on actions_normalize. This is quite nuanced, because the
 * input state is more than a little complex. We have inputs in NFU and NFD, and
 * outputs counting NFU and inserting NFC. Be careful!
 */
TEST_P(GetActionApiTest, TestActionsApi) {
  auto data = GetParam();
  ASSERT_NO_FATAL_FAILURE(Initialize(data));

  if(data.final_cached_context_string == nullptr) {
    GTEST_SKIP() << "Skip the shared tests that don't have a input context string";
    return;
  }

  auto actual_actions = km_core_state_get_actions(test_state);

  std::cout << " (" << data.test_name << "): delete: " << data.expected_delete << " output: |" << std::u32string(actual_actions->output) << "|" << std::endl;
  std::u32string o(actual_actions->output);
  for(auto i = o.begin(); i < o.end(); i++) {
    std::cout << " U+" << std::hex << (int)(*i);
  }
  std::cout << std::endl;

  std::cout << " deleted_context: " << std::u32string(actual_actions->deleted_context) << std::endl;
  std::u32string dc(actual_actions->deleted_context);
  for(auto i = dc.begin(); i < dc.end(); i++) {
    std::cout << " U+" << std::hex << (int)(*i);
  }
  std::cout << std::endl;

  ASSERT_EQ(actual_actions->code_points_to_delete, data.expected_delete);
  ASSERT_EQ(actual_actions->output, data.expected_output);
  ASSERT_EQ(actual_actions->deleted_context, data.expected_deleted_context);

  auto actual_final_app_context = get_context_as_string(km_core_state_app_context(test_state));
  auto actual_final_app_context_string = std::u16string(actual_final_app_context);
  auto expected_final_app_context_string = std::u16string(data.expected_final_app_context);
  std::cout << " final app context: actual: |" << actual_final_app_context_string << "| expected: |" << expected_final_app_context_string << "|" << std::endl;
  ASSERT_EQ(actual_final_app_context_string, expected_final_app_context_string);
  delete [] actual_final_app_context;
}

// Note that .final_cached_context_items is not used in these tests

INSTANTIATE_TEST_SUITE_P(KeymanCore, GetActionApiTest, testing::ValuesIn(actionsTestData), GenerateTestName);
