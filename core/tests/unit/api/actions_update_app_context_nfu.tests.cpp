/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Marc Durdin on 2023-10-23
 *
 * Keyman Core - Tests for NFU context normalization API functions
 */

#include <string>
#include <memory>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"
#include "context.hpp"

#include "../helpers/core_test_helpers.h"

// TODO-WEB-CORE: merge with actions_normalize.tests.cpp? These are identical;
// note that actions_get_api and actions_set_api data are subtly different,
// opportunity to merge those too?

struct TestData {
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
};

std::string GenerateTestName(const testing::TestParamInfo<TestData>& info) {
  return info.param.test_name;
}

class ActionsUpdateAppContextNfuApiTest : public testing::TestWithParam<TestData> {
protected:
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_actions test_actions = {0};

  void Initialize(TestData const& data) {
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
 * Run a single test on actions_update_app_context_nfu. This is simpler than
 * actions_normalize -- everything stays in NFU, and all we need to verify is
 * that the result has no markers and all chars are the same as input.
 */

TEST_P(ActionsUpdateAppContextNfuApiTest, TestActionsUpdateAppContextNfu) {
  auto data = GetParam();
  ASSERT_NO_FATAL_FAILURE(Initialize(data));

  ASSERT_TRUE(km::core::actions_update_app_context_nfu(km_core_state_context(test_state), km_core_state_app_context(test_state)));

  std::cout << "test_actions_update_app_context_nfu: (" << data.test_name << "): delete: " << data.expected_delete << " output: |" << std::u32string(test_actions.output) << "|" << std::endl;
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

const km_core_context_item items_1[] = { //u"a\U0001F607b\uFFFF\u0008\u0001ca\U0001F60E",
  { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x1F607 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x0062 } },
  { KM_CORE_CT_MARKER, {0,}, { 0x1 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x0063 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x1F60E } },
  KM_CORE_CONTEXT_ITEM_END
};

const km_core_context_item items_2[] = { //u"a\U0001F607bca\U0001F60E\uFFFF\u0008\u0001",
  { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x1F607 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x0062 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x0063 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
  { KM_CORE_CT_CHAR,   {0,}, { 0x1F60E } },
  { KM_CORE_CT_MARKER, {0,}, { 0x1 } },
  KM_CORE_CONTEXT_ITEM_END
};

const TestData values[] = {
  // Null boundary tests

  {
    "Noop",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u""
  },

  {
    "NoOutput",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u"abc"
  },

  {
    "NoContext",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"def",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* action del, output: */            0, U"def",
    /* app_context: */                   u"def"
  },

  // surrogate pair tests

  {
    "SurrogatePairInContext",
    /* app context pre transform: */     u"abc\U0001F607ê",
    /* cached context post transform: */ u"abc\U0001F607a\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"a\u0323\u0302",
    // ---- results ----
    /* action del, output: */            1, U"a\u0323\u0302",
    /* app_context: */                   u"abc\U0001F607a\u0323\u0302"
  },

  {
    "SurrogatePairInOutput",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc\U0001F607",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\U0001F607",
    // ---- results ----
    /* action del, output: */            0, U"\U0001F607",
    /* app_context: */                   u"abc\U0001F607"
  },

  {
    "SurrogatePairsInBothContextAndOutput",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ u"a\U0001F607bca\U0001F60E",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  },

  // Marker tests

  {
    "AMarkerInTheCachedContextShouldNotShowUpInAppContext",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_1[0],
    /* action del, output: */            1, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  },

  {
    "AMarkerInTheModifiedSectionOfCachedContextShouldNotShowUpInAppContext",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_2[0],
    /* action del, output: */            1, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  }
};

INSTANTIATE_TEST_SUITE_P(KeymanCore, ActionsUpdateAppContextNfuApiTest, testing::ValuesIn(values), GenerateTestName);
