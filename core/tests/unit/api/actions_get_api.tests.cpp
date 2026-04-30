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
  const km_core_cu *final_cached_context;

  /**
   * number of NFD code points that the keyboard processor has asked to remove
   * in its actions
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

std::string GenerateTestName(const testing::TestParamInfo<TestData>& info) {
  return info.param.test_name;
}

class GetActionApiTest : public testing::TestWithParam<TestData> {
protected:
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_actions * test_actions = nullptr;

  void Initialize(TestData const& data) {
    km::core::path path = km::core::path::join(test_dir, "..", "ldml", "fixtures", "keyboards", "17.0", "k_001_tiny.kmx");
    auto blob = km::tests::load_kmx_file(path.native().c_str());
    ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
    ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));

    ASSERT_STATUS_OK(set_context_from_string(km_core_state_context(test_state), data.final_cached_context));
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

  // setup(initial_app_context, final_cached_context, actions_code_points_to_delete, actions_output);

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

const TestData values[] = {
  // Null boundary tests

  {
    "Noop",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"",
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u"",
    /* expected del */                   U""
  },

  {
    "NoOutput",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc",
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u"abc",
    /* expected del */                   U""
  },

  {
    "NoContext",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"def",
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* action del, output: */            0, U"def",
    /* app_context: */                   u"def",
    /* expected del */                   U""
  },

  // Simple tests -- no deletions involved

  {
    "NoNormalization",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcdef",
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* action del, output: */            0, U"def",
    /* app_context: */                   u"abcdef",
    /* expected del */                   U""
  },

  {
    "OutputToNfcBasic",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcde\u0300f",
    /* action del, output: */            0, U"de\u0300f",
    // ---- results ----
    /* action del, output: */            0, U"dèf",
    /* app_context: */                   u"abcdèf",
    /* expected del */                   U""
  },

  {
    "OutputToNfcHefty",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcA\u0300" u"e\u0316\u0301" u"\u0073\u0323\u0307"  u"\u0041\u030a"     u"\U000114B9\U000114B0",
    /* action del, output: */            0, U"A\u0300" U"e\u0316\u0301" U"\u0073\u0323\u0307"  U"\u0041\u030a"     U"\U000114B9\U000114B0",
    // ---- results ----
    /* action del, output: */            0, U"À"       U"é̖"             U"\u1e69"              U"\u00c5"           U"\U000114BC",
    /* app_context: */                   u"abcÀé̖\u1e69\u00c5\U000114BC",
    /* expected del */                   U""
  },

  // Interaction with input context when not on normalization boundary

  {
    "BacktrackOneCharacterToCombineAsNfc",
    /* app context pre transform: */     u"XYZA",
    /* cached context post transform: */ u"XYZA\u0300abc",
    /* action del, output: */            0, U"\u0300abc",
    // ---- results ----
    /* action del, output: */            1, U"Àabc",
    /* app_context: */                   u"XYZÀabc",
    /* expected del */                   U"A"
  },

  {
    "BacktrackECombCirc2CharsToCombineAsNfc",
    /* app context pre transform: */     u"abce\u0302",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* action del, output: */            1, U"\u0323\u0302",
    // ---- results ----
    /* action del, output: */            2, U"ệ",
    /* app_context: */                   u"abcệ",
    /* expected del */                   U"e\u0302"
  },

  {
    "OneBackspaceForNfdConvertsIntoOneCharInNfcAndRecombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* action del, output: */            1, U"\u0323\u0302",  // NFD input;  delete 1: \u0302
    // ---- results ----
    /* action del, output: */            1, U"ệ",             // NFC output; delete 1: ê
    /* app_context: */                   u"abcệ",
    /* expected del */                   U"ê"
  },

  // a\u0300 should not be normalized because it is not otherwise impacted by
  // the action.
  {
    "AvoidEditingTooFarBackInContextWhenFindingNormalizationBoundary",
    /* app context pre transform: */     u"a\u0300bcê",
    /* cached context post transform: */ u"a\u0300bce\u0323\u0302",
    /* action del, output: */            1, U"\u0323\u0302", // NFD input;  delete 1: \u0302
    // ---- results ----
    /* action del, output: */            1, U"ệ",              // NFC output; delete 1: ê
    /* app_context: */                   u"a\u0300bcệ",
    /* expected del */                   U"ê"
  },

  // If we don't reach a normalization boundary, we still should continue to work
  {
    "NormalizableLettersAtStartOfContext",
    /* app context pre transform: */     u"\u0300",
    /* cached context post transform: */ u"\u0323\u0300\u0302",
    /* action del, output: */            1, U"\u0323\u0300\u0302",       // NFD input;
    // ---- results ----
    /* action del, output: */            1, U"\u0323\u0300\u0302",  // NFC output is still decomposed because there is no base
    /* app_context: */                   u"\u0323\u0300\u0302",
    /* expected del */                   U"\u0300"
  },

  // Modifies the base as well as diacritic

  {
    "TwoBackspacesForNfdConvertsIntoOneCharInNfcAndRecombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abca\u0323\u0302",
    /* action del, output: */            2, U"a\u0323\u0302",  // NFD input;  delete 2: e\u0302
    // ---- results ----
    /* action del, output: */            1, U"ậ",             // NFC output; delete 1: ê
    /* app_context: */                   u"abcậ",
    /* expected del */                   U"ê"
  },

  // surrogate pair tests

  {
    "SurrogatePairInContext",
    /* app context pre transform: */     u"abc\U0001F607ê",
    /* cached context post transform: */ u"abc\U0001F607a\u0323\u0302",
    /* action del, output: */            2, U"a\u0323\u0302",
    // ---- results ----
    /* action del, output: */            1, U"ậ",
    /* app_context: */                   u"abc\U0001F607ậ",
    /* expected del */                   U"ê"
  },

  {
    "SurrogatePairInOutput",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc\U0001F607",
    /* action del, output: */            0, U"\U0001F607",
    // ---- results ----
    /* action del, output: */            0, U"\U0001F607",
    /* app_context: */                   u"abc\U0001F607",
    /* expected del */                   U""
  },

  {
    "SurrogatePairsInBothContextAndOutput",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ u"a\U0001F607bca\U0001F60E",
    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E",
    /* expected del */                   U"ê"
  }
};

INSTANTIATE_TEST_SUITE_P(KeymanCore, GetActionApiTest, testing::ValuesIn(values), GenerateTestName);
