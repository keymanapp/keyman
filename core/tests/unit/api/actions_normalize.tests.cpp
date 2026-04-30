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

class ActionsNormalizeApiTest : public testing::TestWithParam<TestData> {
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

const km_core_context_item items_11067[] = {
  { KM_CORE_CT_CHAR,   {0,}, { U'𐒻' } },
  { KM_CORE_CT_CHAR,   {0,}, { U'𐒷' } },
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

  // Simple tests -- no deletions involved

  {
    "NoNormalization",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcdef",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* action del, output: */            0, U"def",
    /* app_context: */                   u"abcdef"
  },

  {
    "OutputToNfcBasic",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcde\u0300f",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"de\u0300f",
    // ---- results ----
    /* action del, output: */            0, U"dèf",
    /* app_context: */                   u"abcdèf"
  },

  {
    "OutputToNfcHefty",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcA\u0300" u"e\u0316\u0301" u"\u0073\u0323\u0307"  u"\u0041\u030a"     u"\U000114B9\U000114B0",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"A\u0300" U"e\u0316\u0301" U"\u0073\u0323\u0307"  U"\u0041\u030a"     U"\U000114B9\U000114B0",
    // ---- results ----
    /* action del, output: */            0, U"À"       U"é̖"             U"\u1e69"              U"\u00c5"           U"\U000114BC",
    /* app_context: */                   u"abcÀé̖\u1e69\u00c5\U000114BC"
  },

  // Interaction with input context when not on normalization boundary

  {
    "BacktrackOneCharacterToCombineAsNfc",
    /* app context pre transform: */     u"XYZA",
    /* cached context post transform: */ u"XYZA\u0300abc",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\u0300abc",
    // ---- results ----
    /* action del, output: */            1, U"Àabc",
    /* app_context: */                   u"XYZÀabc"
  },

  {
    "BacktrackEcombCirc2CharsToCombineAsNfc",
    /* app context pre transform: */     u"abce\u0302",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302",
    // ---- results ----
    /* action del, output: */            2, U"ệ",
    /* app_context: */                   u"abcệ"
  },

  {
    "OneBackspaceToDeleteLastNfdCharacter15487",
    /* app context pre transform: */     u"abcê", // NFC
    /* cached context post transform: */ u"abce",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"",  // NFD input;  delete 1: \u0302
    // ---- results ----
    /* action del, output: */            1, U"e",             // NFC output; delete 1: e
    /* app_context: */                   u"abce"
  },

  {
    "OneBackspaceForNfdConvertsIntoOneCharInNfcAndRecombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302",  // NFD input;  delete 1: \u0302
    // ---- results ----
    /* action del, output: */            1, U"ệ",             // NFC output; delete 1: ê
    /* app_context: */                   u"abcệ"
  },

  // a\u0300 should not be normalized because it is not otherwise impacted by
  // the action.
  {
    "AvoidEditingTooFarBackInContextWhenFindingNormalizationBoundary",
    /* app context pre transform: */     u"a\u0300bcê",
    /* cached context post transform: */ u"a\u0300bce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302", // NFD input;  delete 1: \u0302
    // ---- results ----
    /* action del, output: */            1, U"ệ",              // NFC output; delete 1: ê
    /* app_context: */                   u"a\u0300bcệ"
  },

  // If we don't reach a normalization boundary, we still should continue to work
  {
    "NormalizableLettersAtStartOfContext",
    /* app context pre transform: */     u"\u0300",
    /* cached context post transform: */ u"\u0323\u0300\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0300\u0302",       // NFD input;
    // ---- results ----
    /* action del, output: */            1, U"\u0323\u0300\u0302",  // NFC output is still decomposed because there is no base
    /* app_context: */                   u"\u0323\u0300\u0302"
  },

  // #15505 - normalization of Bengali characters
  {
    "BengaliNormalizationOfU09C7U09D7U09CC",
    /* app context pre transform: */     u"\u0995\u09C7",
    /* cached context post transform: */ u"\u0995\u09C7\u09D7",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\u09D7",
    // ---- results ----
    /* action del, output: */            1, U"\u09CC",
    /* app_context: */                   u"\u0995\u09CC"
  },

  // Modifies the base as well as diacritic

  {
    "TwoBackspacesForNfdConvertsIntoOneCharInNfcAndRecombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abca\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\u0323\u0302",  // NFD input;  delete 2: e\u0302
    // ---- results ----
    /* action del, output: */            1, U"ậ",             // NFC output; delete 1: ê
    /* app_context: */                   u"abcậ"
  },

  // surrogate pair tests

  {
    "SurrogatePairInContext",
    /* app context pre transform: */     u"abc\U0001F607ê",
    /* cached context post transform: */ u"abc\U0001F607a\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\u0323\u0302",
    // ---- results ----
    /* action del, output: */            1, U"ậ",
    /* app_context: */                   u"abc\U0001F607ậ"
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
    /* action del, output: */            2, U"a\U0001F60E",
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

    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  },

  {
   "AMarkerInTheModifiedSectionOfCachedContextShouldNotShowUpInAppContext",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_2[0],
    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  },

  // regression #11067
  {
    "ANonBmpCharInContext11067",
    /* app context pre transform: */     u"𐒻",
    /* cached context post transform: */ u"𐒻𐒷",
    /* cached context post transform: */ &items_11067[0],
    /* action del, output: */            0, U"𐒻𐒷",
    // ---- results ----
    /* action del, output: */            1, U"𐒻𐒷",
    /* app_context: */                   u"𐒻𐒷"
  }
};

INSTANTIATE_TEST_SUITE_P(KeymanCore, ActionsNormalizeApiTest, testing::ValuesIn(values), GenerateTestName);
