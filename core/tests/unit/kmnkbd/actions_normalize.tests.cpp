/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin
  History:      23 Oct 2023 - MCD - Initial implementation.
*/
#include <string>
#include <memory>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"
#include "context.hpp"

#include <test_assert.h>
#include "../emscripten_filesystem.h"
#include "../load_kmx_file.hpp"

void compare_context(km_core_context *app_context, const km_core_cu* expected_final_app_context);

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

km_core_keyboard * test_kb = nullptr;
km_core_state * test_state = nullptr;
km_core_actions test_actions = {0};
std::string arg_path;

void teardown() {
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



void setup(const km_core_cu *app_context, const km_core_cu *cached_context_string, const km_core_context_item *cached_context_items, int actions_code_points_to_delete, const std::u32string actions_output) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, "..", "ldml", "keyboards", "k_001_tiny.kmx");
  auto blob = km::tests::load_kmx_file(path.native().c_str());
  try_status(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));

  if(cached_context_string) {
    try_status(set_context_from_string(km_core_state_context(test_state), cached_context_string));
  } else {
    // has markers
    km_core_context_set(km_core_state_context(test_state), cached_context_items);
  }
  try_status(set_context_from_string(km_core_state_app_context(test_state), app_context));

  memset(&test_actions, 0, sizeof(km_core_actions));
  test_actions.code_points_to_delete = actions_code_points_to_delete;
  std::unique_ptr<km_core_usv[]> buf(new km_core_usv[actions_output.length() + 1]);
  actions_output.copy(buf.get(), actions_output.length());
  // terminate the buffer
  buf.get()[actions_output.length()] = 0;
  test_actions.output = buf.release();
  test_actions.deleted_context = nullptr;
}

//-------------------------------------------------------------------------------------

/**
 * Run a single test on actions_normalize. This is quite nuanced, because the
 * input state is more than a little complex. We have inputs in NFU and NFD, and
 * outputs counting NFU and inserting NFC. Be careful!
 *
 * @param name                          descriptive name for the test
 * @param initial_app_context           the app context stored in the state,
 *                                      _before_ transform is applied -- NFU
 * @param final_cached_context_string   cached context _after_ actions have been
 *                                      applied -- guaranteed NFD (essentially,
 *                                      this is initial_cached_context -
 *                                      actions_code_points_to_delete +
 *                                      actions_output) - no markers supported
 * @param final_cached_context_items    cached context _after_ actions have been
 *                                      applied -- guaranteed NFD (essentially,
 *                                      this is initial_cached_context -
 *                                      actions_code_points_to_delete +
 *                                      actions_output) - markers supported
 * @param actions_code_points_to_delete number of NFD code points that the
 *                                      keyboard processor has asked to remove
 *                                      in its actions
 * @param actions_output                NFD string that the keyboard processor
 *                                      has asked to insert in its actions
 * @param expected_delete               expected: NFU code points to ask app to
 *                                      remove
 * @param expected_output               expected: adjusted NFC output to insert
 *                                      into the app
 * @param expected_final_app_context    expected: NFU adjusted final app
 *                                      context, which will be NFC from the
 *                                      boundary of the transform, but will not
 *                                      have been modified prior to that. Should
 *                                      match char-for-char what the app ends up
 *                                      with in its text buffer.
 */
void test_actions_normalize(
  const char *name,
  const km_core_cu *initial_app_context,
  const km_core_cu *final_cached_context_string,
  const km_core_context_item *final_cached_context_items,
  int actions_code_points_to_delete,
  const std::u32string actions_output,

  const unsigned int expected_delete,
  const std::u32string expected_output,
  const km_core_cu *expected_final_app_context
) {
  std::cout << "test_actions_normalize: " << name << std::endl;

  setup(initial_app_context, final_cached_context_string, final_cached_context_items, actions_code_points_to_delete, actions_output);

  assert(km::core::actions_normalize(km_core_state_context(test_state), km_core_state_app_context(test_state), test_actions));

  std::cout << "test_actions_normalize: (" << name << "): delete: " << test_actions.code_points_to_delete << " output: |" << std::u32string(test_actions.output) << "|" << std::endl;
  std::u32string o(test_actions.output);
  for(auto i = o.begin(); i < o.end(); i++) {
    std::cout << "U+" << std::hex << (int)(*i) << " ";
  }
  std::cout << std::endl;

  assert(expected_delete == test_actions.code_points_to_delete);
  assert(expected_output == test_actions.output);

  auto debug = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_APP);
  std::cout << " final app context: " << debug << std::endl;
  km_core_cu_dispose(debug);

  compare_context(km_core_state_app_context(test_state), expected_final_app_context);
  teardown();
}

/**
 * Run a single test on actions_update_app_context_nfu. This is simpler than
 * actions_normalize -- everything stays in NFU, and all we need to verify is
 * that the result has no markers and all chars are the same as input.
 *
 * @param name                          descriptive name for the test
 * @param initial_app_context           the app context stored in the state,
 *                                      _before_ transform is applied -- NFU
 * @param final_cached_context_string   cached context _after_ actions have been
 *                                      applied -- NFU (essentially,
 *                                      this is initial_cached_context -
 *                                      actions_code_points_to_delete +
 *                                      actions_output) - no markers supported.
 *                                      If specified, final_cached_context_items
 *                                      must be nullptr.
 * @param final_cached_context_items    cached context _after_ actions have been
 *                                      applied -- NFU (essentially,
 *                                      this is initial_cached_context -
 *                                      actions_code_points_to_delete +
 *                                      actions_output) - markers supported.
 *                                      If specified, final_cached_context_string
 *                                      must be nullptr.
 * @param actions_code_points_to_delete number of NFU code points that the
 *                                      keyboard processor has asked to remove
 *                                      in its actions
 * @param actions_output                NFU string that the keyboard processor
 *                                      has asked to insert in its actions
 * @param expected_delete               expected: NFU code points to ask app to
 *                                      remove
 * @param expected_output               expected: adjusted NFU output to insert
 *                                      into the app
 * @param expected_final_app_context    expected: NFU adjusted final app
 *                                      context, which will be NFC from the
 *                                      boundary of the transform, but will not
 *                                      have been modified prior to that. Should
 *                                      match char-for-char what the app ends up
 *                                      with in its text buffer.
 */
void test_actions_update_app_context_nfu(
  const char *name,
  const km_core_cu *initial_app_context,
  const km_core_cu *final_cached_context_string,
  const km_core_context_item *final_cached_context_items,
  int actions_code_points_to_delete,
  const std::u32string actions_output,

  const unsigned int expected_delete,
  const std::u32string expected_output,
  const km_core_cu *expected_final_app_context
) {
  std::cout << "test_actions_update_app_context_nfu: " << name << std::endl;

  setup(initial_app_context, final_cached_context_string, final_cached_context_items, actions_code_points_to_delete, actions_output);

  assert(km::core::actions_update_app_context_nfu(km_core_state_context(test_state), km_core_state_app_context(test_state)));

  std::cout << "test_actions_update_app_context_nfu: (" << name << "): delete: " << expected_delete << " output: |" << std::u32string(test_actions.output) << "|" << std::endl;
  std::u32string o(test_actions.output);
  for(auto i = o.begin(); i < o.end(); i++) {
    std::cout << "U+" << std::hex << (int)(*i) << " ";
  }
  std::cout << std::endl;

  assert(expected_delete == test_actions.code_points_to_delete);
  assert(expected_output == test_actions.output);

  auto debug = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_APP);
  std::cout << " final app context: " << debug << std::endl;
  km_core_cu_dispose(debug);

  compare_context(km_core_state_app_context(test_state), expected_final_app_context);
  teardown();
}

void run_actions_normalize_tests() {

  // Null boundary tests

  test_actions_normalize(
    "noop",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u""
  );

  test_actions_normalize(
    "no_output",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u"abc"
  );

  test_actions_normalize(
    "no_context",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"def",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* action del, output: */            0, U"def",
    /* app_context: */                   u"def"
  );

  // Simple tests -- no deletions involved

  test_actions_normalize(
    "no_normalization",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcdef",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* action del, output: */            0, U"def",
    /* app_context: */                   u"abcdef"
  );

  test_actions_normalize(
    "output_to_nfc_basic",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcde\u0300f",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"de\u0300f",
    // ---- results ----
    /* action del, output: */            0, U"dèf",
    /* app_context: */                   u"abcdèf"
  );

  test_actions_normalize(
    "output_to_nfc_hefty",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcA\u0300" u"e\u0316\u0301" u"\u0073\u0323\u0307"  u"\u0041\u030a"     u"\U000114B9\U000114B0",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"A\u0300" U"e\u0316\u0301" U"\u0073\u0323\u0307"  U"\u0041\u030a"     U"\U000114B9\U000114B0",
    // ---- results ----
    /* action del, output: */            0, U"À"       U"é̖"             U"\u1e69"              U"\u00c5"           U"\U000114BC",
    /* app_context: */                   u"abcÀé̖\u1e69\u00c5\U000114BC"
  );

  // Interaction with input context when not on normalization boundary

  test_actions_normalize(
    "Backtrack one character to combine as NFC",
    /* app context pre transform: */     u"XYZA",
    /* cached context post transform: */ u"XYZA\u0300abc",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\u0300abc",
    // ---- results ----
    /* action del, output: */            1, U"Àabc",
    /* app_context: */                   u"XYZÀabc"
  );

  test_actions_normalize(
    "Backtrack e+comb circ (2 chars) to combine as NFC",
    /* app context pre transform: */     u"abce\u0302",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302",
    // ---- results ----
    /* action del, output: */            2, U"ệ",
    /* app_context: */                   u"abcệ"
  );

  test_actions_normalize(
    "One backspace for NFD converts into one char in NFC (ê) and recombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302",  // NFD input;  delete 1: \u0302
    // ---- results ----
    /* action del, output: */            1, U"ệ",             // NFC output; delete 1: ê
    /* app_context: */                   u"abcệ"
  );

  // a\u0300 should not be normalized because it is not otherwise impacted by
  // the action.
  test_actions_normalize(
    "Avoid editing too far back in context when finding normalization boundary",
    /* app context pre transform: */     u"a\u0300bcê",
    /* cached context post transform: */ u"a\u0300bce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302", // NFD input;  delete 1: \u0302
    // ---- results ----
    /* action del, output: */            1, U"ệ",              // NFC output; delete 1: ê
    /* app_context: */                   u"a\u0300bcệ"
  );

  // If we don't reach a normalization boundary, we still should continue to work
  test_actions_normalize(
    "Normalizable letters at start of context",
    /* app context pre transform: */     u"\u0300",
    /* cached context post transform: */ u"\u0323\u0300\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0300\u0302",       // NFD input;
    // ---- results ----
    /* action del, output: */            1, U"\u0323\u0300\u0302",  // NFC output is still decomposed because there is no base
    /* app_context: */                   u"\u0323\u0300\u0302"
  );

  // Modifies the base as well as diacritic

  test_actions_normalize(
    "Two backspaces for NFD converts into one char in NFC (ê) and recombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abca\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\u0323\u0302",  // NFD input;  delete 2: e\u0302
    // ---- results ----
    /* action del, output: */            1, U"ậ",             // NFC output; delete 1: ê
    /* app_context: */                   u"abcậ"
  );

  // surrogate pair tests

  test_actions_normalize(
    "Surrogate pair in context",
    /* app context pre transform: */     u"abc\U0001F607ê",
    /* cached context post transform: */ u"abc\U0001F607a\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\u0323\u0302",
    // ---- results ----
    /* action del, output: */            1, U"ậ",
    /* app_context: */                   u"abc\U0001F607ậ"
  );

  test_actions_normalize(
    "Surrogate pair in output",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc\U0001F607",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\U0001F607",
    // ---- results ----
    /* action del, output: */            0, U"\U0001F607",
    /* app_context: */                   u"abc\U0001F607"
  );

  test_actions_normalize(
    "Surrogate pairs in both context and output",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ u"a\U0001F607bca\U0001F60E",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  );

  // Marker tests

  km_core_context_item items_1[] = { //u"a\U0001F607b\uFFFF\u0008\u0001ca\U0001F60E",
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F607 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0062 } },
    { KM_CORE_CT_MARKER, {0,}, { 0x1 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0063 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F60E } },
    KM_CORE_CONTEXT_ITEM_END
  };

  test_actions_normalize(
    "A marker in the cached context, should not show up in app_context",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_1[0],

    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  );

  km_core_context_item items_2[] = { //u"a\U0001F607bca\U0001F60E\uFFFF\u0008\u0001",
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F607 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0062 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0063 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F60E } },
    { KM_CORE_CT_MARKER, {0,}, { 0x1 } },
    KM_CORE_CONTEXT_ITEM_END
  };

  test_actions_normalize(
    "A marker in the modified section of cached context, should not show up in app_context",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_2[0],
    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  );

  km_core_context_item items_11067[] = {
    { KM_CORE_CT_CHAR,   {0,}, { U'𐒻' } },
    { KM_CORE_CT_CHAR,   {0,}, { U'𐒷' } },
    KM_CORE_CONTEXT_ITEM_END
  };

  // regression #11067
  test_actions_normalize(
    "A non-BMP char in context (#11067)",
    /* app context pre transform: */     u"𐒻",
    /* cached context post transform: */ u"𐒻𐒷",
    /* cached context post transform: */ &items_11067[0],
    /* action del, output: */            0, U"𐒻𐒷",
    // ---- results ----
    /* action del, output: */            1, U"𐒻𐒷",
    /* app_context: */                   u"𐒻𐒷"
  );
}

void run_actions_update_app_context_nfu_tests() {

  // Null boundary tests

  test_actions_update_app_context_nfu(
    "noop",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u""
  );

  test_actions_update_app_context_nfu(
    "no_output",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* action del, output: */            0, U"",
    /* app_context: */                   u"abc"
  );

  test_actions_update_app_context_nfu(
    "no_context",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"def",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* action del, output: */            0, U"def",
    /* app_context: */                   u"def"
  );

  // surrogate pair tests

  test_actions_update_app_context_nfu(
    "Surrogate pair in context",
    /* app context pre transform: */     u"abc\U0001F607ê",
    /* cached context post transform: */ u"abc\U0001F607a\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"a\u0323\u0302",
    // ---- results ----
    /* action del, output: */            1, U"a\u0323\u0302",
    /* app_context: */                   u"abc\U0001F607a\u0323\u0302"
  );

  test_actions_update_app_context_nfu(
    "Surrogate pair in output",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc\U0001F607",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\U0001F607",
    // ---- results ----
    /* action del, output: */            0, U"\U0001F607",
    /* app_context: */                   u"abc\U0001F607"
  );

  test_actions_update_app_context_nfu(
    "Surrogate pairs in both context and output",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ u"a\U0001F607bca\U0001F60E",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  );

  // Marker tests

  km_core_context_item items_1[] = { //u"a\U0001F607b\uFFFF\u0008\u0001ca\U0001F60E",
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F607 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0062 } },
    { KM_CORE_CT_MARKER, {0,}, { 0x1 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0063 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F60E } },
    KM_CORE_CONTEXT_ITEM_END
  };

  test_actions_update_app_context_nfu(
    "A marker in the cached context, should not show up in app_context",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_1[0],
    /* action del, output: */            1, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  );

  km_core_context_item items_2[] = { //u"a\U0001F607bca\U0001F60E\uFFFF\u0008\u0001",
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F607 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0062 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0063 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x0061 } },
    { KM_CORE_CT_CHAR,   {0,}, { 0x1F60E } },
    { KM_CORE_CT_MARKER, {0,}, { 0x1 } },
    KM_CORE_CONTEXT_ITEM_END
  };

  test_actions_update_app_context_nfu(
    "A marker in the modified section of cached context, should not show up in app_context",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_2[0],
    /* action del, output: */            1, U"a\U0001F60E",
    // ---- results ----
    /* action del, output: */            1, U"a\U0001F60E",
    /* app_context: */                   u"a\U0001F607bca\U0001F60E"
  );
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str = "\
test_actions_normalize [--color] <BUILD_PATH>\n\
\n\
  --color         Force color output\n\
  BUILD_PATH      Path where test_actions_normalize.exe is found; kmx files are\n\
                  located relative to this path.\n";

int error_args() {
  std::cerr << "test_actions_normalize: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {

  if(argc < 2) {
    return error_args();
  }

  auto arg_color = std::string(argv[1]) == "--color";
  if(arg_color && argc < 3) {
    return error_args();
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

#ifdef __EMSCRIPTEN__
  arg_path = get_wasm_file_path(argv[arg_color ? 2 : 1]);
#else
  arg_path = argv[arg_color ? 2 : 1];
#endif

  run_actions_normalize_tests();
  run_actions_update_app_context_nfu_tests();
}


void compare_context(km_core_context *app_context, const km_core_cu* expected_final_app_context) {
  // Compare context items -- to ensure no markers have leaked into app context
  km_core_context_item *actual_final_app_context_items = nullptr, *expected_final_app_context_items = nullptr;
  try_status(km_core_context_get(app_context, &actual_final_app_context_items));
  try_status(context_items_from_utf16(expected_final_app_context, &expected_final_app_context_items));

  for(int i = 0; actual_final_app_context_items[i].type != KM_CORE_CT_END || expected_final_app_context_items[i].type != KM_CORE_CT_END; i++) {
    assert(
      actual_final_app_context_items[i].type == expected_final_app_context_items[i].type &&
      // union so testing character is sufficient to do both char + marker types
      actual_final_app_context_items[i].character == expected_final_app_context_items[i].character
    );
  }

  km_core_context_items_dispose(actual_final_app_context_items);
  km_core_context_items_dispose(expected_final_app_context_items);
}
