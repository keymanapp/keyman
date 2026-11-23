
#include "./actions_test_data.h"

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

const std::vector<ActionsTestData> actionsTestData = {
  // Null boundary tests

  {
    "Noop",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* expected action del, output: */   0, U"",
    /* expected app_context: */          u"",
    /* expected del */                   U""
  },

  {
    "NoOutput",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"",
    // ---- results ----
    /* expected action del, output: */   0, U"",
    /* expected app_context: */          u"abc",
    /* expected del */                   U""
  },

  {
    "NoContext",
    /* app context pre transform: */     u"",
    /* cached context post transform: */ u"def",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* expected action del, output: */   0, U"def",
    /* expected app_context: */          u"def",
    /* expected del */                   U""
  },

  // Simple tests -- no deletions involved

  {
    "NoNormalization",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcdef",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"def",
    // ---- results ----
    /* expected action del, output: */   0, U"def",
    /* expected app_context: */          u"abcdef",
    /* expected del */                   U""
  },

  {
    "OutputToNfcBasic",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcde\u0300f",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"de\u0300f",
    // ---- results ----
    /* expected action del, output: */   0, U"dèf",
    /* expected app_context: */          u"abcdèf",
    /* expected del */                   U""
  },

  {
    "OutputToNfcHefty",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abcA\u0300" u"e\u0316\u0301" u"\u0073\u0323\u0307"  u"\u0041\u030a"     u"\U000114B9\U000114B0",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"A\u0300" U"e\u0316\u0301" U"\u0073\u0323\u0307"  U"\u0041\u030a"     U"\U000114B9\U000114B0",
    // ---- results ----
    /* expected action del, output: */   0, U"À"       U"é̖"             U"\u1e69"              U"\u00c5"           U"\U000114BC",
    /* expected app_context: */          u"abcÀé̖\u1e69\u00c5\U000114BC",
    /* expected del */                   U""
  },

  // Interaction with input context when not on normalization boundary

  {
    "BacktrackOneCharacterToCombineAsNfc",
    /* app context pre transform: */     u"XYZA",
    /* cached context post transform: */ u"XYZA\u0300abc",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\u0300abc",
    // ---- results ----
    /* expected action del, output: */   1, U"Àabc",
    /* expected app_context: */          u"XYZÀabc",
    /* expected del */                   U"A"
  },

  {
    "BacktrackECombCirc2CharsToCombineAsNfc",
    /* app context pre transform: */     u"abce\u0302",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302",
    // ---- results ----
    /* expected action del, output: */   2, U"ệ",
    /* expected app_context: */          u"abcệ",
    /* expected del */                   U"e\u0302"
  },

  {
    "OneBackspaceToDeleteLastNfdCharacter15487",
    /* app context pre transform: */     u"abcê", // NFC
    /* cached context post transform: */ u"abce",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"",  // NFD input;  delete 1: \u0302
    // ---- results ----
    /* expected action del, output: */   1, U"e",             // NFC output; delete 1: e
    /* expected app_context: */          u"abce",
    /* expected del */                   U"ê"
  },

  {
    "OneBackspaceToDeleteLastNfdCharacterWithNfdAppContext15487",
    /* app context pre transform: */     u"abce\u0302", // NFD
    /* cached context post transform: */ u"abce",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"",  // NFD input;  delete 1: \u0302
    // ---- results ----
    /* expected action del, output: */   1, U"",             // NFC output; delete 1: e
    /* expected app_context: */          u"abce",
    /* expected del */                   U"\u0302"
  },

  {
    "OneBackspaceForNfdConvertsIntoOneCharInNfcAndRecombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abce\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0302",  // NFD input;  delete 1: \u0302
    // ---- results ----
    /* expected action del, output: */   1, U"ệ",             // NFC output; delete 1: ê
    /* expected app_context: */          u"abcệ",
    /* expected del */                   U"ê"
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
    /* expected action del, output: */   1, U"ệ",              // NFC output; delete 1: ê
    /* expected app_context: */          u"a\u0300bcệ",
    /* expected del */                   U"ê"
  },

  // If we don't reach a normalization boundary, we still should continue to work
  {
    "NormalizableLettersAtStartOfContext",
    /* app context pre transform: */     u"\u0300",
    /* cached context post transform: */ u"\u0323\u0300\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            1, U"\u0323\u0300\u0302",       // NFD input;
    // ---- results ----
    /* expected action del, output: */   1, U"\u0323\u0300\u0302",  // NFC output is still decomposed because there is no base
    /* expected app_context: */          u"\u0323\u0300\u0302",
    /* expected del */                   U"\u0300"
  },

  // #15505 - normalization of Bengali characters
  {
    "BengaliNormalizationOfU09C7U09D7U09CC",
    /* app context pre transform: */     u"\u0995\u09C7",
    /* cached context post transform: */ u"\u0995\u09C7\u09D7",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\u09D7",
    // ---- results ----
    /* expected action del, output: */   1, U"\u09CC",
    /* expected app_context: */          u"\u0995\u09CC",
    /* expected del */                   U"\u09C7"
  },

  // Modifies the base as well as diacritic

  {
    "TwoBackspacesForNfdConvertsIntoOneCharInNfcAndRecombine",
    /* app context pre transform: */     u"abcê",
    /* cached context post transform: */ u"abca\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\u0323\u0302",  // NFD input;  delete 2: e\u0302
    // ---- results ----
    /* expected action del, output: */   1, U"ậ",             // NFC output; delete 1: ê
    /* expected app_context: */          u"abcậ",
    /* expected del */                   U"ê"
  },

  // surrogate pair tests

  {
    "SurrogatePairInContext",
    /* app context pre transform: */     u"abc\U0001F607ê",
    /* cached context post transform: */ u"abc\U0001F607a\u0323\u0302",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\u0323\u0302",
    // ---- results ----
    /* expected action del, output: */   1, U"ậ",
    /* expected app_context: */          u"abc\U0001F607ậ",
    /* expected del */                   U"ê"
  },

  {
    "SurrogatePairInOutput",
    /* app context pre transform: */     u"abc",
    /* cached context post transform: */ u"abc\U0001F607",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            0, U"\U0001F607",
    // ---- results ----
    /* expected action del, output: */   0, U"\U0001F607",
    /* expected app_context: */          u"abc\U0001F607",
    /* expected del */                   U""
  },

  {
    "SurrogatePairsInBothContextAndOutput",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ u"a\U0001F607bca\U0001F60E",
    /* cached context post transform: */ nullptr,
    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* expected action del, output: */   1, U"a\U0001F60E",
    /* expected app_context: */          u"a\U0001F607bca\U0001F60E",
    /* expected del */                   U"ê"
  },

  // Marker tests

  {
    "AMarkerInTheCachedContextShouldNotShowUpInAppContext",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_1[0],

    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* expected action del, output: */   1, U"a\U0001F60E",
    /* expected app_context: */          u"a\U0001F607bca\U0001F60E",
    /* expected del */                   U"ê"
  },

  {
   "AMarkerInTheModifiedSectionOfCachedContextShouldNotShowUpInAppContext",
    /* app context pre transform: */     u"a\U0001F607bcê",
    /* cached context post transform: */ nullptr,
    /* cached context post transform: */ &items_2[0],
    /* action del, output: */            2, U"a\U0001F60E",
    // ---- results ----
    /* expected action del, output: */   1, U"a\U0001F60E",
    /* expected app_context: */          u"a\U0001F607bca\U0001F60E",
    /* expected del */                   U"ê"
  },

  // regression #11067
  {
    "ANonBmpCharInContext11067",
    /* app context pre transform: */     u"𐒻",
    /* cached context post transform: */ u"𐒻𐒷",
    /* cached context post transform: */ &items_11067[0],
    /* action del, output: */            0, U"𐒻𐒷",
    // ---- results ----
    /* expected action del, output: */   1, U"𐒻𐒷",
    /* expected app_context: */          u"𐒻𐒷",
    /* expected del: */                  U"\x104BB"
  }
};

std::string GenerateTestName(const testing::TestParamInfo<ActionsTestData>& info) {
  return info.param.test_name;
}
