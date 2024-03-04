#include "pch.h"
#include "kmprocessactions.cpp"

// Test the Process Actions private functions
// TODO: The following actions are not tested KM_CORE_IT_ALERT, KM_CORE_IT_PERSIST_OPT, KM_CORE_IT_EMIT_KEYSTROKE

// Fixture for kmprocessactons tests
class KMPROCESSACTIONS : public ::testing::Test {
public:
  KMPROCESSACTIONS() {}

  void
  SetUp() {
    Globals_InitProcess();
  }

  void
  TearDown() {
    UninitialiseProcess(FALSE);
    Globals_UninitProcess();
  }

  ~KMPROCESSACTIONS() {}
};

// KM_CORE_IT_CHAR - processUnicodeChar
// Now that the AITIP no longer contains a context
// this test is just reduced to checking the size of the
// action queue
TEST_F(KMPROCESSACTIONS, processOutputStringtest) {
 // just check something has been added to the que
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  int expectedQueueSize           = 3; // 1 + surrogate pair
  km_core_usv output_string[] = {0x0041, 0x010000, 0};
  context_char32_char16(output_string, callbuf, MAXCONTEXT);
  process_output_string(&testApp, callbuf);
  int queueSize = testApp.GetQueueSize();
  EXPECT_EQ(queueSize, expectedQueueSize);
}


// Test char32_char16_test
TEST_F(KMPROCESSACTIONS, context_char32_char16_test) {
  WCHAR wchar16_string[MAXCONTEXT];
  AITIP testApp;
  // L"Hello\nWorld\nYou 𝄞Rock";
  km_core_usv core_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000A, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
                                 0x000A, 0x0059, 0x006F, 0x0075, 0x0020, 0x1D11E, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};

  wchar_t expected_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000A, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064, 0x000A,
                               0x0059, 0x006F, 0x0075, 0x0020, 0xD834, 0xDD1E, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};

  context_char32_char16(&core_string[0], wchar16_string, MAXCONTEXT);
  EXPECT_STREQ(expected_string, wchar16_string);
}

// Test char32_char16_test
TEST_F(KMPROCESSACTIONS, context_char32_char16_small_context) {
  const int small_context = 15;
  WCHAR wchar16_string[small_context];
  AITIP testApp;
  // L"Hello\nWorld\nYou 𝄞Rock"
  km_core_usv core_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000A,  0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
                               0x000A, 0x0059, 0x006F, 0x0075, 0x0020, 0x1D11E, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
  // L"rld\nYou 𝄞Rock"
  wchar_t expected_string[] = {0x0072, 0x006C, 0x0064, 0x000A, 0x0059, 0x006F, 0x0075, 0x0020,
                               0xD834, 0xDD1E, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};

  context_char32_char16(&core_string[0], wchar16_string, small_context);
  EXPECT_STREQ(expected_string, wchar16_string);
}
