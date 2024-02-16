#include "pch.h"


// Test calling buffer of MAXCONTEXT size
TEST(AppContext, Get_Max_buff) {
  WCHAR callbuf[MAXCONTEXT];
  AppContext testContext;
  WCHAR *testString = L"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz0123456789";
  WCHAR *expectedString = L"JKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz0123456789"; // MAXCONTEXT taking into account null character termination
  testContext.Set(testString);
  testContext.Get(callbuf, MAXCONTEXT);
  EXPECT_STREQ((expectedString), callbuf);
}
// Test for a calling buffer smaller than AppContext's current context size.
// Expected result should return subset with charcters closet
// to the caret.
TEST(AppContext, Get_small_buff) {
  WCHAR callbuf[MAXCONTEXT];
  AppContext testContext;
  testContext.Set(L"abcdefghijk");
  testContext.Get(callbuf, 5);
  EXPECT_STREQ((L"ghijk"), callbuf);
}

// Test with the calling buffer size that would only have enough bytes for one
// surrogate of a surrogate pair.
TEST(AppContext, Get_split_surrogate) {

  WCHAR callbuff[MAXCONTEXT];
  AppContext testContext;
  WCHAR testString[] = { 0x0042, 0x0042, 0xD800, 0xDC00, 0x0043, 0x0044, 0x0000 };
  WCHAR expectedString[] = { 0x0043, 0x0044, 0x0000 };

  testContext.Set(testString);
  // split the surrogate should return on 16-bit code unit less than requested
  testContext.Get(callbuff, 3);
  EXPECT_STREQ((expectedString), callbuff);
  // This will should be the same size as the split surrogate
  testContext.Get(callbuff, 2);
  EXPECT_STREQ((expectedString), callbuff);
}


// Test Calling BufMax
TEST(AppContext, BufMax_LastChar) {
  const uint32_t ExpectedMarker = 0x0002;
  AppContext testContext;
  WCHAR testString[]            = {0x0042, 0xD800, 0xDC00, UC_SENTINEL, CODE_DEADKEY, ExpectedMarker, 0x0000};
  WCHAR expectedString[] = {UC_SENTINEL, CODE_DEADKEY, 0x0002, 0x0000};
  testContext.Set(testString);
  WCHAR *contextBuf = testContext.BufMax(3);
  EXPECT_STREQ((expectedString), contextBuf);
  contextBuf += 2;
  uint32_t TestMarker = (uint32_t)*contextBuf;
  EXPECT_EQ(ExpectedMarker, TestMarker);

}

// Test AppContext::Delete()
TEST(AppContext, AppContext_Delete) {
  const uint32_t ExpectedMarker = 0x0002;
  AppContext testContext;
  WCHAR testString[]     = {0x0042, 0xD800, 0xDC00, UC_SENTINEL, CODE_DEADKEY, ExpectedMarker, 0x0000};
  WCHAR expectedString1[] = {0x0042, 0xD800, 0xDC00, 0x0000};
  WCHAR expectedString2[] = {0x0042, 0x0000};
  WCHAR expectedString3[] = {0x0000};
  testContext.Set(testString);

  testContext.Delete();
  WCHAR *contextBuf = testContext.BufMax(MAXCONTEXT);
  EXPECT_STREQ((expectedString1), contextBuf);
  testContext.Delete();
  contextBuf = testContext.BufMax(MAXCONTEXT);
  EXPECT_STREQ((expectedString2), contextBuf);
  testContext.Delete();
  contextBuf = testContext.BufMax(MAXCONTEXT);
  EXPECT_STREQ((expectedString3), contextBuf);
  testContext.Delete();

}

// Test pre context function

TEST(AppContext, format_context_for_core) {
  wchar_t windows_context[]         = L"Hello\r\nWorld\r\nYou Rock";
  wchar_t core_context[MAXCONTEXT];
  wchar_t expected_string[] = L"Hello\nWorld\nYou Rock";
  EXPECT_TRUE(format_context_for_core(windows_context, core_context, MAXCONTEXT) == frUpdated);
  EXPECT_STREQ(expected_string, core_context);
}

// No CRLF `\r\n` found
TEST(AppContext, format_context_for_core_unchanged) {
  wchar_t windows_context[] = L"Hello World You Rock";
  wchar_t core_context[MAXCONTEXT];
  core_context[0]           = L'\0'; // for test just to verify it wasn't changed
  wchar_t expected_string[] = L"";
  EXPECT_EQ(format_context_for_core(windows_context, core_context, MAXCONTEXT), frNoChange);
  EXPECT_STREQ(expected_string, core_context);
}

//// Core Context too short 
TEST(AppContext, format_context_for_core_too_short) {
  wchar_t windows_context[] = L"Hello\r\nWorld\r\nYou Rock";
  wchar_t core_context[2];
  core_context[0]           = L'\0';  // for test just to verify it wasn't changed
  wchar_t expected_string[] = L"";
  EXPECT_EQ(format_context_for_core(windows_context, core_context, 2), frError);
  //EXPECT_STREQ(expected_string, core_context);
}

