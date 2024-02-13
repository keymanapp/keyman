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

// Test pre/post context functions

TEST(AppContext, pre_process_context) {
  wchar_t context[]         = L"Hello\r\nWorld\r\nYou Rock";
  wchar_t expected_string[] = L"Hello\nWorld\nYou Rock";
  EXPECT_TRUE(pre_process_context(context));
  EXPECT_STREQ(expected_string, context);
}

// No CRLF `\r\n` found
TEST(AppContext, pre_process_context_unchanged) {
  wchar_t context[]         = L"Hello World You Rock";
  wchar_t expected_string[] = L"Hello World You Rock";
  EXPECT_FALSE(pre_process_context(context));
  EXPECT_STREQ(expected_string, context);
}

// post process
// Test where there is no LF `\n' most common case
TEST(AppContext, post_process_context_no_change) {
  const size_t max_context = 17;
  wchar_t windows_context[max_context];
  const wchar_t context[] = L"Hello World You Rock";
  EXPECT_FALSE(post_process_context(context, windows_context, max_context));
}

// Test where inserting '\r' with sufficient room for the
// extended string in the output context.
TEST(AppContext, post_process_context_full) {
  const size_t max_context = 64;

  wchar_t windows_context[max_context];
  const wchar_t context[]   = L"Hello\nWorld\nYou Rock";
  wchar_t expected_string[] = L"Hello\r\nWorld\r\nYou Rock";
  EXPECT_TRUE(post_process_context(context, windows_context, max_context));
  EXPECT_STREQ((expected_string), windows_context);
}

// Test inserting '\r' makes the string to long for context
// and needs to be truncated furtherest from the caret
TEST(AppContext, post_process_context_truncated) {
  const size_t max_context = 22;
  wchar_t windows_context[max_context];
  const wchar_t context[] = L"Hello\nWorld\nYou Rock";
  wchar_t expected_string[] = L"ello\r\nWorld\r\nYou Rock";
  EXPECT_TRUE(post_process_context(context, windows_context, max_context));
  EXPECT_STREQ((expected_string), windows_context);
}

// Test where inserting a '\r' at the end furtherest from the caret
// would make it to long for the context buffer so you still end up with the \n
TEST(AppContext, post_process_context_split_rn) {
  const size_t max_context = 17;
  wchar_t windows_context[max_context];
  const wchar_t context[]   = L"Hello\nWorld\nYou Rock";
  wchar_t expected_string[] = L"\nWorld\r\nYou Rock";
  EXPECT_TRUE(post_process_context(context, windows_context, max_context));
  EXPECT_STREQ((expected_string), windows_context);
}
