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

// Test normalize and restore line break functions

TEST(AppContext, normalize_restore_CRLF) {
  wchar_t windows_context[]         = L"Hello\r\nWorld\r\nYou Rock";
  wchar_t core_context[MAXCONTEXT];
  wchar_t expected_string[] = L"Hello\nWorld\nYou Rock";
  LBType lb_type;
  lb_type = normalize_line_breaks(windows_context, core_context, MAXCONTEXT);
  EXPECT_TRUE(lb_type == lbCRLF);
  EXPECT_STREQ(expected_string, core_context);
  EXPECT_TRUE(restore_line_breaks(core_context, MAXCONTEXT, lb_type, lbCRLF) == rsSUCCESS);
  // should be the same as the original windows_context.
  EXPECT_STREQ(windows_context, core_context);
}

// No CRLF `\r\n` found
TEST(AppContext, normalize_restore_NONE) {
  wchar_t windows_context[] = L"Hello World You Rock";
  wchar_t core_context[MAXCONTEXT];
  wchar_t expected_string[] = L"Hello World You Rock";
  LBType lb_type;
  lb_type = normalize_line_breaks(windows_context, core_context, MAXCONTEXT);
  EXPECT_TRUE(lb_type == lbNONE);
  EXPECT_STREQ(expected_string, core_context);
  EXPECT_TRUE(restore_line_breaks(core_context, MAXCONTEXT, lb_type, lbCRLF) == rsNO_LB);
  // should be the same as the original windows_context.
  EXPECT_STREQ(windows_context, core_context);
}

TEST(AppContext, normalize_restore_CR) {
  wchar_t windows_context[] = L"Hello\rWorld\rYou Rock";
  wchar_t core_context[MAXCONTEXT];
  wchar_t expected_string[] = L"Hello\nWorld\nYou Rock";
  LBType lb_type;
  lb_type = normalize_line_breaks(windows_context, core_context, MAXCONTEXT);
  EXPECT_TRUE(lb_type == lbCR);
  EXPECT_STREQ(expected_string, core_context);
  EXPECT_TRUE(restore_line_breaks(core_context, MAXCONTEXT, lb_type, lbCRLF) == rsSUCCESS);
  // should be the same as the original windows_context.
  EXPECT_STREQ(windows_context, core_context);
}

TEST(AppContext, normalize_restore_LF) {
  wchar_t windows_context[] = L"Hello\nWorld\nYou Rock";
  wchar_t core_context[MAXCONTEXT];
  wchar_t expected_string[] = L"Hello\nWorld\nYou Rock";
  LBType lb_type;
  lb_type = normalize_line_breaks(windows_context, core_context, MAXCONTEXT);
  EXPECT_TRUE(lb_type == lbLF);
  EXPECT_STREQ(expected_string, core_context);
  EXPECT_TRUE(restore_line_breaks(core_context, MAXCONTEXT, lb_type, lbCRLF) == rsSUCCESS);
  // should be the same as the original windows_context.
  EXPECT_STREQ(windows_context, core_context);
}
