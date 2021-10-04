#include "pch.h"


// Test calling buffer of MAXCONTEXT size
TEST(AppContext, Get_Max_buff) {
  WCHAR callbuf[MAXCONTEXT];
  AppContext testContext;
  WCHAR *testString = L"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz0123456789";
  WCHAR *expectedString = L"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz0"; // MAXCONTEXT taking into account null character termination
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
