#include "pch.h"


TEST(TestCaseName, TestName) {
  EXPECT_EQ(1, 1);
  EXPECT_TRUE(true);
}

//int xstrlen(PWSTR p);

TEST(xstring, xstrlen) {
  EXPECT_EQ(3, xstrlen(L"abc"));
}

// Test Context should return bcd
TEST(AppContext, Get_small_buff) {

  WCHAR tmpbuf[MAXCONTEXT];
  AppContext myContext;
  myContext.Set(L"abcd");
  myContext.Get(tmpbuf, 3);
// EXPECT_STREQ((L"bcd"), tmpbuf);
}

// Test Context should return bcd
TEST(AppContext, GetSize_small_buff) {

  WCHAR tmpbuf[MAXCONTEXT];
  AppContext myContext;
  myContext.Set(L"abcd");
  myContext.GetSize(tmpbuf, 3);
  EXPECT_STREQ((L"bcd"), tmpbuf);
}

// Test with the surrage pair in the middle of the size buffer requested
// Set the context with a string containing a surratage pair.

TEST(AppContext, Get_over_run_buf) {

  WCHAR tmpbuf[MAXCONTEXT];
  AppContext myContext;

 // myContext.Set(L"a𐐷bc4");
  //myContext.GetSize(tmpbuf, 10);

  WCHAR testString[] = { 0x0042, 0x0042, 0xD800, 0xDC00, 0x0043, 0x0044, 0x0000 };
  WCHAR expectedString[] = { 0x0043, 0x0044, 0x0000 };
  // Test for breaking at surrogate and one less should give the same string.
  myContext.Set(testString);
  myContext.GetSize(tmpbuf, 10);

  EXPECT_STREQ((testString), tmpbuf);
  // split the surrogate should return on less than requested
  myContext.GetSize(tmpbuf, 3);
  EXPECT_STREQ((expectedString), tmpbuf);
  // This will should be the same size as the split surrogate
  myContext.GetSize(tmpbuf, 2);
  EXPECT_STREQ((expectedString), tmpbuf);
}
