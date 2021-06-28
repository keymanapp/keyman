#include "pch.h"


TEST(TestCaseName, TestName) {
  EXPECT_EQ(1, 1);
  EXPECT_TRUE(true);
}

//int xstrlen(PWSTR p);

TEST(xstring, xstrlen) {
  EXPECT_EQ(3, xstrlen(L"abc"));
}
