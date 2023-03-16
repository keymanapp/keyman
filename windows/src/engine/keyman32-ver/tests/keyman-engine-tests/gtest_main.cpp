/**
 * @file
 * @copyright (c) 2013 Stephan Brenner
 * @license   This project is released under the MIT License.
 * repo: https://github.com/stbrenner/gtest_mem
 * Adapted in 2021 from the gtest_mem.cpp
 * This file implements a main() function for Google Test that runs all tests
 * and detects memory leaks.
 */
#include "pch.h"
#include <crtdbg.h>
#include <gtest/gtest.h>
#include <iostream>

using namespace std;
using namespace testing;

namespace testing {
class MemoryLeakDetector : public EmptyTestEventListener {
#ifdef _DEBUG
public:
  virtual void
  OnTestStart(const TestInfo&) {
    _CrtMemCheckpoint(&memState_);
  }

  virtual void
  OnTestEnd(const TestInfo& test_info) {
    if (test_info.result()->Passed()) {
      _CrtMemState stateNow, stateDiff;
      _CrtMemCheckpoint(&stateNow);
      int diffResult = _CrtMemDifference(&stateDiff, &memState_, &stateNow);
      if (diffResult) {
        FAIL() << "Memory leak of " << stateDiff.lSizes[1] << " byte(s) detected.";
      }
    }
  }

private:
  _CrtMemState memState_;
#endif  // _DEBUG
};
}  // namespace testing

GTEST_API_ int
main(int argc, char** argv) {
  cout << "Running main() from gtest_main.cpp" << endl;

  InitGoogleTest(&argc, argv);
  UnitTest::GetInstance()->listeners().Append(new MemoryLeakDetector());

  return RUN_ALL_TESTS();
}
