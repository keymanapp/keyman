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
TEST_F(KMPROCESSACTIONS, processUnicodeChartest) {
 // just check something has been added to the que
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  int expectedQueueSize           = 3; // 1 + surrogate pair
  km_core_usv output_string[] = {0x0041, 0x010000, 0};
  processUnicodeChar(&testApp, &output_string[0]);
  int queueSize = testApp.GetQueueSize();
  EXPECT_EQ(queueSize, expectedQueueSize);
}

