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
TEST_F(KMPROCESSACTIONS, processUnicodeChartest) {
 // just check something has been added to the que
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  int expectedQueueSize           = 1;
  km_core_action_item itemAddChar = { KM_CORE_IT_CHAR, {0,}, {'A'}};

  processUnicodeChar(&testApp, &itemAddChar);
  int queueSize = testApp.GetQueueSize();
  EXPECT_EQ(queueSize, expectedQueueSize);

  //km_core_usv testSurrogateChar    = Uni_SurrogateToUTF32(0xD801, 0xDC37);  //𐐷';
  //km_core_action_item itemAddChar2 = {KM_CORE_IT_CHAR, {0,}, {testSurrogateChar}};
  //WCHAR expectedStringSurrogate[] = {'A', 0xD801, 0xDC37, 0};
  //processUnicodeChar(&testApp, &itemAddChar2);
  //contextBuf = testApp.ContextBufMax(MAXCONTEXT);
  //EXPECT_STREQ(contextBuf, &expectedStringSurrogate[0]);
}


// KM_CORE_IT_BACK - processBack
// First test processing a backspace for a deadkey
TEST_F(KMPROCESSACTIONS, processBackDeadkeytest) {

  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  int expectedQueueSize           = 1;
  km_core_action_item itemAddChar = {KM_CORE_IT_CHAR, {0,}, {'A'}};

  processUnicodeChar(&testApp, &itemAddChar);
  uint32_t marker                 = 2;
  km_core_action_item itemAddMarker = {KM_CORE_IT_MARKER, {0,}, {marker}};
  processMarker(&testApp, &itemAddMarker);
  km_core_action_item itemBackSpace = {KM_CORE_IT_BACK};
  itemBackSpace.backspace.expected_type  = KM_CORE_IT_MARKER;
  itemBackSpace.backspace.expected_value = marker;
  processBack(&testApp, &itemBackSpace);
  int queueSize = testApp.GetQueueSize();
  EXPECT_EQ(expectedQueueSize, queueSize);
}

