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
//TEST_F(KMPROCESSACTIONS, processOutputStringtest) {
// // just check something has been added to the que
//  WCHAR callbuf[MAXCONTEXT];
//  AITIP testApp;
//  int expectedQueueSize           = 3; // 1 + surrogate pair
//  km_core_usv output_string[] = {0x0041, 0x010000, 0};
//  process_output_string(&testApp, &output_string[0]);
//  int queueSize = testApp.GetQueueSize();
//  EXPECT_EQ(queueSize, expectedQueueSize);
//}
//
//// Test where there is no LF `\n' most common case
//TEST_F(KMPROCESSACTIONS, processOutputStringNoChange) {
//  WCHAR callbuf[MAXCONTEXT];
//  AITIP testApp;
//  int expectedQueueSize = 20;  
//  // L"Hello World You Rock";
// km_core_usv output_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x0020, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
//                                 0x0020, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0};
// //km_core_usv output_string[] = {0x0041, 0x010000, 0};
// process_output_string(&testApp, &output_string[0]);
//  int queueSize = testApp.GetQueueSize();
//  EXPECT_EQ(queueSize, expectedQueueSize);
//  
//}
//
//// Test inserting '\r' 
//TEST_F(KMPROCESSACTIONS, processOutputStringInsertCR) {
//  WCHAR callbuf[MAXCONTEXT];
//  AITIP testApp;
//  int expectedQueueSize = 22;
//  // L"Hello\nWorld\nYou Rock";
//  km_core_usv output_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000A, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
//                                 0x000A, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
//  // km_core_usv output_string[] = {0x0041, 0x010000, 0};
//  process_output_string(&testApp, &output_string[0]);
//  int queueSize = testApp.GetQueueSize();
//  EXPECT_EQ(queueSize, expectedQueueSize);
//}




//// Test where there is no LF `\n' most common case
//TEST_F(KMPROCESSACTIONS, processOutputStringNoChange) {
//  WCHAR callbuf[MAXCONTEXT];
//  AITIP testApp;
//  int expectedQueueSize = 20;
//  // L"Hello World You Rock";
//  km_core_usv output_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x0020, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
//                                 0x0020, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0};
//  // km_core_usv output_string[] = {0x0041, 0x010000, 0};
//  process_output_string_2(&testApp, &output_string[0]);
//  int queueSize = testApp.GetQueueSize();
//  EXPECT_EQ(queueSize, expectedQueueSize);
//}

// Test inserting '\r'
//TEST_F(KMPROCESSACTIONS, processOutputStringInsertCR) {
//  WCHAR callbuf[MAXCONTEXT];
//  AITIP testApp;
//  int expectedQueueSize = 22;
//  // L"Hello\nWorld\nYou Rock";
//  km_core_usv output_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000A, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
//                                 0x000A, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
//  // km_core_usv output_string[] = {0x0041, 0x010000, 0};
//  process_output_string_2(&testApp, &output_string[0]);
//  int queueSize = testApp.GetQueueSize();
//  EXPECT_EQ(queueSize, expectedQueueSize);
//}

// Test where there is LF `\n' most common case
TEST_F(KMPROCESSACTIONS, processNormalisLB_LF) {
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  LBType received_lb;
  int expectedQueueSize = 20;
  // L"Hello World You Rock";
  WCHAR app_string[] = {
    0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000A, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
                                     0x000A, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
  received_lb = normalize_line_breaks(app_string, callbuf, MAXCONTEXT);
  //process_output_string_2(&testApp, &output_string[0]);
  //int queueSize = testApp.GetQueueSize();
  //EXPECT_EQ(queueSize, expectedQueueSize);
  EXPECT_EQ(lbLF, received_lb);
}

// Test where there is a CR LF `\n' most common case
TEST_F(KMPROCESSACTIONS, processNormaliseLB_CRLF) {
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  LBType received_lb;
  int expectedQueueSize = 20;
  // L"Hello World You Rock";
  WCHAR app_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000D, 0x000A, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
                        0x000D, 0x000A, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
  received_lb        = normalize_line_breaks(app_string, callbuf, MAXCONTEXT);
  //process_output_string_2(&testApp, &output_string[0]);
  // int queueSize = testApp.GetQueueSize();
  // EXPECT_EQ(queueSize, expectedQueueSize);
  EXPECT_EQ(lbCRLF, received_lb);
}

// Test where there is a CR `\n' most common case
TEST_F(KMPROCESSACTIONS, processNormalisLB_CR) {
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  LBType received_lb;
  int expectedQueueSize = 20;
  // L"Hello World You Rock";
  WCHAR app_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000D, 0x0057, 0x006F, 0x0072, 0x006C,
                        0x0064, 0x000D, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
  received_lb        = normalize_line_breaks(app_string, callbuf, MAXCONTEXT);
  // process_output_string_2(&testApp, &output_string[0]);
  // int queueSize = testApp.GetQueueSize();
  // EXPECT_EQ(queueSize, expectedQueueSize);
  EXPECT_EQ(lbCR, received_lb);
}


// Test where there is a CR `\n' most common case
TEST_F(KMPROCESSACTIONS, processrestore_LB_CR) {
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  LBType received_lb;
  int expectedQueueSize = 20;
  // L"Hello World You Rock";
  WCHAR app_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000D, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
                        0x000D, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
  received_lb        = normalize_line_breaks(app_string, callbuf, MAXCONTEXT);
  EXPECT_EQ(lbCR, received_lb);
  restore_line_breaks(callbuf, MAXCONTEXT, received_lb, lbCRLF);
  process_output_string_2(&testApp, callbuf);
  int queueSize = testApp.GetQueueSize();
  EXPECT_EQ(queueSize, expectedQueueSize);
  
}

// Test where there is a CRLF `\n' most common case
TEST_F(KMPROCESSACTIONS, processrestore_LB_CRLF) {
  WCHAR callbuf[MAXCONTEXT];
  AITIP testApp;
  LBType received_lb;
  int expectedQueueSize = 22;
  // L"Hello World You Rock";
  WCHAR app_string[] = {0x0048, 0x0065, 0x006C, 0x006C, 0x006F, 0x000D, 0x000A, 0x0057, 0x006F, 0x0072, 0x006C, 0x0064,
                        0x000D, 0x000A, 0x0059, 0x006F, 0x0075, 0x0020, 0x0052, 0x006F, 0x0063, 0x006B, 0x0000};
  received_lb        = normalize_line_breaks(app_string, callbuf, MAXCONTEXT);
  EXPECT_EQ(lbCRLF, received_lb);
  restore_line_breaks(callbuf, MAXCONTEXT, received_lb, lbCRLF);
  process_output_string_2(&testApp, callbuf);
  int queueSize = testApp.GetQueueSize();
  EXPECT_EQ(queueSize, expectedQueueSize);
}
