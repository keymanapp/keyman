/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * CoreWrapperTest.m
 * CoreWrapperTests
 *
 * Created by Shawn Schantz on 2023-02-17.
 *
 * Description...
 */

#import <XCTest/XCTest.h>
#import "CoreHelper.h"
#import "CoreWrapper.h"
#import "CoreTestStaticHelperMethods.h"
#import "CoreAction.h"
#import "MacVKCodes.h"
#import "KMELogs.h"

@interface CoreWrapperTests : XCTestCase

@end

@implementation CoreWrapperTests

NSString *mockKmxFilePath = @"/a/dummy/keyboard.mock";
CoreWrapper *mockWrapper;

+ (void)setUp {
  NSString *khmerKeyboardPath = [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"khmer_angkor.kmx"];
  
  os_log_debug([KMELogs testLog], "mockKmxFilePath  = %@\n", mockKmxFilePath);

  mockWrapper = [[CoreWrapper alloc] initWithHelper: [CoreTestStaticHelperMethods helper] kmxFilePath:mockKmxFilePath];
}

- (void)tearDown {
  // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testCreateWrapper_mockKeyboardPath_noException {
  XCTAssertNoThrow([[CoreWrapper alloc] initWithHelper: [CoreTestStaticHelperMethods helper] kmxFilePath: mockKmxFilePath], @"Unexpected exception while creating helper");
}

- (void)testCreateWrapper_nilKeyboardPath_noException {
  XCTAssertNoThrow([[CoreWrapper alloc] initWithHelper: [CoreTestStaticHelperMethods helper] kmxFilePath: nil], @"Unexpected exception while creating helper with a nil keyboard path");
}

- (void)testChangeKeyboard_mockKeyboard_noException {
  // reloads the keyboard: take existing wrapper of mock keyboard and load the mock keyboard
  XCTAssertNoThrow([mockWrapper changeKeyboardWithKmxFilePath:mockKmxFilePath], @"Unexpected exception while reloading keyboard for existing wrapper");
}

- (void)testLoadKeyboard_nonExistentKeyboard_Exception {
  XCTAssertThrows([mockWrapper changeKeyboardWithKmxFilePath:@"/nonexistent/file.kmx"]);
}

- (void)testprocessEvent_lowercaseA_returnsExpectedCharacterForKmx {
  NSString *kmxPath = [CoreTestStaticHelperMethods getKmxFilePathTestMacEngine];
  CoreWrapper *core = [[CoreWrapper alloc] initWithHelper: [CoreTestStaticHelperMethods helper] kmxFilePath:kmxPath];
  
  // expecting character with 'Ç'
  CoreKeyOutput *coreOutput = [core processMacVirtualKey:MVK_A withModifiers:0 withKeyDown:YES];
  XCTAssert([coreOutput.textToInsert isEqualToString:@"\u00C7"], @"Expected capital C cedille (U+00C7)");
}

- (void)testgetContextAsString_ContextContainsEmojis_ReturnsSameContext  {
  NSString *kmxPath = [CoreTestStaticHelperMethods getKmxFilePathTestMacEngine];
  CoreWrapper *core = [[CoreWrapper alloc] initWithHelper: [CoreTestStaticHelperMethods helper] kmxFilePath:kmxPath];
  [core setContextIfNeeded:@"🤔?👍🏻✅"];
  NSString *finalContext = core.contextDebug;
  // Note: relying on km_core_state_context_debug output format is just barely
  // acceptable for a unit test
  XCTAssert([finalContext isEqualToString:@"|🤔?👍🏻✅| (len: 5) [ U+1f914 U+003f U+1f44d U+1f3fb U+2705 ]"], @"Expected '🤔?👍🏻✅' in context buffer");
}

@end
