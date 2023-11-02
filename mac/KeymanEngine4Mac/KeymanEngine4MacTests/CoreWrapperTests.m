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

@interface CoreWrapperTests : XCTestCase

@end

@implementation CoreWrapperTests

NSString *mockKmxFilePath = @"/a/dummy/keyboard.mock";
CoreWrapper *mockWrapper;

+ (void)setUp {
  NSString *khmerKeyboardPath = [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"khmer_angkor.kmx"];

  NSLog(@"mockKmxFilePath  = %@\n", mockKmxFilePath);
  
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
  
  // expecting two actions: character action with 'Ç' and end action
  NSArray *actions = [core processMacVirtualKey:MVK_A withModifiers:0 withKeyDown:YES];
  XCTAssert(actions.count == 2, @"Expected 2 actions");
  CoreAction *charAction = actions[0];
  XCTAssert(charAction.actionType==CharacterAction, @"Expected CharacterAction");
  NSString *content = charAction.content;
  XCTAssert([content isEqualToString:@"\u00C7"], @"Expected capital C cedille (U+00C7)");
  CoreAction *endAction = actions[1];
  XCTAssert(endAction.actionType==EndAction, @"Expected EndAction");
}

- (void)testgetContextAsString_ContextContainsEmojis_ReturnsSameContext  {
  NSString *kmxPath = [CoreTestStaticHelperMethods getKmxFilePathTestMacEngine];
  CoreWrapper *core = [[CoreWrapper alloc] initWithHelper: [CoreTestStaticHelperMethods helper] kmxFilePath:kmxPath];
  [core setContext:@"🤔?👍🏻✅"];
  NSString *finalContext = core.context;
  XCTAssert([finalContext isEqualToString:@"🤔?👍🏻✅"], @"Expected '🤔?👍🏻✅' in context buffer");
}

@end
