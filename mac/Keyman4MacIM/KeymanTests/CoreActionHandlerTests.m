/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreActionHandlerTests.m
 * KeymanTests
 * 
 * Created by Shawn Schantz on 2023-06-22.
 * 
 * Description...
 */

#import <XCTest/XCTest.h>
#import "KMCoreActionHandler.h"
#import "KMContext.h"

//#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

@interface CoreActionHandlerTests : XCTestCase

@end

@implementation CoreActionHandlerTests

- (void)setUp {
    // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testExample {
  
  NSArray *actions = nil;
  KMContext *context = [[KMContext alloc] init];
  unsigned int keycode = 0;
  
  KMCoreActionHandler *actionHandler = [[KMCoreActionHandler alloc] initWithActions:actions keyCode: keycode];

    // This is an example of a functional test case.
    // Use XCTAssert and related functions to verify your tests produce the correct results.
  NSLog(@"*** KMCoreActionHandler test is unimplemented.");
}

@end
