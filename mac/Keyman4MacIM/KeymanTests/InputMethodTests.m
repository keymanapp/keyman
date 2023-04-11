/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * InputMethodTests.m
 * KeymanTests
 * 
 * Created by Shawn Schantz on 2023-03-29.
 * 
 * Description...
 */

#import <XCTest/XCTest.h>
#import "KMInputMethodEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#import "LegacyTestClient.h"
#import "AppleCompliantTestClient.h"

@interface InputMethodTests : XCTestCase

@end

@implementation InputMethodTests

- (void)setUp {
    // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testIsClientAppLegacy_unlistedClientAppId_returnsNo {
  KMInputMethodEventHandler * inputMethod =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
  id client = [[AppleCompliantTestClient alloc] init];

  NSString *clientAppId = @"com.compliant.app";
  NSArray *legacyAppsArray = [NSArray arrayWithObjects:@"com.microsoft.VSCode",@"com.adobe.Photoshop",nil];

  BOOL isLegacy = [inputMethod isClientAppLegacy:clientAppId fromArray:legacyAppsArray];
  NSLog(@"isLegacy = %@", isLegacy?@"yes":@"no");
    XCTAssert(isLegacy == NO, @"App not expected to be in legacy list");
}

- (void)testIsClientAppLegacy_listedClientAppId_returnsYes {
  KMInputMethodEventHandler * inputMethod =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
  id client = [[AppleCompliantTestClient alloc] init];

  NSString *clientAppId = @"com.microsoft.VSCode";
  NSArray *legacyAppsArray = [NSArray arrayWithObjects:@"com.adobe.Photoshop",@"com.microsoft.VSCode",nil];

  BOOL isLegacy = [inputMethod isClientAppLegacy:clientAppId fromArray:legacyAppsArray];
  NSLog(@"isLegacy = %@", isLegacy?@"yes":@"no");
    XCTAssert(isLegacy == YES, @"App expected to be in legacy list");
}

@end
