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
#import "TextApiCompliance.h"

@interface InputMethodTests : XCTestCase

@end

// included following interface that we can see and test private methods of TextApiCompliance
@interface TextApiCompliance (Testing)

- (BOOL)isClientAppLegacy:(NSString *)clientAppId fromArray:(NSArray *)legacyApps;

@end

@implementation InputMethodTests

- (void)setUp {
    // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testIsClientAppLegacy_unlistedClientAppId_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];

  NSArray *legacyAppsArray = [NSArray arrayWithObjects:@"com.microsoft.VSCode",@"com.adobe.Photoshop",nil];

  BOOL isLegacy = [apiCompliance isClientAppLegacy:clientAppId fromArray:legacyAppsArray];
  NSLog(@"isLegacy = %@", isLegacy?@"yes":@"no");
    XCTAssert(isLegacy == NO, @"App not expected to be in legacy list");
}

- (void)testIsClientAppLegacy_listedClientAppId_returnsYes {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.microsoft.VSCode";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];

  NSArray *legacyAppsArray = [NSArray arrayWithObjects:@"com.adobe.Photoshop",@"com.microsoft.VSCode",nil];

  BOOL isLegacy = [apiCompliance isClientAppLegacy:clientAppId fromArray:legacyAppsArray];
  NSLog(@"isLegacy = %@", isLegacy?@"yes":@"no");
    XCTAssert(isLegacy == YES, @"App expected to be in legacy list");
}

@end
