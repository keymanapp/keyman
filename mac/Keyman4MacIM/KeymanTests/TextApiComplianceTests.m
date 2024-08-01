/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * TextApiComplianceTests.m
 * KeymanTests
 *
 * Created by Shawn Schantz on 2024-04-08.
 *
 * Unit tests of TextApiCompliance class
 */

#import <XCTest/XCTest.h>
#import "TextApiCompliance.h"
#import "AppleCompliantTestClient.h"
#import "KMLogs.h"

@interface TextApiComplianceTests : XCTestCase
@end

// included following interface that we can see and test private methods of TextApiCompliance
@interface TextApiCompliance (Testing)
@property NSRange initialSelection;
- (BOOL)validateNewLocation:(NSUInteger)location delete:(NSString *)textToDelete;
- (void) validateLocationChange:(BOOL) changeExpected hasLocationChanged:(BOOL) locationChanged;
- (BOOL)isLocationChangeExpectedOnInsert:(NSString *)textToDelete insert:(NSString *)textToInsert;
- (BOOL)hasLocationChanged:(NSRange)newSelection;
- (BOOL)arrayContainsApplicationId:(NSString *)clientAppId fromArray:(NSArray *)legacyApps;
@end

@implementation TextApiComplianceTests

- (void)setUp {
  // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
  // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testValidateNewLocation_locationNotFound_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  BOOL isNewLocationValid = [apiCompliance validateNewLocation:NSNotFound delete:@"a"];
  XCTAssertFalse(isNewLocationValid, @"New location is not valid if equal to NSNotFound");
}

- (void)testValidateNewLocation_deleteAtLocationZero_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  BOOL isNewLocationValid = [apiCompliance validateNewLocation:0 delete:@"a"];
  XCTAssertFalse(isNewLocationValid, @"New location is not valid if attempting to delete at location of zero");
}

- (void)testValidateNewLocation_deleteAtLocationOne_returnsYes {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  BOOL isNewLocationValid = [apiCompliance validateNewLocation:1 delete:@"a"];
  XCTAssertTrue(isNewLocationValid, @"New location is valid if attempting to delete at location of one");
}

- (void)testIsLocationChangeExpectedOnInsert_nilDeleteString_returnsYes {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  BOOL isExpected = [apiCompliance isLocationChangeExpectedOnInsert:nil insert:@"a"];
  XCTAssertTrue(isExpected, @"Location change is expected for nil delete string and non-empty insert string");
}

- (void)testIsLocationChangeExpectedOnInsert_emptyDeleteString_returnsYes {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  BOOL isExpected = [apiCompliance isLocationChangeExpectedOnInsert:@"" insert:@"a"];
  XCTAssertTrue(isExpected, @"Location change is expected for empty delete string and non-empty insert string");
}

- (void)testIsLocationChangeExpectedOnInsert_deleteAndInsertOneCharacter_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  BOOL isExpected = [apiCompliance isLocationChangeExpectedOnInsert:@"a" insert:@"X"];
  XCTAssertFalse(isExpected, @"Location change not expected for delete string and insert string of same length");
}

- (void)testIsLocationChangeExpectedOnInsert_deleteAndInsertSurrogatePair_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  BOOL isExpected = [apiCompliance isLocationChangeExpectedOnInsert:@"𑀓" insert:@"𑀢"];
  XCTAssertFalse(isExpected, @"Location change not expected for delete string and insert string of same length");
}

- (void)testHasLocationChanged_zeroForBoth_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  NSRange zeroSelection = NSMakeRange(0, 0);
  apiCompliance.initialSelection = zeroSelection;
  
  BOOL hasChanged = [apiCompliance hasLocationChanged:zeroSelection];
  XCTAssertFalse(hasChanged, @"Location change unexpected for matching selections");
}

/**
 should not encounter the unintuitive case
 of moving from a discrete location to NSNotFound because we will
 catch this first with the method validateNewLocation
 */
- (void)testHasLocationChanged_newLocationNotFound_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  NSRange notFoundSelection = NSMakeRange(NSNotFound, NSNotFound);
  apiCompliance.initialSelection = notFoundSelection;
  
  BOOL hasChanged = [apiCompliance hasLocationChanged:notFoundSelection];
  XCTAssertFalse(hasChanged, @"Location change unexpected for selection of NSNotFound");
}

/**
 * unlikely scenario, because if the initial location is notFound, then the app is
 * probably non-complianat because we are still unable to get the current selection
 */
- (void)testHasLocationChanged_initialLocationNotFound_returnsYes {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  NSRange notFoundSelection = NSMakeRange(NSNotFound, NSNotFound);
  apiCompliance.initialSelection = notFoundSelection;
  
  BOOL hasChanged = [apiCompliance hasLocationChanged:NSMakeRange(0, 0)];
  XCTAssertTrue(hasChanged, @"Location change unexpected for selection of NSNotFound");
}

- (void)testHasLocationChanged_fromZeroToOne_returnsYes {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  apiCompliance.initialSelection = NSMakeRange(0, 0);
  
  BOOL hasChanged = [apiCompliance hasLocationChanged:NSMakeRange(1, 0)];
  XCTAssertTrue(hasChanged, @"Location change expected for inserting one char");
}


- (void)testIsClientAppLegacy_unlistedClientAppId_returnsNo {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  
  NSArray *legacyAppsArray = [NSArray arrayWithObjects:@"com.microsoft.VSCode",@"com.adobe.Photoshop",nil];
  
  BOOL isLegacy = [apiCompliance arrayContainsApplicationId:clientAppId fromArray:legacyAppsArray];
  os_log_debug([KMLogs testLog], "isLegacy = %@", isLegacy?@"yes":@"no");
  XCTAssertFalse(isLegacy, @"App not expected to be in legacy list");
}

- (void)testIsClientAppLegacy_listedClientAppId_returnsYes {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.microsoft.VSCode";
  TextApiCompliance *apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:clientAppId];
  
  NSArray *legacyAppsArray = [NSArray arrayWithObjects:@"com.adobe.Photoshop",@"com.microsoft.VSCode",nil];
  
  BOOL isLegacy = [apiCompliance arrayContainsApplicationId:clientAppId fromArray:legacyAppsArray];
  XCTAssertTrue(isLegacy, @"App expected to be in legacy list");
}

@end
