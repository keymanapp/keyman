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
#import "AppleCompliantTestClient.h"
#import "TextApiCompliance.h"
#import "KMSettingsRepository.h"
#import "KMDataRepository.h"
#import <os/log.h>

KMInputMethodEventHandler *testEventHandler = nil;
id testClient = nil;

@interface InputMethodTests : XCTestCase
@end

// redeclare private properties and functions to expose for testing
@interface KMInputMethodEventHandler (Testing)
@property (nonatomic, retain) TextApiCompliance* apiCompliance;
@property (nonatomic, retain) NSString* clientApplicationId;
@property BOOL contextChanged;
- (instancetype)initWithClient:(NSString *)clientAppId client:(id) sender;
- (NSRange) calculateInsertRangeForDeletedText:(NSString*)textToDelete selectionRange:(NSRange) selection;
- (void)checkTextApiCompliance:(id)client;
- (ReplacementInfo)evaluateForReplaceability:(NSString*) context
                           textStoreLocation:(NSUInteger)textStoreLocation
                                deleteLength:(NSUInteger)deleteLength
                    locationOfDeletionTarget:(NSUInteger)deletionLocation;
@end

@implementation InputMethodTests

- (void)setUp {
  testClient = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  testEventHandler = [[KMInputMethodEventHandler alloc]initWithClient:clientAppId client:testClient];
}

- (void)tearDown {
  // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testCalculateInsertRange_noDelete_returnsCurrentLocation {
  NSRange selectionRange = NSMakeRange(1, 0);
  NSRange insertRange = [testEventHandler calculateInsertRangeForDeletedText:@"" selectionRange:selectionRange];
  BOOL notFound = (insertRange.length == NSNotFound) && (insertRange.location == NSNotFound);
  XCTAssertTrue(notFound, @"insert or replacement range expected to be NSNotFound ");
}

- (void)testCalculateInsertRange_noDeleteWithOneSelected_returnsCurrentLocation {
  NSRange selectionRange = NSMakeRange(1, 1);
  NSRange insertRange = [testEventHandler calculateInsertRangeForDeletedText:@"" selectionRange:selectionRange];
  BOOL notFound = (insertRange.length == NSNotFound) && (insertRange.location == NSNotFound);
  XCTAssertTrue(notFound, @"insert or replacement range expected to be NSNotFound ");
}

- (void)testCalculateInsertRange_deleteNonexistentCharacters_returnsCurrentLocation {
  NSRange selectionRange = NSMakeRange(0, 0);
  NSRange insertRange = [testEventHandler calculateInsertRangeForDeletedText:@"a" selectionRange:selectionRange];
  BOOL notFound = (insertRange.length == NSNotFound) && (insertRange.location == NSNotFound);
  XCTAssertTrue(notFound, @"insert or replacement range expected to be NSNotFound ");
}

- (void)testCalculateInsertRange_deleteOneSurrogatePair_returnsRangeLengthTwo {
  // Brahmi 𑀓 D804 DC13 surrogate pair
  NSRange selectionRange = NSMakeRange(2, 0);
  NSRange insertRange = [testEventHandler calculateInsertRangeForDeletedText:@"𑀓" selectionRange:selectionRange];
  BOOL correctResult = (insertRange.location == 0) && (insertRange.length == 2);
  XCTAssertTrue(correctResult, @"insert or replacement range expected to be {0,2}");
}

- (void)testCalculateInsertRange_deleteTwoBMPCharacters_returnsRangeLengthTwo {
  NSRange selectionRange = NSMakeRange(3, 0);
  // deletedText = KHMER VOWEL SIGN OE, KHMER SIGN COENG
  NSRange insertRange = [testEventHandler calculateInsertRangeForDeletedText:@"\u17BE\u17D2" selectionRange:selectionRange];
  BOOL correctResult = (insertRange.location == 1) && (insertRange.length == 2);
  XCTAssertTrue(correctResult, @"insert or replacement range expected to be {1,2}");
}

// equivalent to Kenya BTL, type "b^x", select 'x' and type 'u'
// selected x should be deleted, as should "^" and replaced with 'û'

- (void)testCalculateInsertRange_deleteOneBMPCharacterWithOneSelected_returnsRangeLengthTwo {
  NSRange selectionRange = NSMakeRange(2, 1); // location = 2, length = 1
  NSRange insertRange = [testEventHandler calculateInsertRangeForDeletedText:@"'" selectionRange:selectionRange];
  BOOL correctResult = (insertRange.location == 1) && (insertRange.length == 2);
  XCTAssertTrue(correctResult, @"insert or replacement range expected to be {1,2}");
}

- (void)testEvaluateReplacement_precededByControlCharacter_canReplaceFalse {
  NSString *context = @"test\ta";
  NSUInteger storeLocation = 100;
  NSUInteger deletionLength = 1;
  NSUInteger locationOfDeletion = 5;

  ReplacementInfo replacementInfo = [testEventHandler evaluateForReplaceability: context textStoreLocation:storeLocation deleteLength:deletionLength locationOfDeletionTarget:locationOfDeletion];
  XCTAssertFalse(replacementInfo.canDeleteWithReplacement, @"target preceded by control character, canDelete should be false");
}

- (void)testEvaluateReplacement_precededByLatinCharacter_canReplaceTrue {
  NSString *context = @"testxa";
  NSUInteger storeLocation = 100;
  NSUInteger deletionLength = 1;
  NSUInteger locationOfDeletion = 5;
  
  ReplacementInfo replacementInfo = [testEventHandler evaluateForReplaceability: context textStoreLocation:storeLocation deleteLength:deletionLength locationOfDeletionTarget:locationOfDeletion];
  
  XCTAssertTrue([replacementInfo.replacementString isEqualToString:@"x"], @"replacementString should be == 'x'");
  XCTAssertTrue(replacementInfo.replacementLength == 2, @"replacementLength should be 2");
}

//𑒏𑒹

- (void)testEvaluateReplacement_latinCharacterPrecededBySurrogatePair_replacementLengthThree {
  NSString *context = @"test.𑒏a";
  NSString *stringToDelete = @"a";
  
  NSRange range = [context rangeOfString:stringToDelete];
  
  NSUInteger storeLocation = 100;
  NSUInteger deletionLength = stringToDelete.length;
  NSUInteger locationOfDeletion = range.location;
  
  ReplacementInfo replacementInfo = [testEventHandler evaluateForReplaceability: context textStoreLocation:storeLocation deleteLength:deletionLength locationOfDeletionTarget:locationOfDeletion];
  
  XCTAssertTrue([replacementInfo.replacementString isEqualToString:@"𑒏"], @"replacementString is incorrect");
  XCTAssertTrue(replacementInfo.replacementLength == 3, @"replacementLength should == 3");
}

- (void)testEvaluateReplacement_surrogatePairPrecededByLatinCharacter_replacementLengthThree {
  NSString *context = @"test.a𑒏";
  NSString *stringToDelete = @"𑒏";
  
  NSRange range = [context rangeOfString:stringToDelete];
  
  NSUInteger storeLocation = 100;
  NSUInteger deletionLength = stringToDelete.length;
  NSUInteger locationOfDeletion = range.location;
  
  ReplacementInfo replacementInfo = [testEventHandler evaluateForReplaceability: context textStoreLocation:storeLocation deleteLength:deletionLength locationOfDeletionTarget:locationOfDeletion];
  
  XCTAssertTrue([replacementInfo.replacementString isEqualToString:@"a"], @"replacementString is incorrect");
  XCTAssertTrue(replacementInfo.replacementLength == 3, @"replacementLength should == 3");
}

- (void)testEvaluateReplacement_latinCharacterPrecededByCheckeredFlag_replacementLengthThree {
  NSString *context = @"test🏁a";
  NSString *stringToDelete = @"a";
  
  NSRange range = [context rangeOfString:stringToDelete];
  
  NSUInteger storeLocation = 100;
  NSUInteger deletionLength = stringToDelete.length;
  NSUInteger locationOfDeletion = range.location;
  
  ReplacementInfo replacementInfo = [testEventHandler evaluateForReplaceability: context textStoreLocation:storeLocation deleteLength:deletionLength locationOfDeletionTarget:locationOfDeletion];
  
  XCTAssertTrue([replacementInfo.replacementString isEqualToString:@"🏁"], @"replacementString should be == '🏁'");
  XCTAssertTrue(replacementInfo.replacementLength == 3, @"replacementLength should == 3");
}

- (void)testEvaluateReplacement_checkeredFlagPrecededByLatinCharacter_replacementLengthThree {
  NSString *context = @"testa🏁";
  NSString *stringToDelete = @"🏁";
  
  NSRange range = [context rangeOfString:stringToDelete];
  
  NSUInteger storeLocation = 100;
  NSUInteger deletionLength = stringToDelete.length;
  NSUInteger locationOfDeletion = range.location;
  
  ReplacementInfo replacementInfo = [testEventHandler evaluateForReplaceability: context textStoreLocation:storeLocation deleteLength:deletionLength locationOfDeletionTarget:locationOfDeletion];
  
  XCTAssertTrue([replacementInfo.replacementString isEqualToString:@"a"], @"replacementString should be == 'a'");
  XCTAssertTrue(replacementInfo.replacementLength == 3, @"replacementLength should == 3");
}

// Note: though this test checks the calculation, deleting the jolly roger emoji will currently not behave as expected.
// Keyman core instructs only part of the emoji to be deleted. The same happens with emoji with skin tones.
- (void)testEvaluateReplacement_jollyRogerPrecededByLatinCharacter_replacementLengthSix {
  NSString *context = @"testa🏴‍☠️";
  NSString *stringToDelete = @"🏴‍☠️";
  
  NSRange range = [context rangeOfString:stringToDelete];
  
  NSUInteger storeLocation = 100;
  NSUInteger deletionLength = stringToDelete.length;
  NSUInteger locationOfDeletion = range.location;
  
  ReplacementInfo replacementInfo = [testEventHandler evaluateForReplaceability: context textStoreLocation:storeLocation deleteLength:deletionLength locationOfDeletionTarget:locationOfDeletion];
  
  XCTAssertTrue([replacementInfo.replacementString isEqualToString:@"a"], @"replacementString should be == 'a'");
  XCTAssertTrue(replacementInfo.replacementLength == 6, @"replacementLength should == 6");
}

/**
 * test compliance check of a KMInputMethodEventHandler with a nil client application ID
 * not sure if this can ever happen, but the lifecycle of the input method is not 100% clear,
 * and we would like this scenario, if it can occur, to not result in a crash
 */
- (void)testCheckCompliance_withUnknownApplicationId_createsComplianceObject {
  id client = [[AppleCompliantTestClient alloc] init];
  KMInputMethodEventHandler *eventHandler = [[KMInputMethodEventHandler alloc]initWithClient:nil client:client];
  [eventHandler checkTextApiCompliance:client];
  XCTAssertNotNil(eventHandler.apiCompliance, @"apiCompliance object was not created");
}

- (void)testCheckCompliance_withNilComplianceObject_createsComplianceObject {
  [testEventHandler checkTextApiCompliance:testClient];
  XCTAssertNotNil(testEventHandler.apiCompliance, @"apiCompliance object was not created");
}

- (void)testCheckCompliance_withChangedClientApplicationId_createsNewComplianceObject {
  // first call causes textApiCompliance object to be created
  [testEventHandler checkTextApiCompliance:testClient];
  TextApiCompliance *originalComplianceObject = testEventHandler.apiCompliance;
  
  testEventHandler.clientApplicationId = @"com.different.app";
  // second call causes new textApiCompliance object to be created due to stale application ID
  [testEventHandler checkTextApiCompliance:testClient];
  XCTAssertNotEqualObjects(originalComplianceObject, testEventHandler.apiCompliance, @"New TextApiCompliance object not created for new client application ID");
}

- (void)testCheckCompliance_withContextChanged_createsNewComplianceObject {
  // first call causes textApiCompliance object to be created
  [testEventHandler checkTextApiCompliance:testClient];
  TextApiCompliance *originalComplianceObject = testEventHandler.apiCompliance;
  
  testEventHandler.contextChanged = YES;
  // second call causes new textApiCompliance object to be created due to setting contextChanged flag
  [testEventHandler checkTextApiCompliance:testClient];
  XCTAssertNotEqual(originalComplianceObject, testEventHandler.apiCompliance, @"New TextApiCompliance object not created after contextChanged flag set");
  XCTAssertEqualObjects(originalComplianceObject, testEventHandler.apiCompliance, @"New TextApiCompliance object not functionally equal to original object");
}

@end
