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

KMInputMethodEventHandler *testEventHandler = nil;

@interface InputMethodTests : XCTestCase
@end

@interface KMInputMethodEventHandler (Testing)

- (instancetype)initWithClient:(NSString *)clientAppId client:(id) sender;
- (NSRange) calculateInsertRangeForDeletedText:(NSString*)textToDelete selectionRange:(NSRange) selection;

@end

@implementation InputMethodTests

- (void)setUp {
  id client = [[AppleCompliantTestClient alloc] init];
  NSString *clientAppId = @"com.compliant.app";
  testEventHandler = [[KMInputMethodEventHandler alloc]initWithClient:clientAppId client:client];
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

@end
