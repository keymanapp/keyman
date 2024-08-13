/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreHelperTests.m
 * CoreHelperTests
 * 
 * Created by Shawn Schantz on 2022-12-12.
 * 
 * Description...
 */

#import <XCTest/XCTest.h>
#import "CoreHelper.h"
#import <Cocoa/Cocoa.h>
#import "CoreTestStaticHelperMethods.h"
#import "keyman_core_api.h"
#import "CoreAction.h"

@interface CoreHelperTests : XCTestCase

@end

@implementation CoreHelperTests

+ (void)setUp {
}

- (void)tearDown {
  // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testKeycodeConversion_MacA_ReturnsWindowsA {
  const unsigned int MAC_A_KEYCODE = 0;
  // Mac 'A' key = 0, Windows 'S' key = 83;
  uint16_t windowsKeyCode = [[CoreTestStaticHelperMethods helper] macVirtualKeyToWindowsVirtualKey:MAC_A_KEYCODE];
  XCTAssertEqual(windowsKeyCode, KM_CORE_VKEY_A, @"Unexpected conversion from Mac to Windows key code.");
}

- (void)testKeycodeConversion_MacS_ReturnsWindowsS {
  const unsigned int MAC_S_KEYCODE = 1;
  // Mac 'S' key = 1, Windows 'S' key = 65;
  uint16_t windowsKeyCode = [[CoreTestStaticHelperMethods helper] macVirtualKeyToWindowsVirtualKey:MAC_S_KEYCODE];
  XCTAssertEqual(windowsKeyCode, KM_CORE_VKEY_S, @"Unexpected conversion from Mac to Windows key code.");
}

- (void)testKeycodeConversion_NegativeKeycode_ReturnsZero {
  const unsigned short int INVALID_KEYCODE = -1;
  uint16_t windowsKeyCode = [[CoreTestStaticHelperMethods helper] macVirtualKeyToWindowsVirtualKey:INVALID_KEYCODE];
  XCTAssertEqual(windowsKeyCode, 0, @"Did not return zero for invalid Mac keycode.");
}

- (void)testKeycodeConversion_LargeKeycode_ReturnsZero {
  const unsigned short int INVALID_KEYCODE = 10000;
  uint16_t windowsKeyCode = [[CoreTestStaticHelperMethods helper] macVirtualKeyToWindowsVirtualKey:INVALID_KEYCODE];
  XCTAssertEqual(windowsKeyCode, 0, @"Did not return zero for invalid Mac keycode.");
}

- (void)testModifierConversion_MacShift_ReturnsKeymanShift {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagShift];
  XCTAssertEqual(keymanModifierState, KM_CORE_MODIFIER_SHIFT, @"Failed conversion of shift modifier from Mac to Keyman.");
}

- (void)testModifierConversion_MacCapsLock_ReturnsKeymanCapsLock {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagCapsLock];
  XCTAssertEqual(keymanModifierState, KM_CORE_MODIFIER_CAPS, @"Failed conversion of caps lock modifier from Mac to Keyman.");
}

/**
 * Not really intuitive, but we should not be setting the NOCAPS bit for Keyman Core when Caps Lock is not set.
 * Apparently, Core knows that Caps Lock is not set because the CAPS bit is cleared.
 */
- (void)testModifierConversion_MacModifierWithCapsLock_ReturnsKeymanClearedNoCapsLock {
  // use command key flag just to pick something that is not caps lock
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagCommand];
  BOOL noCapsFlag = !(keymanModifierState & KM_CORE_MODIFIER_NOCAPS);
  XCTAssertTrue(noCapsFlag, @"Failed modifier flag conversion for no caps lock case.");
}

- (void)testModifierConversion_MacOption_ReturnsKeymanAlt {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagOption];
  XCTAssertEqual(keymanModifierState, KM_CORE_MODIFIER_ALT, @"Failed conversion of option modifier from Mac to Keyman alt.");
}

- (void)testModifierConversion_MacControl_ReturnsKeymanControl {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagControl];
  XCTAssertEqual(keymanModifierState, KM_CORE_MODIFIER_CTRL, @"Failed conversion of control modifier from Mac to Keyman  control.");
}

- (void)testUnsupportedModifierConversion_MacHelpKeyModifier_ReturnsZero {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagHelp];
  XCTAssertEqual(keymanModifierState, 0, @"Failed conversion of Mac Help key flag from Mac to Keyman cleared modifier state.");
}

- (void)testConversionToUnicharString_optionName_matchesLiteral {
  NSString *optionNameString = @"option_ligature_ew";
  unichar const * keyUnicharString = u"option_ligature_ew";
  
  NSUInteger stringLength = [optionNameString lengthOfBytesUsingEncoding:NSUTF16StringEncoding];
  
  unichar const *  convertedString = [[CoreTestStaticHelperMethods helper] createUnicharStringFromNSString: optionNameString];
  NSData *dataFromConversion = [NSData dataWithBytes:convertedString length:stringLength];
  NSData *dataFromLiteral = [NSData dataWithBytes:keyUnicharString length:stringLength];
  XCTAssertTrue([dataFromLiteral isEqualToData:dataFromConversion], @"Converted unichar string is not equal to literal unichar string.");
}

- (void)testConversionToUnicharString_number_matchesLiteral {
  NSString *numberString = @"1";
  unichar const * unicharString = u"1";
  
  NSUInteger stringLength = [numberString lengthOfBytesUsingEncoding:NSUTF16StringEncoding];
  
  unichar const *  convertedString = [[CoreTestStaticHelperMethods helper] createUnicharStringFromNSString: numberString];
  NSData *dataFromConversion = [NSData dataWithBytes:convertedString length:stringLength];
  NSData *dataFromLiteral = [NSData dataWithBytes:unicharString length:stringLength];
  XCTAssertTrue([dataFromLiteral isEqualToData:dataFromConversion], @"Converted unichar string is not equal to literal unichar string.");
}

- (void)testConversionToUnicharString_brahmiAndLatinString_matchesLiteral {
  NSString *mixedString = @"𑀧a𑀫";
  unichar const * mixedUnicharString = u"𑀧a𑀫";
  
  NSUInteger stringLength = [mixedString lengthOfBytesUsingEncoding:NSUTF16StringEncoding];
  
  unichar const *  convertedString = [[CoreTestStaticHelperMethods helper] createUnicharStringFromNSString: mixedString];
  NSData *dataFromConversion = [NSData dataWithBytes:convertedString length:stringLength];
  NSData *dataFromLiteral = [NSData dataWithBytes:mixedUnicharString length:stringLength];
  XCTAssertTrue([dataFromLiteral isEqualToData:dataFromConversion], @"Converted unichar string is not equal to literal unichar string.");
}

- (void)testConversionFromUnicharString_optionName_matchesLiteral {
  unichar const * unicharString = u"option_ligature_ew";
  NSString *optionNameString = @"option_ligature_ew";
  
  NSString *convertedString = [[CoreTestStaticHelperMethods helper] createNSStringFromUnicharString:unicharString];
  
  XCTAssertTrue([convertedString isEqual:optionNameString], @"Converted unichar string is not equal to literal string.");
}

- (void)testHelperCreationPerformance {
  // This is an example of a performance test case.
  [self measureBlock:^{
    CoreHelper *helper = [[CoreHelper alloc] init];
  }];
}

@end
