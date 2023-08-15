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
#import "keyboardprocessor.h"
#import "CoreAction.h"
#import "ActionArrayOptimizer.h"

@interface CoreHelperTests : XCTestCase

@end

@implementation CoreHelperTests

ActionArrayOptimizer *optimizer;

+ (void)setUp {
  optimizer  = [[ActionArrayOptimizer alloc] init];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
}

- (void)testKeycodeConversion_MacA_ReturnsWindowsA {
  const unsigned int MAC_A_KEYCODE = 0;
  // Mac 'A' key = 0, Windows 'S' key = 83;
  uint16_t windowsKeyCode = [[CoreTestStaticHelperMethods helper] macVirtualKeyToWindowsVirtualKey:MAC_A_KEYCODE];
  XCTAssertEqual(windowsKeyCode, KM_KBP_VKEY_A, @"Unexpected conversion from Mac to Windows key code.");
}

- (void)testKeycodeConversion_MacS_ReturnsWindowsS {
  const unsigned int MAC_S_KEYCODE = 1;
  // Mac 'S' key = 1, Windows 'S' key = 65;
  uint16_t windowsKeyCode = [[CoreTestStaticHelperMethods helper] macVirtualKeyToWindowsVirtualKey:MAC_S_KEYCODE];
  XCTAssertEqual(windowsKeyCode, KM_KBP_VKEY_S, @"Unexpected conversion from Mac to Windows key code.");
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
  XCTAssertEqual(keymanModifierState, KM_KBP_MODIFIER_SHIFT, @"Failed conversion of shift modifier from Mac to Keyman.");
}

- (void)testModifierConversion_MacOption_ReturnsKeymanAlt {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagOption];
  XCTAssertEqual(keymanModifierState, KM_KBP_MODIFIER_ALT, @"Failed conversion of option modifier from Mac to Keyman alt.");
}

- (void)testModifierConversion_MacControl_ReturnsKeymanControl {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagControl];
  XCTAssertEqual(keymanModifierState, KM_KBP_MODIFIER_CTRL, @"Failed conversion of control modifier from Mac to Keyman  control.");
}

- (void)testUnsupportedModifierConversion_MacHelpKeyModifier_ReturnsZero {
  uint32_t keymanModifierState = [[CoreTestStaticHelperMethods helper] macToKeymanModifier:NSEventModifierFlagHelp];
  XCTAssertEqual(keymanModifierState, 0, @"Failed conversion of Mac Help key flag from Mac to Keyman cleared modifier state.");
}

- (void)testUnicharStringConversion_optionName_matchesLiteral {
  NSString *keyString = @"option_ligature_ew";
  unichar const * keyUnicharString = u"option_ligature_ew";
  
  NSUInteger stringLength = [keyString lengthOfBytesUsingEncoding:NSUTF16StringEncoding];
  
  unichar const *  convertedKeyString = [CoreHelper createUnicharStringFromNSString: keyString];
  NSData *dataFromConversion = [NSData dataWithBytes:convertedKeyString length:stringLength];
  NSData *dataFromLiteral = [NSData dataWithBytes:keyUnicharString length:stringLength];
  XCTAssertTrue([dataFromLiteral isEqualToData:dataFromConversion], @"Converted unichar string is not equal to literal unichar string.");
}

- (void)testUnicharStringConversion_integer_matchesLiteral {
  NSString *keyString = @"1";
  unichar const * keyUnicharString = u"1";
  
  NSUInteger stringLength = [keyString lengthOfBytesUsingEncoding:NSUTF16StringEncoding];
  
  unichar const *  convertedKeyString = [CoreHelper createUnicharStringFromNSString: keyString];
  NSData *dataFromConversion = [NSData dataWithBytes:convertedKeyString length:stringLength];
  NSData *dataFromLiteral = [NSData dataWithBytes:keyUnicharString length:stringLength];
  XCTAssertTrue([dataFromLiteral isEqualToData:dataFromConversion], @"Converted unichar string is not equal to literal unichar string.");
}


/*
- (void)testOptimize_MultipleCharacterActions_CombinedToSingleAction {
  CoreAction *characterAAction = [[CoreAction alloc] initWithType:CharacterAction actionContent:@"A" backspaceCount:0];
  CoreAction *characterBAction = [[CoreAction alloc] initWithType:CharacterAction actionContent:@"B" backspaceCount:0];
  CoreAction *endAction = [[CoreAction alloc] initWithType:EndAction actionContent:@"" backspaceCount:0];
  NSArray *coreArray = @[characterAAction, characterBAction, endAction];
  
  NSArray *optimizedArray = [optimizer optimize:coreArray];
  XCTAssert(optimizedArray.count == 1, @"Expected 1 action");
  CoreAction *action = optimizedArray[0];
  XCTAssert([action.content isEqualToString:@"AB"], @"Expected combined string.");
}

- (void)testOptimize_MultipleBackspaceActions_CombinedToSingleAction {
  CoreAction *backspaceAction = [[CoreAction alloc] initWithType:CharacterBackspaceAction actionContent:@"" backspaceCount:1];
  CoreAction *anotherBackspaceAction = [[CoreAction alloc] initWithType:CharacterBackspaceAction actionContent:@"" backspaceCount:1];
  CoreAction *endAction = [[CoreAction alloc] initWithType:EndAction actionContent:@"" backspaceCount:0];
  NSArray *coreArray = @[backspaceAction, anotherBackspaceAction, endAction];
  
  NSArray *optimizedArray = [optimizer optimize:coreArray];
  XCTAssert(optimizedArray.count == 1, @"Expected 1 action");
  CoreAction *action = optimizedArray[0];
  XCTAssert(action.backspaceCount==2, @"Expected a backspace count of 2.");
}
*/
- (void)testOptimize_OneBackspaceAndOneCharacterAction_RetainedWithEndActionStripped {
  CoreAction *backspaceAction = [[CoreAction alloc] initCharacterBackspaceAction:@""];
  CoreAction *characterAAction = [[CoreAction alloc] initCharacterAction:@"A"];
  CoreAction *endAction = [[CoreAction alloc] initWithType:EndAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
  NSArray *coreArray = @[backspaceAction, characterAAction, endAction];
  
  NSArray *optimizedArray = [optimizer optimize:coreArray];
  XCTAssert(optimizedArray.count == 2, @"Expected two actions.");
  CoreAction *action = optimizedArray[1];
  XCTAssert([action.content isEqualToString:@"A"], @"Expected CharacterAction.");
}

- (void)testOptimize_OneCharacterAndOneBackspaceAction_CompactedToEmptyArray {
  CoreAction *characterAAction = [[CoreAction alloc] initCharacterAction:@"A"];
  CoreAction *backspaceAction = [[CoreAction alloc] initCharacterBackspaceAction:@""];
  CoreAction *endAction = [[CoreAction alloc] initWithType:EndAction actionContent:@"" backspaceCount:0 key:@"" value:@"" scope:0];
  NSArray *coreArray = @[characterAAction, backspaceAction, endAction];
  
  NSArray *optimizedArray = [optimizer optimize:coreArray];
  XCTAssert(optimizedArray.count == 0, @"Expected empty array");
}

- (void)testHelperCreationPerformance {
    // This is an example of a performance test case.
    [self measureBlock:^{
      CoreHelper *helper = [[CoreHelper alloc] init];
    }];
}

@end
