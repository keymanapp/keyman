//
//  KMEngineTests.m
//  KeymanEngine4MacTests
//
//  Created by tom on 1/18/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <Carbon/Carbon.h>
#import "KeymanEngineTestsStaticHelperMethods.h"
#import "KMEngine.h"
#import "CoreAction.h"

@interface KMEngineTests : XCTestCase
@end

@implementation KMEngineTests
const int nCombinations = 14;
NSEventModifierFlags modifiers[nCombinations];
NSString * names[nCombinations];

- (void)setUp {
    [super setUp];
    // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    [super tearDown];
}

- (void)testinitWithKMX_NilKmx_ProcessEventReturnsNil {
    KMEngine *engine = [[KMEngine alloc] initWithKMX:nil context:@"" verboseLogging:YES];
    NSEvent *event = [[NSEvent alloc] init];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected processEvent to return nil for nil kmx");
}

- (void)testinitWithKMX_ValidKmxEmptyContext_InitializedWithEmptyContext {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    XCTAssert(engine != nil, @"Expected non-nil engine");
    XCTAssert(engine.getCoreContext.length == 0, @"Expected empty context buffer");
}

- (void)testinitWithKMX_ValidKmxNonEmptyContext_InitializedWithContext {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"abc" verboseLogging:YES];
    XCTAssert(engine != nil, @"Expected non-nil engine");
    XCTAssert([engine.getCoreContext isEqualToString:@"abc"], @"Expected 'abc' in context buffer");
}

- (void)testsetCoreContextIfNeeded_NonEmptyContext_InitialContextUpdated {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"a" verboseLogging:YES];
    [engine setCoreContextIfNeeded:@"xyz"];
    XCTAssert([engine.getCoreContext isEqualToString:@"xyz"], @"Expected 'xyz' in context buffer");
}

// TODO: re-enable this one after the core API km_core_state_context_set_if_needed is fixed
/*
- (void)testsetCoreContextIfNeeded_EmptyContext_InitialContextUpdated {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    [engine setCoreContextIfNeeded:@"xyz"];
    XCTAssert([engine.getCoreContext isEqualToString:@"xyz"], @"Expected 'xyz' in context buffer");
}
 */

- (void)testprocessEvent_eventForCommandKey_ReturnsNilActionsArray {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:NSEventModifierFlagCommand timestamp:0 windowNumber:0 context:nil characters:@"a" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:kVK_ANSI_A];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected nil array of actions");
}

- (void)testprocessEvent_eventWithoutKeycode_ReturnsNilActionsArray {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent mouseEventWithType:NSEventTypeMouseMoved location:NSMakePoint(29, 21) modifierFlags:NSEventModifierFlagShift timestamp:0 windowNumber:0 context:nil eventNumber:23 clickCount:0 pressure:0];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected nil array of actions");
}

// TODO: 
- (void)testprocessEvent_eventForUnmappedKey_ReturnsNoActions {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"z" charactersIgnoringModifiers:@"z" isARepeat:NO keyCode:kVK_ANSI_Z];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected nil array of actions");
}

- (void)testprocessEvent_eventForLowercaseA_ReturnsCharacterActionWithExpectedCharacterBasedOnKmx {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"a" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:kVK_ANSI_A];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 1, @"Expected 1 action");
    CoreAction *action = actions[0];
    XCTAssert([action isCharacter], @"Expected CharacterAction");
    XCTAssert([action.content isEqualToString:@"\u00C7"], @"Expected capital C cedille (U+00C7)");
}

// TODO: fails with core, investigate
/*
-(void)testprocessEvent_eventForCtrlShiftNumeralWithCipherMusicKmx_ReturnsQstrActionForCorrectUnicodeSurrogatePair {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForCipherMusicTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"" verboseLogging:YES];
    for (int i = 1; i <= 6; i++)
    {
        unsigned short ansiCode = kVK_ANSI_1 + i - 1;
        NSString * chars = [NSString stringWithFormat:@"%d", i];
        UTF32Char expectedUtf32Char;
        switch (i) {
            case 1: expectedUtf32Char = 0x1D100; break;
            case 2: expectedUtf32Char = 0x1D101; break;
            case 3: expectedUtf32Char = 0x1D103; break;
            case 4: expectedUtf32Char = 0x1D102; break;
            case 5:
                expectedUtf32Char = 0x1D106;
                ansiCode = kVK_ANSI_5; // 5 and 6 have codes swapped so they are not in sequential order
                break;
            case 6:
                expectedUtf32Char = 0x1D107;
                ansiCode = kVK_ANSI_6;
                break;
        }
        NSString * expectedOutput = [[NSString alloc] initWithBytes:&expectedUtf32Char length:4 encoding:NSUTF32LittleEndianStringEncoding];
        NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:NSEventModifierFlagShift|NSEventModifierFlagControl timestamp:0 windowNumber:0 context:nil characters:chars charactersIgnoringModifiers:chars isARepeat:NO keyCode:ansiCode];
        NSArray *actions = [engine processEvent:event];
        XCTAssert(actions.count == 1, @"Expected 1 action");
        NSDictionary *action = actions[0];
        NSString *actionType = [[action allKeys] objectAtIndex:0];
        XCTAssert([actionType isEqualToString:Q_STR], @"Expected Q_STR action");
        NSString *output = [action objectForKey:actionType];
        XCTAssert([output isEqualToString:expectedOutput], @"Output incorrect");
        [engine clearContext];
    }
}
 */

- (void)testprocessEvent_eventsForOpenCurlyBraceWithCipherMusicKmx_ReturnsCharacterActionForStartSlide {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForCipherMusicTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    UTF32Char expectedUtf32Char = 0x1D177;
    NSString * expectedStartSlideSurrogatePair = [[NSString alloc] initWithBytes:&expectedUtf32Char length:4 encoding:NSUTF32LittleEndianStringEncoding];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:NSEventModifierFlagShift timestamp:0 windowNumber:0 context:nil characters:@"{" charactersIgnoringModifiers:@"[" isARepeat:NO keyCode:kVK_ANSI_LeftBracket];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 1, @"Expected 1 action");
    CoreAction *action = actions[0];
    XCTAssert([action isCharacter], @"Expected CharacterAction");
    XCTAssert([action.content isEqualToString:expectedStartSlideSurrogatePair], @"Output incorrect");
}

/*
// TODO: fails with core, investigate
- (void)testprocessEvent_eventForUnshiftedNumeralWithCipherMusicKmx_ReturnsCharacterActionToInsertNumeral {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForCipherMusicTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"" verboseLogging:YES];
    for (int i = 1; i <= 9; i++)
    {
        unsigned short ansiCode = kVK_ANSI_1 + i - 1;
        NSString * numeral = [NSString stringWithFormat:@"%d", i];
        NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:numeral charactersIgnoringModifiers:numeral isARepeat:NO keyCode:ansiCode];
        NSArray *actions = [engine processEvent:event];
        NSLog(@"testprocessEvent, actions[0]: %@, expected numeral: %@", actions[0], numeral);
        XCTAssert(actions.count == 1, @"Expected 1 action");
        CoreAction *action = actions[0];
        XCTAssert([action isCharacter], @"Expected CharacterAction");
        XCTAssert([action.content isEqualToString:numeral], @"Output incorrect");
        [engine clearContext];
    }
}
*/

// TODO: fails with core, returns CharacterAction
- (void)testprocessEvent_eventForShiftNumeralsWithoutRulesInCipherMusicKmx_ReturnsNoAction {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForCipherMusicTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    for (int i = 0; i <= 9; i++)
    {
        if (i == 6)
            continue; // There is a rule in the keyboard for '^' (which is a SHIFT+6).
        unsigned short ansiCode = kVK_ANSI_1 + i - 1; // This works for 1-4
        NSString * unmodifiedChars = [NSString stringWithFormat:@"%d", i];
        NSString * chars;
        switch (i) {
            case 0:
                chars = @")";
                ansiCode = kVK_ANSI_0;
                break;
            case 1: chars = @"!"; break;
            case 2: chars = @"@"; break;
            case 3: chars = @"#"; break;
            case 4: chars = @"$"; break;
            case 5:
                chars = @"%";
                ansiCode = kVK_ANSI_5;
                break;
            case 7:
                chars = @"&";
                ansiCode = kVK_ANSI_7;
                break;
            case 8:
                chars = @"*";
                ansiCode = kVK_ANSI_8;
                break;
            case 9:
                chars = @"(%)";
                ansiCode = kVK_ANSI_9;
                break;
        }
        NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:NSEventModifierFlagShift timestamp:0 windowNumber:0 context:nil characters:chars charactersIgnoringModifiers:unmodifiedChars isARepeat:NO keyCode:ansiCode];
        NSArray *actions = [engine processEvent:event];
        XCTAssert(actions.count == 0, @"Expected no matching action in keyboard");
    }
}

// TODO: fails with core, returns CharacterAction
- (void)testprocessEvent_eventForPeriodWithCipherMusicKmx_ReturnsNoAction {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForCipherMusicTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"." charactersIgnoringModifiers:@"." isARepeat:NO keyCode:kVK_ANSI_Period];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 0, @"Expected no actions");
}

// TODO: fails with core, returns CharacterAction
- (void)testprocessEvent_eventForCtrl8WithCipherMusicKmx_ReturnsNoAction {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForCipherMusicTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:NSEventModifierFlagControl timestamp:0 windowNumber:0 context:nil characters:@"8" charactersIgnoringModifiers:@"8" isARepeat:NO keyCode:kVK_ANSI_8];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 0, @"Expected no actions");
}

+ (void)fillInNamesAndModifiersForAllChiralCombinations {
    int i = 0;
    modifiers[i] = LEFT_CTRL_FLAG;
    names[i++] = @"LEFT CTRL";
    modifiers[i] = LEFT_SHIFT_FLAG;
    names[i++] = @"LEFT SHIFT";
    modifiers[i] = RIGHT_SHIFT_FLAG;
    names[i++] = @"RIGHT SHIFT";
    modifiers[i] = RIGHT_CTRL_FLAG;
    names[i++] = @"RIGHT CTRL";
    modifiers[i] = LEFT_ALT_FLAG;
    names[i++] = @"LEFT ALT";
    modifiers[i] = RIGHT_ALT_FLAG;
    names[i++] = @"RIGHT ALT";
    modifiers[i] = LEFT_CTRL_FLAG | LEFT_ALT_FLAG;
    names[i++] = @"LEFT CTRL+ALT";
    modifiers[i] = RIGHT_CTRL_FLAG | RIGHT_ALT_FLAG;
    names[i++] = @"RIGHT CTRL+ALT";
    modifiers[i] = LEFT_CTRL_FLAG | LEFT_SHIFT_FLAG;
    names[i++] = @"LEFT CTRL+SHIFT";
    modifiers[i] = RIGHT_CTRL_FLAG | RIGHT_SHIFT_FLAG;
    names[i++] = @"RIGHT CTRL+SHIFT";
    modifiers[i] = LEFT_ALT_FLAG | LEFT_SHIFT_FLAG;
    names[i++] = @"LEFT ALT+SHIFT";
    modifiers[i] = RIGHT_ALT_FLAG | RIGHT_SHIFT_FLAG;
    names[i++] = @"RIGHT ALT+SHIFT";
    modifiers[i] = LEFT_CTRL_FLAG | LEFT_ALT_FLAG | LEFT_SHIFT_FLAG;
    names[i++] = @"LEFT CTRL+ALT+SHIFT";
    modifiers[i] = RIGHT_CTRL_FLAG | RIGHT_ALT_FLAG | RIGHT_SHIFT_FLAG;
    names[i++] = @"RIGHT CTRL+ALT+SHIFT";
}

- (void)testprocessEvent_eventForAWithModifiers_ReturnsCharacterActionWithExpectedCharacterBasedOnKmx {
    int i = 0;
    [KMEngineTests fillInNamesAndModifiersForAllChiralCombinations];

    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];

    for (i = 0; i < nCombinations; i++) {
        [engine clearCoreContext];
        NSString *charactersIgnoringModifiers = (modifiers[i] & (LEFT_SHIFT_FLAG | RIGHT_SHIFT_FLAG)) ? @"A" : @"a";
        NSString * characters = charactersIgnoringModifiers;
        if (modifiers[i] & (LEFT_ALT_FLAG | RIGHT_ALT_FLAG))
            characters = [characters stringByAppendingString:@"\u030A"];
        
        NSLog(@"Test case: %lu", (NSUInteger)modifiers[i]);
        // NOTE: 'a' happens to be keyCode 0 (see initVirtualKeyMapping in CoreHelper)
        NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:modifiers[i] timestamp:0 windowNumber:0 context:nil characters:characters charactersIgnoringModifiers:charactersIgnoringModifiers isARepeat:NO keyCode:0];
        NSString * keyCombination = [names[i] stringByAppendingFormat:@" %@", charactersIgnoringModifiers];
        NSArray *actions = [engine processEvent:event];
        XCTAssert(actions.count == 1, @"Expected 1 action for %@", keyCombination);
        CoreAction *action = actions[0];
        XCTAssert([action isCharacter], @"Expected CharacterAction %@", keyCombination);
        NSString *output = [action content];
        NSLog(@"output = %@", output);
        switch (modifiers[i]) {
            case LEFT_SHIFT_FLAG:
            case RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00AE"], @"Expected Registered Sign (U+00AE) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG:
                XCTAssert([output isEqualToString:@"\u00A3"], @"Expected Pound Sign (U+00A3) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG:
                XCTAssert([output isEqualToString:@"\u00A4"], @"Expected Currency Sign (U+00A4) for %@", keyCombination);
                break;
            case LEFT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00A1"], @"Expected Inverted Exclamation Mark (U+00A1) for %@", keyCombination);
                break;
            case RIGHT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00A2"], @"Expected Cent Sign (U+00A2) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG | LEFT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00A6"], @"Expected Broken Bar (U+00A6) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG | RIGHT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00A5"], @"Expected Yen Sign (U+00A5) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG | LEFT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00DB"], @"Expected Capital U with Circumflex (U+00DB) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG | RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00DF"], @"Expected Small Letter Sharp S (U+00DF) for %@", keyCombination);
                break;
            case LEFT_ALT_FLAG | LEFT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00CB"], @"Expected Capital E with Diaresis (U+00CB) for %@", keyCombination);
                break;
            case RIGHT_ALT_FLAG | RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00BD"], @"Expected Vulgar Fraction One Half (U+00BD) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG | LEFT_ALT_FLAG | LEFT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00D8"], @"Expected Capital O with Stroke (U+00D8) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG | RIGHT_ALT_FLAG | RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00B5"], @"Expected Micro Sign (U+00B5) for %@", keyCombination);
                break;
        }
    }
}

- (void)testprocessEvent_eventForSWithModifiers_ReturnsCharacterActionWithExpectedCharacterBasedOnKmx {
    int i = 0;
    [KMEngineTests fillInNamesAndModifiersForAllChiralCombinations];
    
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    
    for (i = 0; i < nCombinations; i++) {
        [engine clearCoreContext];
        NSString *charactersIgnoringModifiers = (modifiers[i] & (LEFT_SHIFT_FLAG | RIGHT_SHIFT_FLAG)) ? @"S" : @"s";
        NSString * characters = charactersIgnoringModifiers;
        if (modifiers[i] & (LEFT_ALT_FLAG | RIGHT_ALT_FLAG)) {
            if (modifiers[i] & (LEFT_SHIFT_FLAG | RIGHT_SHIFT_FLAG))
                characters = @"Í";
            else
                characters = @"ß";
        }
        
        NSLog(@"Test case: %lu", (NSUInteger)modifiers[i]);
        // NOTE: 's' happens to be keyCode 1 (see initVirtualKeyMapping in CoreHelper)
        NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:modifiers[i] timestamp:0 windowNumber:0 context:nil characters:characters charactersIgnoringModifiers:charactersIgnoringModifiers isARepeat:NO keyCode:1];
        NSString * keyCombination = [names[i] stringByAppendingFormat:@" %@", charactersIgnoringModifiers];
        NSArray *actions = [engine processEvent:event];
        XCTAssert(actions.count == 1, @"Expected 1 action for %@", keyCombination);
        CoreAction *action = actions[0];
        XCTAssert([action isCharacter], @"Expected CharacterAction for %@", keyCombination);
        NSString *output = [action content];
        NSLog(@"output = %@", output);
        switch (modifiers[i]) {
            case LEFT_SHIFT_FLAG:
            case RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00E6"], @"Expected Small Letter ae (U+00E6) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG:
                XCTAssert([output isEqualToString:@"\u00AA"], @"Expected Feminine Ordinal Indicator (U+00AA) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG:
                XCTAssert([output isEqualToString:@"\u00AB"], @"Expected Left-Pointing Double Angle Quotation Mark (U+00AB) for %@", keyCombination);
                break;
            case LEFT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00A8"], @"Expected Diaresis (U+00A8) for %@", keyCombination);
                break;
            case RIGHT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00A9"], @"Expected Copyright Sign (U+00A9) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG | LEFT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00AD"], @"Expected Soft Hyphen (U+00AD) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG | RIGHT_ALT_FLAG:
                XCTAssert([output isEqualToString:@"\u00AC"], @"Expected Not Sign (U+00AC) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG | LEFT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00E1"], @"Expected Small A with Acute (U+00E1) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG | RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00E0"], @"Expected Small A with Grave (U+00E0) for %@", keyCombination);
                break;
            case LEFT_ALT_FLAG | LEFT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00E5"], @"Expected Small A with Ring Above (U+00E5) for %@", keyCombination);
                break;
            case RIGHT_ALT_FLAG | RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00E4"], @"Expected Small A with Diaresis (U+00E4) for %@", keyCombination);
                break;
            case LEFT_CTRL_FLAG | LEFT_ALT_FLAG | LEFT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00E2"], @"Expected Small A with Circumflex (U+00E2) for %@", keyCombination);
                break;
            case RIGHT_CTRL_FLAG | RIGHT_ALT_FLAG | RIGHT_SHIFT_FLAG:
                XCTAssert([output isEqualToString:@"\u00E3"], @"Expected Small A with Tilde (U+00E3) for %@", keyCombination);
                break;
        }
    }
}

// The following checkPlatform tests all use the PlatformTest KMX file based on PlatformTest.kmn
// (not included in the project itself, but available in the source repo for reference. This
// keyboard has been craefully crafted to check for all the "wrong" values first, for which checkPlatform
// should return NO, so while these tests are ostensibly testing for YES for the platform components that
// should match on a Mac, they are also testing for NO for all the other non-matching values.

- (NSString *)checkPlatform_getOutputForKeystroke: (NSString*) character modifierFlags: (NSEventModifierFlags) flag keyCode:(unsigned short)code {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForPlatformTest];
  KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSString *lcChar = [character lowercaseString];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:flag timestamp:0 windowNumber:0 context:nil characters:character charactersIgnoringModifiers:lcChar isARepeat:NO keyCode:code];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 1, @"Expected 1 action");
    CoreAction *action = actions[0];
    XCTAssert([action isCharacter], @"Expected CharacterAction");
    return [action content];
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_native_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"u" modifierFlags:0 keyCode:kVK_ANSI_U];
    XCTAssert([output isEqualToString:@" Native"], @"Expected checkPlatform to return YES for native.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_NATIVE_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"U" modifierFlags:NSEventModifierFlagShift keyCode:kVK_ANSI_U];
    XCTAssert([output isEqualToString:@" Native"], @"Expected checkPlatform to return YES for NATIVE.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_hardware_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"p" modifierFlags:0 keyCode:kVK_ANSI_P];
    XCTAssert([output isEqualToString:@" Hardware"], @"Expected checkPlatform to return YES for hardware.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_HARDWARE_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"P" modifierFlags:NSEventModifierFlagShift keyCode:kVK_ANSI_P];
    XCTAssert([output isEqualToString:@" Hardware"], @"Expected checkPlatform to return YES for HARDWARE.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_desktop_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"I" modifierFlags:0 keyCode:kVK_ANSI_I];
    XCTAssert([output isEqualToString:@" Desktop"], @"Expected checkPlatform to return YES for desktop.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_Desktop_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"I" modifierFlags:NSEventModifierFlagShift keyCode:kVK_ANSI_I];
    XCTAssert([output isEqualToString:@" Desktop"], @"Expected checkPlatform to return YES for Desktop.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_macosx_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"o" modifierFlags:0 keyCode:kVK_ANSI_O];
    XCTAssert([output isEqualToString:@" macOS"], @"Expected checkPlatform to return YES for macosx.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_MacOS_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"K" modifierFlags:NSEventModifierFlagShift keyCode:kVK_ANSI_K];
    XCTAssert([output isEqualToString:@" macOS"], @"Expected checkPlatform to return YES for MacOS.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_MAC_ReturnsYes {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"L" modifierFlags:NSEventModifierFlagShift keyCode:kVK_ANSI_L];
    XCTAssert([output isEqualToString:@" mac"], @"Expected checkPlatform to return YES for MAC.");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_Browsers_ReturnsNo {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"y" modifierFlags:0 keyCode:kVK_ANSI_Y];
    XCTAssert([output isEqualToString:@" [Browser Undefined]"], @"Expected checkPlatform to return NO for all browsers (ie, chrome, firefox, safari, opera).");
}

// TODO: rewrite for combined character actions
- (void)testCheckPlatform_multipleTokens_MatchesCorrectOneForMac {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"m" modifierFlags:0 keyCode:kVK_ANSI_M];
    XCTAssert([output isEqualToString:@" macOS native desktop hardware"], @"Expected checkPlatform to return YES for macOS native desktop hardware.");
}

// TODO: rewrite for combined character actions
- (void)testContextMatch_InvertedPlatformLogic_NotTouch {
    NSString *output = [self checkPlatform_getOutputForKeystroke:@"x" modifierFlags:0 keyCode:kVK_ANSI_X];
    XCTAssert([output isEqualToString:@" !Touch"], @"Expected !touch to be true.");
}

// This allows for easy debugging of the "manual" platformtest keyboard developed for filling in the test spreadsheet.
//- (void)testCheckPlatform_temp_UnknownPlatformX {
//    KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:@"/Users/tom/Documents/keymanapp/keyman/windows/src/test/manual-tests/platform-rules/platformtest.kmx"];
//
//    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
//    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"a" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:kVK_ANSI_A];
//    NSArray *actions = [engine processEvent:event];
//    XCTAssert(actions.count == 1, @"Expected 1 action");
//    NSDictionary *action = actions[0];
//    NSString *actionType = [[action allKeys] objectAtIndex:0];
//    XCTAssert([actionType isEqualToString:Q_STR], @"Expected Q_STR action");
//    NSString *output = [action objectForKey:actionType];
//    XCTAssert([output isEqualToString:@"hardware macosx desktop native undefined !touch undefined macosx"], @"Expected !touch.");
//}


- (void)testEngine_ipaKeyboardAction_DoesNotCrash_Issue1892 {
//  KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForSilIpaTests];
//  KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"a" verboseLogging:YES];
//  NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"=" charactersIgnoringModifiers:@"=" isARepeat:NO keyCode:kVK_ANSI_Equal];
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForIndexOffsetTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"z" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"a" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:kVK_ANSI_A];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 2, @"Expected 2 actions");
    CoreAction *action = actions[0];
    XCTAssert([action isCharacterBackspace], @"Expected CharacterAction");

    action = actions[1];
    XCTAssert([action isCharacter], @"Expected CharacterAction");
    XCTAssert([action.content isEqualToString:@"Z"], @"Expected output to be 'Z'.");
}

/*
- (void)testLegacyProcessEvent_eventForFWithElNuerKmx_ReturnsCorrectCharacter {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"f" charactersIgnoringModifiers:@"f" isARepeat:NO keyCode:kVK_ANSI_F];
    NSArray *actions = [engine experimentallyProcessEventForUnitTestingOnly:event usingCore:NO];
    XCTAssert(actions.count == 1, @"Expected one action");
    NSDictionary *action = actions[0];
    NSString *actionType = [[action allKeys] objectAtIndex:0];
    XCTAssert([actionType isEqualToString:Q_STR], @"Expected Q_STR action");
    NSString *output = [action objectForKey:actionType];
    XCTAssert([output isEqualToString:@"ɣ"], @"Expected output to be 'ɣ'.");
}
*/

- (void)testCoreProcessEvent_eventForFWithElNuerKmx_ReturnsCorrectCharacter {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"f" charactersIgnoringModifiers:@"f" isARepeat:NO keyCode:kVK_ANSI_F];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 1, @"Expected one action");
    CoreAction *action = actions[0];
    XCTAssert([action isCharacter], @"Expected CharacterAction");
    XCTAssert([action.content isEqualToString:@"ɣ"], @"Expected output to be 'ɣ'.");
}

- (void)testCoreProcessEvent_eventDeleteWithElNuerKmx_EmptiesContextReturnsDelete {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"ɣ" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"\b" charactersIgnoringModifiers:@"\b" isARepeat:NO keyCode:kVK_Delete];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 1, @"Expected one action");
    CoreAction *action = actions[0];
    XCTAssert([action isCharacterBackspace], @"Expected CharacterBackspace action");
    NSString *context = engine.getCoreContext;
    XCTAssert([context isEqualToString:@""], @"Context should be empty.");
}

/*
- (void)testLegacyProcessEvent_eventDeleteWithElNuerKmx_ReturnsEmptyActionListContextUnchanged {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"ɣ" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"\b" charactersIgnoringModifiers:@"\b" isARepeat:NO keyCode:kVK_Delete];
    NSArray *actions = [engine experimentallyProcessEventForUnitTestingOnly:event usingCore:NO];
    XCTAssert(actions.count == 0, @"Expected no actions");
    NSString *context = engine.contextBuffer;
    XCTAssert([context isEqualToString:@"ɣ"], @"Context should be unchanged.");
}
*/


- (void)testCoreProcessEvent_eventReturnWithElNuerKmx_ContextUnchangedReturnsReturn {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"ɣ" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"\n" charactersIgnoringModifiers:@"\n" isARepeat:NO keyCode:kVK_Return];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 2, @"Expected one action");
    CoreAction *action = actions[1];
    XCTAssert(action.actionType == EmitKeystrokeAction, @"Expected EmitKeystrokeAction");
    NSString *context = engine.getCoreContext;
    XCTAssert([context isEqualToString:@"ɣ"], @"Context should be unchanged.");
}

/*
- (void)testLegacyProcessEvent_eventReturnWithElNuerKmx_ReturnsEmptyActionListContextUnchanged {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"ɣ" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"\n" charactersIgnoringModifiers:@"\n" isARepeat:NO keyCode:kVK_Return];
    NSArray *actions = [engine experimentallyProcessEventForUnitTestingOnly:event usingCore:NO];
    XCTAssert(actions.count == 0, @"Expected no actions");
    NSString *context = engine.contextBuffer;
    XCTAssert([context isEqualToString:@"ɣ"], @"Context should be unchanged.");
}
*/

- (void)testCoreProcessEvent_eventTabWithElNuerKmx_ContextUnchangedReturnsTab {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:@"ɣ" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"\t" charactersIgnoringModifiers:@"\t" isARepeat:NO keyCode:kVK_Tab];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 2, @"Expected two actions");
    CoreAction *action = actions[1];
    XCTAssert(action.actionType == EmitKeystrokeAction, @"Expected EmitKeystrokeAction");
    NSString *context = engine.getCoreContext;
    XCTAssert([context isEqualToString:@"ɣ"], @"Context should be unchanged.");
}

/*
- (void)testLegacyProcessEvent_eventTabWithElNuerKmx_ReturnsEmptyActionListContextUnchanged {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"ɣ" verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"\t" charactersIgnoringModifiers:@"\n" isARepeat:NO keyCode:kVK_Tab];
    NSArray *actions = [engine experimentallyProcessEventForUnitTestingOnly:event usingCore:NO];
    XCTAssert(actions.count == 0, @"Expected no actions");
    NSString *context = engine.contextBuffer;
    XCTAssert([context isEqualToString:@"ɣ"], @"Context should be unchanged.");
}
*/

- (void)testCoreProcessEvent_eventSingleQuoteWithElNuerKmx_ReturnsDiacritic {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    NSString *context = @"ɛ";
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile context:context verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"'" charactersIgnoringModifiers:@"'" isARepeat:NO keyCode:kVK_ANSI_Quote];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 1, @"Expected one action");
    CoreAction *action = actions[0];
    XCTAssert([action isCharacter], @"Expected CharacterAction");
    context = engine.getCoreContext;
    XCTAssert([context isEqualToString:@"\u025B\u0308"], @"Context updated with diacritic.");
}

/*
- (void)testLegacyProcessEvent_eventSingleQuoteWithElNuerKmx_ReturnsTwoActions {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileForElNuerTests];
    NSString *context = @"ɛ";
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:context verboseLogging:YES];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"'" charactersIgnoringModifiers:@"'" isARepeat:NO keyCode:kVK_ANSI_Quote];
    NSArray *actions = [engine experimentallyProcessEventForUnitTestingOnly:event usingCore:NO];
    XCTAssert(actions.count == 2, @"Expected two actions");
    NSDictionary *action = actions[0];
    NSString *actionType = [[action allKeys] objectAtIndex:0];
    XCTAssert([actionType isEqualToString:Q_BACK], @"Expected Q_BACK action");
    action = actions[1];
    actionType = [[action allKeys] objectAtIndex:0];
    NSString *characters = [action objectForKey:actionType];
    XCTAssert([characters isEqualToString:@"\u025B\u0308"], @"Expected \u025B\u0308");
    context = engine.contextBuffer;
    XCTAssert([context isEqualToString:@"\u025B\u0308"], @"Context updated with diacritic.");
}
*/

@end
