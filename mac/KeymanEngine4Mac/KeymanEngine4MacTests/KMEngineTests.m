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
    KMEngine *engine = [[KMEngine alloc] initWithKMX:nil contextBuffer:@""];
    NSEvent *event = [[NSEvent alloc] init];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected processEvent to return nil for nil kmx");
}

- (void)testinitWithKMX_ValidKmxEmptyContext_InitializedWithEmptyContext {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
    XCTAssert(engine != nil, @"Expected non-nil engine");
    XCTAssert(engine.contextBuffer.length == 0, @"Expected empty context buffer");
}

- (void)testinitWithKMX_ValidKmxNonEmptyContext_InitializedWithContext {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@"abc"];
    XCTAssert(engine != nil, @"Expected non-nil engine");
    XCTAssert([engine.contextBuffer isEqualToString:@"abc"], @"Expected 'abc' in context buffer");
}

- (void)testsetContextBuffer_NonEmptyContext_InitializedWithContext {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
    [engine setContextBuffer:@"xyz"];
    XCTAssert([engine.contextBuffer isEqualToString:@"xyz"], @"Expected 'xyz' in context buffer");
}

- (void)testprocessEvent_eventForCommandKey_ReturnsNilActionsArray {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:NSEventModifierFlagCommand timestamp:0 windowNumber:0 context:nil characters:@"a" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:kVK_ANSI_A];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected nil array of actions");
}

- (void)testprocessEvent_eventWithoutKeycode_ReturnsNilActionsArray {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
    NSEvent *event = [NSEvent mouseEventWithType:NSEventTypeMouseMoved location:NSMakePoint(29, 21) modifierFlags:NSEventModifierFlagShift timestamp:0 windowNumber:0 context:nil eventNumber:23 clickCount:0 pressure:0];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected nil array of actions");
}

- (void)testprocessEvent_eventForUnmappedKey_ReturnsNoActions {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"z" charactersIgnoringModifiers:@"z" isARepeat:NO keyCode:kVK_ANSI_Z];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions == nil, @"Expected nil array of actions");
}

- (void)testprocessEvent_eventForLowercaseA_ReturnsQstrActionWithExpectedCharacterBasedOnKmx {
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil characters:@"a" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:kVK_ANSI_A];
    NSArray *actions = [engine processEvent:event];
    XCTAssert(actions.count == 1, @"Expected 1 action");
    NSDictionary *action = actions[0];
    NSString *actionType = [[action allKeys] objectAtIndex:0];
    XCTAssert([actionType isEqualToString:Q_STR], @"Expected Q_STR action");
    NSString *output = [action objectForKey:actionType];
    XCTAssert([output isEqualToString:@"\u00C7"], @"Expected capital C cedille (U+00C7)");
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

- (void)testprocessEvent_eventForAWithModifiers_ReturnsQstrActionWithExpectedCharacterBasedOnKmx {
    int i = 0;
    [KMEngineTests fillInNamesAndModifiersForAllChiralCombinations];

    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];

    for (i = 0; i < nCombinations; i++) {
        [engine setContextBuffer:@""];
        NSString *charactersIgnoringModifiers = (modifiers[i] & (LEFT_SHIFT_FLAG | RIGHT_SHIFT_FLAG)) ? @"A" : @"a";
        NSString * characters = charactersIgnoringModifiers;
        if (modifiers[i] & (LEFT_ALT_FLAG | RIGHT_ALT_FLAG))
            characters = [characters stringByAppendingString:@"\u030A"];
        
        NSLog(@"Test case: %lu", (NSUInteger)modifiers[i]);
        // NOTE: 'a' happens to be keyCode 0 (see setVKMapping in KMEngine)
        NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:modifiers[i] timestamp:0 windowNumber:0 context:nil characters:characters charactersIgnoringModifiers:charactersIgnoringModifiers isARepeat:NO keyCode:0];
        NSString * keyCombination = [names[i] stringByAppendingFormat:@" %@", charactersIgnoringModifiers];
        NSArray *actions = [engine processEvent:event];
        XCTAssert(actions.count == 1, @"Expected 1 action for %@", keyCombination);
        NSDictionary *action = actions[0];
        NSString *actionType = [[action allKeys] objectAtIndex:0];
        XCTAssert([actionType isEqualToString:Q_STR], @"Expected Q_STR action for %@", keyCombination);
        NSString *output = [action objectForKey:actionType];
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

- (void)testprocessEvent_eventForSWithModifiers_ReturnsQstrActionWithExpectedCharacterBasedOnKmx {
    int i = 0;
    [KMEngineTests fillInNamesAndModifiersForAllChiralCombinations];
    
    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
    
    for (i = 0; i < nCombinations; i++) {
        [engine setContextBuffer:@""];
        NSString *charactersIgnoringModifiers = (modifiers[i] & (LEFT_SHIFT_FLAG | RIGHT_SHIFT_FLAG)) ? @"S" : @"s";
        NSString * characters = charactersIgnoringModifiers;
        if (modifiers[i] & (LEFT_ALT_FLAG | RIGHT_ALT_FLAG)) {
            if (modifiers[i] & (LEFT_SHIFT_FLAG | RIGHT_SHIFT_FLAG))
                characters = @"Í";
            else
                characters = @"ß";
        }
        
        NSLog(@"Test case: %lu", (NSUInteger)modifiers[i]);
        // NOTE: 's' happens to be keyCode 1 (see setVKMapping in KMEngine)
        NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:modifiers[i] timestamp:0 windowNumber:0 context:nil characters:characters charactersIgnoringModifiers:charactersIgnoringModifiers isARepeat:NO keyCode:1];
        NSString * keyCombination = [names[i] stringByAppendingFormat:@" %@", charactersIgnoringModifiers];
        NSArray *actions = [engine processEvent:event];
        XCTAssert(actions.count == 1, @"Expected 1 action for %@", keyCombination);
        NSDictionary *action = actions[0];
        NSString *actionType = [[action allKeys] objectAtIndex:0];
        XCTAssert([actionType isEqualToString:Q_STR], @"Expected Q_STR action for %@", keyCombination);
        NSString *output = [action objectForKey:actionType];
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
@end
