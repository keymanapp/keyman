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

//- (void)testprocessEvent_eventForLeftOptionA_ReturnsQstrActionWithExpectedCharacterBasedOnKmx {
//    KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
//    KMEngine *engine = [[KMEngine alloc] initWithKMX:kmxFile contextBuffer:@""];
//    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSMakePoint(0, 0) modifierFlags:NSEventModifierFlagControl timestamp:0 windowNumber:0 context:nil characters:@"å" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:kVK_ANSI_A];
//    NSArray *actions = [engine processEvent:event];
//    XCTAssert(actions.count == 1, @"Expected 1 action");
//    NSDictionary *action = actions[0];
//    NSString *actionType = [[action allKeys] objectAtIndex:0];
//    XCTAssert([actionType isEqualToString:Q_STR], @"Expected Q_STR action");
//    NSString *output = [action objectForKey:actionType];
//    XCTAssert([output isEqualToString:@"\u00A3"], @"Expected pound sign (U+00A3)");
//}
@end
