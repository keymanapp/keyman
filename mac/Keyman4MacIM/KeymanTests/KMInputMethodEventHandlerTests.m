//
//  KMInputMethodEventHandlerTests.m
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "TestAppDelegate.h"
#import "SimpleTestClient.h"
#import "KMInputMethodEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#import <OCMock/OCMock.h>

@interface KMInputMethodEventHandlerTests : XCTestCase

@end

@implementation KMInputMethodEventHandlerTests

- (void)setUp {
    [super setUp];
    TestAppDelegate *delegate = [[TestAppDelegate alloc] init];
    [[NSApplication sharedApplication] setDelegate:delegate];
    // Put setup code here. This method is called before the invocation of each test method in the class.
    NSString *path = [[NSBundle bundleForClass:[KMInputMethodEventHandlerTests class]] pathForResource:@"KMMethodEventHandlerTests.kmx" ofType:nil];
    KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
    [delegate setKmx:kmxFile];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    [super tearDown];
}

- (void)testProcessUnhandledDeleteBack_ruleGeneratesNul_ExistingContextCharacterDeleted {
    // Keyboard rule "q" + "q" > nul
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[SimpleTestClient alloc] init];
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSZeroPoint modifierFlags:0 timestamp:NSTimeIntervalSince1970 windowNumber:0 context:nil characters:@"q" charactersIgnoringModifiers:@"q" isARepeat:NO keyCode:0];
    [client handeEventIfNotHandledBy:im event:event];
    [client handeEventIfNotHandledBy:im event:event];
    assert(![client length]);
}

@end
