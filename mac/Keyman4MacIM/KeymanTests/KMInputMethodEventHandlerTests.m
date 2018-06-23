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
@property (nonatomic, strong) TestAppDelegate * delegate;
@end

@implementation KMInputMethodEventHandlerTests

- (void)setUp {
    [super setUp];
    _delegate = [[TestAppDelegate alloc] init];
    [[NSApplication sharedApplication] setDelegate:_delegate];
    // Put setup code here. This method is called before the invocation of each test method in the class.
    NSString *path = [[NSBundle bundleForClass:[KMInputMethodEventHandlerTests class]] pathForResource:@"KMMethodEventHandlerTests.kmx" ofType:nil];
    KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
    [_delegate setKmx:kmxFile];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    [super tearDown];
}

-(void)testHandleEvent_Actions1QBack2Nul_NotLegacy_ClientFailsToGiveContext_precedingCharacterInEngineContext_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[SimpleTestClient alloc] init];
    [client handeEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a"]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q"];
    [client handeEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handeEventIfNotHandledBy:im event:event];
    assert([client length] == 1);
    assert([[client getResult] isEqualToString:@"a"]);
}

-(void)testHandleEvent_Actions1QBack2Nul_NotLegacy_ClientFailsToGiveContext_precedingAndFollowingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[SimpleTestClient alloc] init];
    [client handeEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a"]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q"];
    [client handeEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handeEventIfNotHandledBy:im event:event];
    [client handeEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b"]];
    assert([client length] == 2);
    assert([[client getResult] isEqualToString:@"ab"]);
}

- (void)testHandleEvent_Actions1QBack2Nul_NotLegacy_ClientFailsToGiveContext_followingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[SimpleTestClient alloc] init];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q"];
    [client handeEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handeEventIfNotHandledBy:im event:event];
    [client handeEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b"]];
    assert(_delegate.virtualKeyPosted == kVK_Delete);
    NSEvent *deleteEvent = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSZeroPoint modifierFlags:0 timestamp:NSTimeIntervalSince1970 windowNumber:0 context:nil characters:@"^h" charactersIgnoringModifiers:@"^h" isARepeat:NO keyCode:kVK_Delete];
    assert([im handleEvent:deleteEvent client:client]);
    assert([client length] == 2);
    assert([[client getResult] isEqualToString:@"b"]);
}
@end
