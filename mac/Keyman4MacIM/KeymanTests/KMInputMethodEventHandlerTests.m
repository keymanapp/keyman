//
//  KMInputMethodEventHandlerTests.m
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "TestAppDelegate.h"
#import "LegacyTestClient.h"
#import "NoContextTestClient.h"
#import "AppleCompliantTestClient.h"
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

// ***** Legacy: Client does not implement insertText or attributedSubstringFromRange *****

-(void)testHandleEvent_Actions1QBack2Nul_Legacy_precedingCharacterInEngineContext_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:YES clientSelectionCanChangeUnexpectedly:YES];
    id client = [[LegacyTestClient alloc] init];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a" keyCode:kVK_ANSI_A]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    [self helperVerifyAndProcessPendingDelete:client inputMethodEventHandler:im];
    assert([client length] == 1);
    assert([[client getResult] isEqualToString:@"a"]);
}

-(void)testHandleEvent_Actions1QBack2Nul_NotLegacy_precedingAndFollowingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:YES clientSelectionCanChangeUnexpectedly:YES];
    id client = [[LegacyTestClient alloc] init];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a" keyCode:kVK_ANSI_A]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    [self helperVerifyAndProcessPendingDelete:client inputMethodEventHandler:im];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b" keyCode:kVK_ANSI_B]];
    assert([client length] == 2);
    assert([[client getResult] isEqualToString:@"ab"]);
}

- (void)testHandleEvent_Actions1QBack2Nul_Legacy_followingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:YES clientSelectionCanChangeUnexpectedly:YES];
    id client = [[LegacyTestClient alloc] init];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    [self helperVerifyAndProcessPendingDelete:client inputMethodEventHandler:im];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b" keyCode:kVK_ANSI_B]];
    assert([client length] == 1);
    assert([[client getResult] isEqualToString:@"b"]);
}

// ***** NoContext: Client implements insertText; implementation of attributedSubstringFromRange returns nil *****
-(void)testHandleEvent_Actions1QBack2Nul_NoContext_precedingCharacterInEngineContext_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[NoContextTestClient alloc] init];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a" keyCode:kVK_ANSI_A]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    assert([client length] == 1);
    assert([[client getResult] isEqualToString:@"a"]);
}

-(void)testHandleEvent_Actions1QBack2Nul_NoContext_precedingAndFollowingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[NoContextTestClient alloc] init];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a" keyCode:kVK_ANSI_A]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b" keyCode:kVK_ANSI_B]];
    assert([client length] == 2);
    assert([[client getResult] isEqualToString:@"ab"]);
}

- (void)testHandleEvent_Actions1QBack2Nul_NoContext_followingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[NoContextTestClient alloc] init];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b" keyCode:kVK_ANSI_B]];
    [self helperVerifyAndProcessPendingDelete:client inputMethodEventHandler:im];
    assert(_delegate.virtualKeyPosted == kVK_ANSI_B); // In real life, this key would now be posted and would come through as the next event.
    assert([client length] == 0);
    assert([[client getResult] isEqualToString:@""]);
}

// ***** Apple-compliant: Client implements insertText and attributedSubstringFromRange correctly *****
-(void)testHandleEvent_Actions1QBack2Nul_AppleCompliant_precedingCharacterInEngineContext_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[AppleCompliantTestClient alloc] init];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a" keyCode:kVK_ANSI_A]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    assert([client length] == 1);
    assert([[client getResult] isEqualToString:@"a"]);
}

-(void)testHandleEvent_Actions1QBack2Nul_AppleCompliant_precedingAndFollowingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[AppleCompliantTestClient alloc] init];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"a" keyCode:kVK_ANSI_A]];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b" keyCode:kVK_ANSI_B]];
    assert([client length] == 2);
    assert([[client getResult] isEqualToString:@"ab"]);
}

- (void)testHandleEvent_Actions1QBack2Nul_AppleCompliant_followingCharacter_ExistingContextCharacterDeleted {
    KMInputMethodEventHandler * im =  [[KMInputMethodEventHandler alloc] initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    id client = [[AppleCompliantTestClient alloc] init];
    NSEvent *event = [_delegate keyStrokeEventForCharacter: @"q" keyCode:kVK_ANSI_Q];
    [client handleEventIfNotHandledBy:im event:event];
    // Keyboard rule "q" + "q" > nul
    [client handleEventIfNotHandledBy:im event:event];
    [client handleEventIfNotHandledBy:im event:[_delegate keyStrokeEventForCharacter: @"b" keyCode:kVK_ANSI_B]];
    [self helperVerifyAndProcessPendingDelete:client inputMethodEventHandler:im];
    assert(_delegate.virtualKeyPosted == kVK_ANSI_B); // In real life, this key would now be posted and would come through as the next event.
    assert([client length] == 0);
    assert([[client getResult] isEqualToString:@""]);
}

// ***** Helper methods *****

-(void)helperVerifyAndProcessPendingDelete:(LegacyTestClient *)client inputMethodEventHandler:(KMInputMethodEventHandler *)im {
    assert(_delegate.virtualKeyPosted == kVK_Delete);
    NSEvent *deleteEvent = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSZeroPoint modifierFlags:0 timestamp:NSTimeIntervalSince1970 windowNumber:0 context:nil characters:@"^h" charactersIgnoringModifiers:@"^h" isARepeat:NO keyCode:kVK_Delete];
    [client handleEventIfNotHandledBy:im event:deleteEvent];
}
@end
