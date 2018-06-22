//
//  KMInputMethodBrowserClientEventHandlerTests.m
//  KeymanTests
//
//  Created by tom on 1/25/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "TestAppDelegate.h"
#import "KMInputMethodBrowserClientEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#import <OCMock/OCMock.h>

@interface KeymanTests : XCTestCase

@end

@implementation KeymanTests
KMInputMethodBrowserClientEventHandler * _im;

- (void)setUp {
    [super setUp];
    TestAppDelegate *delegate = [[TestAppDelegate alloc] init];
    [[NSApplication sharedApplication] setDelegate:delegate];
    // Put setup code here. This method is called before the invocation of each test method in the class.
    NSString *path = [[NSBundle bundleForClass:[KeymanTests class]] pathForResource:@"SimpleTest.kmx" ofType:nil];
    KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
    [delegate setKmx:kmxFile];
    _im = [[KMInputMethodBrowserClientEventHandler alloc] init];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    [super tearDown];
}

- (void)testcheckContextIn_EmptyContextExpected_NoChangeToclientSelectionCanChangeUnexpectedly {
    // If the expected context is empty, no attempt should be made to check for matching context from
    // client, so even though the client would also return an empty string, calling checkContextIn
    // repeatedly has no effect (i.e., it does not set clientSelectionCanChangeUnexpectedly to false).
    // Therefore a subsequent key down event *should* result in a call to the client to inquire about
    // the current selection.
    id client = OCMStrictProtocolMock(@protocol(IMKTextInput));
    OCMStub([client selectedRange]).andReturn(NSMakeRange(0, 0));
    NSAttributedString *attrSubstring = [[NSAttributedString alloc] init];
    OCMStub([client attributedSubstringFromRange:(NSMakeRange(0, 0))]).andReturn(attrSubstring);
    OCMExpect([client insertText:@"a" replacementRange:NSMakeRange(NSNotFound, NSNotFound)]);
    for (int i = 0; i < 8; i++)
    {
        [_im checkContextIn:client];
    }
    NSEvent *event = [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSZeroPoint modifierFlags:0 timestamp:NSTimeIntervalSince1970 windowNumber:0 context:nil characters:@"a" charactersIgnoringModifiers:@"a" isARepeat:NO keyCode:0];
    [_im handleEvent:event client:client];
    [client verify];
}

@end
