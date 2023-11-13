//
//  KMInputMethodSafariClientEventHandler.m
//  Keyman
//
//  Created by tom on 1/9/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import "KMInputMethodSafariClientEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#import "KMInputMethodBrowserClientEventHandlerProtected.h"

@implementation KMInputMethodSafariClientEventHandler

// Because Google Docs can't report its context, if we detect that we are in Google Docs, we change to legacy mode.
//BOOL _explicitlyDeleteExistingSelectionBeforeInserting = NO;
BOOL _preserveContextForNextCmdA;

- (instancetype)init {
    self = [super initWithLegacyMode:NO clientSelectionCanChangeUnexpectedly:NO];
    if (self) {
        _preserveContextForNextCmdA = NO;
        //_explicitlyDeleteExistingSelectionBeforeInserting = YES;
    }
    return self;
}

- (void)handleCommand:(NSEvent *)event {
    // If Safari issued a Cmd-A to remove the selection, we know what the
    // context should be, so keep it.
    if ([event.characters isEqualTo:@"a"] && _preserveContextForNextCmdA)
        _preserveContextForNextCmdA = NO;
    else
        [super handleCommand:event];
}

- (void)setInSiteThatDoesNotGiveContext {
    [self switchToLegacyMode];
}

- (void)replaceExistingSelectionIn:(id)client with:(NSString *) text {
    // The following commented out code is the code that would make it possible to type over an
    // existing selection in Word in Safari, but I don't know of a way to distinguish that case
    // from other contexts in Safari (wherein this code inserts an extra leading space).
    //                    if (_explicitlyDeleteExistingSelectionBeforeInserting) {
    //                        NSUInteger selLength = [sender selectedRange].length;
    //                        if (selLength > 0 && selLength != NSNotFound) {
    //                            if ([self.AppDelegate debugMode]) {
    //                                NSLog(@"Attempting to delete existing selection of length = %lu by replacing it with a space (which should not actually appear in the text).", selLength);
    //                            }
    //
    //                            [sender insertText:@" " replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
    //
    //                            if ([self.AppDelegate debugMode])
    //                                NSLog(@"Re-posting original (unhandled) code: %d", (int)_keyCodeOfOriginalEvent);
    //
    //                            CGEventSourceRef sourceFromEvent = CGEventCreateSourceFromEvent([event CGEvent]);
    //                            [self postKeyPressToFrontProcess:event.keyCode from:sourceFromEvent];
    //                            CFRelease(sourceFromEvent);
    //                            return YES;
    //                        }
    //                    }
    [super replaceExistingSelectionIn:client with:text];
    
    // In Google Docs (i.e., "legacy mode" when there is an existing selection, the inserted characters
    // stay selected. The following clears the selection.
    if (self.legacyMode) {
        if ([self.appDelegate debugMode])
            NSLog(@"Sending Command-Shift-A to clear selection in Google Docs");
        _preserveContextForNextCmdA = YES;
        ProcessSerialNumber psn;
        GetFrontProcess(&psn);
        
        CGEventRef event = CGEventCreateKeyboardEvent(NULL, kVK_ANSI_A, true);
        //set shift and command keys down for above event
        CGEventSetFlags(event, kCGEventFlagMaskShift | kCGEventFlagMaskCommand);
        CGEventPostToPSN(&psn, event);
        CFRelease(event);
        
        event = CGEventCreateKeyboardEvent(NULL, kVK_ANSI_A, false);
        CGEventSetFlags(event, kCGEventFlagMaskShift | kCGEventFlagMaskCommand);
        CGEventPostToPSN(&psn, event);
        CFRelease(event);
    }
}
@end
