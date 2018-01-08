//
//  KMInputMethodBrowserClientEventHandler.m
//  Keyman
//
//  Created by tom on 1/5/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import "KMInputMethodBrowserClientEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"

@implementation KMInputMethodBrowserClientEventHandler

// Because Google Docs can't report its context in any of the browsers (Safari, Chrome, Firefox), we want to
// try to detect it and:
// in Safari, switch to legacy mode// in Chrome, NOT assume that it needs to re-get the context every time around (which means that if the user
// does mouse-click somewhere else, it could lead to bad behaviour).
// in Firefox, we're already in legacy mode and we do get mouse clicks, so we're already doing the best we can.
NSUInteger _failuresToRetrieveExpectedContext;
BOOL _forceRemoveSelectionInGoogleDocs;
//BOOL _explicitlyDeleteExistingSelectionBeforeInserting = NO;
BOOL _googleChrome;
BOOL _insertCharactersIndividually;

- (instancetype)initWithClient:(NSString *)clientAppId {
    
    self = [super initWithClient:clientAppId];
    if (self) {
        BOOL safari = [clientAppId isEqual: @"com.apple.Safari"];
        _googleChrome = [clientAppId isEqual: @"com.google.Chrome"];
        _failuresToRetrieveExpectedContext = 0;
        _forceRemoveSelectionInGoogleDocs = safari;
        self.insertCharactersIndividually = NO;
        //_explicitlyDeleteExistingSelectionBeforeInserting = safari;
        if (safari)
            self.clientSelectionCanChangeUnexpectedly = NO;
    }
    return self;
}

- (void)checkContextIn:(id) client {
    if (!self.willDeleteNullChar && !self.contextOutOfDate && _failuresToRetrieveExpectedContext < 3) {
        if (self.AppDelegate.debugMode) {
            NSLog(@"Checking to see if we're in Google Docs (or some other site that can't give context).");
        }
        NSUInteger bufferLength = self.contextBuffer.length;
        if (bufferLength) {
            NSUInteger location = [client selectedRange].location;
            
            if (location != NSNotFound && location > 0) {
                NSString *clientContext = [[client attributedSubstringFromRange:NSMakeRange(0, location)] string];
                if (clientContext == nil || !clientContext.length ||
                    [clientContext characterAtIndex:clientContext.length - 1] !=
                    [self.contextBuffer characterAtIndex:bufferLength - 1]) {
                    _failuresToRetrieveExpectedContext++;
                }
                else {
                    if ([self AppDelegate].debugMode) {
                        NSLog(@"We got what we were expecting from the client. We can stop checking.");
                    }
                    _failuresToRetrieveExpectedContext = NSUIntegerMax;
                }
            }
            else {
                _failuresToRetrieveExpectedContext++;
            }
            
            if (_failuresToRetrieveExpectedContext == 3)
            {
                if ([self AppDelegate].debugMode) {
                    NSLog(@"Detected Google Docs or some other editor that can't provide context. Using legacy mode.");
                }
                _failuresToRetrieveExpectedContext = NSUIntegerMax;
                self.legacyMode = YES;
                if (self.clientSelectionCanChangeUnexpectedly) {
                    self.cannnotTrustSelectionLength = YES;
                    self.clientSelectionCanChangeUnexpectedly = NO; // This isn't true (it can change unexpectedly), but we can't get the context, so we pretend/hope it won't.
                    // Google docs in Chrome allows only a single character at a time :-(
                    _insertCharactersIndividually = _googleChrome;
                }
            }
        }
    }
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
    
    // In Google Docs in Safari when there is an existing selection, the inserted characters
    // stay selected. The following clears the selection.
    if (self.legacyMode && _forceRemoveSelectionInGoogleDocs) {
        if ([self.AppDelegate debugMode])
            NSLog(@"Sending Command-Shift-A to clear selection in Google Docs");
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

- (void)insertPendingBufferTextIn:(id)client {
    NSUInteger length = 0;
    
    if (!_insertCharactersIndividually) {
        length = [self pendingBuffer].length;
        if (length > 1) {
            [super insertPendingBufferTextIn:client];
            return;
        }
    }
    if ([self.AppDelegate debugMode]) {
        NSLog(@"Using special Google Docs in Chrome logic");
    }
    NSString* remainingText = [self.pendingBuffer substringFromIndex:1];
    [self.pendingBuffer deleteLastNChars:length - 1];
    
    [super insertPendingBufferTextIn:client];
        
    // Reset the pending buffer to contain remaining characters and issue call to come back for more...
    [self setPendingBuffer:remainingText];
    [self performSelector:@selector(initiatePendingBufferProcessing:) withObject:client afterDelay:0.1];
}
@end
