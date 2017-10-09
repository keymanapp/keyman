//
//  KMInputController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 29/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInputController.h"
#include <Carbon/Carbon.h> /* For kVK_ constants. */

@interface KMInputController ()
@property (nonatomic, strong) NSMutableDictionary *kbData;
//@property (nonatomic, strong) NSDictionary *kmModes;
@property (assign) BOOL willDeleteNullChar;
@end

@implementation KMInputController

const CGKeyCode kProcessPendingBuffer = 0xFF;

//_pendingBuffer contains text that is ready to be sent to the client when all delete-backs are finished.
NSMutableString* _pendingBuffer;
NSUInteger _numberOfPostedDeletesToExpect = 0;
CGKeyCode _keyCodeOfOriginalEvent;
CGEventSourceRef _sourceFromOriginalEvent = nil;

// These two flags indicate which mode is being used for composing characters (i.e., when not using deadkeys).
BOOL _legacyMode = NO; // Composes using the approach of posting a delete-back, followed by the required key codes
// This flag is ignored when in "legacy" mode.
//BOOL _insertTextMode = YES; // Indicates that a call to setMarkedText should be followed by a call to insertText
NSRange _previousSelRange;

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (id)initWithServer:(IMKServer *)server delegate:(id)delegate client:(id)inputClient
{
    NSRunningApplication *currApp = [[NSWorkspace sharedWorkspace] frontmostApplication];
    NSString *clientAppName = [currApp localizedName];
    if ([self.AppDelegate debugMode])
        NSLog(@"New active app %@", clientAppName);
    // TODO: Use a table of known apps to decide whether or not to operate in legacy mode
    // and whether or not to follow calls to setMarkedText with calls to insertText.
    if ([clientAppName isEqual: @"Google Chrome"] ||
        [clientAppName isEqual: @"Safari"] || // Most things in Safari work well using the normal way, but Google Docs doesn't.
        [clientAppName isEqual: @"Atom"] ||
        [clientAppName isEqual: @"LibreOffice Vanilla"]) {
        _legacyMode = YES;
        if ([self.AppDelegate debugMode])
            NSLog(@"Using legacy mode for this app.");
    }
    else
        _legacyMode = NO;

    self = [super initWithServer:server delegate:delegate client:inputClient];
    if (self) {
        self.AppDelegate.inputController = self;
        if (self.AppDelegate.kvk != nil && self.AppDelegate.alwaysShowOSK)
            [self.AppDelegate showOSK];
    }
    
    return self;
}

- (NSUInteger)recognizedEvents:(id)sender {
    return (NSKeyDownMask | NSLeftMouseDownMask);
}

- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
    if ([self.AppDelegate debugMode])
        NSLog(@"Event = %@", event);
    
    if (event == nil || sender == nil)
        return NO;
    
    // OSK key feedback from hardware keyboard is disabled
    /*if (event.type == NSKeyDown)
        [self.AppDelegate handleKeyEvent:event];*/
    
    if (self.kmx == nil)
        return NO;
    
    if (event.type == NSLeftMouseDown) {
        [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
        return NO;
    }
    
    if (_legacyMode && event.type == NSKeyDown && event.keyCode == kProcessPendingBuffer)
    {
        if ([self.AppDelegate debugMode])
            NSLog(@"Processing the special %hu code", kProcessPendingBuffer);
        
        NSString* text = [self pendingBuffer];
        
        if ([text length] > 0)
        {
            if ([self.AppDelegate debugMode])
                NSLog(@"Inserting text from pending buffer: %@", text);
            
            [sender insertText:text replacementRange:NSMakeRange(NSNotFound, NSNotFound)];

            [self setPendingBuffer:@""];
        }
        else
        {
            if ([self.AppDelegate debugMode])
                NSLog(@"Error - expected text in pending buffer!");
        }
        return YES;
    }

    // TODO (at least in legacy mode?): At the conclusion of all (relevant?) code paths in this method, before returning,
    // record our current position in the client (assuming the client can report this). Then, here,
    // at the start of the loop before asking the Engine to process the event, check to see if
    // we are still at the same position. If not, then reset the context buffer. This is needed
    // because some clients (e.g. Chrome) handle the mouse down events before we get a crack at them.
    if (_legacyMode) {
        NSRange currentSelRange = [sender selectedRange];
        if (_previousSelRange.location != currentSelRange.location || _previousSelRange.length != currentSelRange.length) {
            if ([self.AppDelegate debugMode])
                NSLog(@"Client selection has changed since context was set. Resetting context...");
            [self updateContextBuffer:sender];
        }
        // REVIEW: For now, if the client always reports its selectedRange as "not found", we're assuming that any
        // previous context we've built up is indeed current. This may be totally untrue.
    }
    
    BOOL handled = NO;
    BOOL deleteBackPosted = NO;
    NSArray *actions = nil;
    if (!self.willDeleteNullChar)
        actions = [self.kme processEvent:event];
    
    if ([self.AppDelegate debugMode])
    {
        NSLog(@"sender type = %@", NSStringFromClass([sender class]));
        NSLog(@"sender selection range location = %lu", [sender selectedRange].location);
        NSLog(@"actions = %@", actions);
    }
    for (NSDictionary *action in actions) {
        NSString *actionType = [[action allKeys] objectAtIndex:0];
        if ([actionType isEqualToString:Q_STR]) {
            if ([self.AppDelegate debugMode])
                NSLog(@"About to start handling Q_STR action...");
            
            NSString *output = [action objectForKey:actionType];
            if ([self.AppDelegate debugMode])
                NSLog(@"output = %@", output);
            if (deleteBackPosted)
            {
//                if (_pendingBuffer == nil || _pendingBuffer.length == 0)
//                {
//                    if ([self.AppDelegate debugMode])
//                        NSLog(@"Posting the 'done deleting' code...");
//                    [self postSpecialDoneDeletingCode];
//                }
                [self appendPendingBuffer:output];
                if ([self.AppDelegate debugMode])
                    NSLog(@"pendingBuffer = %@", [self pendingBuffer]);
            }
            else {
                NSUInteger nc = [self.contextBuffer deleteLastNullChars];
                // Each null in the context buffer presumably corresponds to a space we inserted when
                // processing the Q_BACk, which we now need to replace with the text we're inserting.
                if (nc > 0) {
                    NSRange selRange = [sender selectedRange];
                    NSUInteger pos = selRange.location;
                    if (pos >= nc && pos != NSNotFound) {
                        if ([self.AppDelegate debugMode]) {
                            NSLog(@"Replacement index = %lu", pos - nc);
                            NSLog(@"Replacement length = %lu", nc);
                        }
                        [sender insertText:output replacementRange:NSMakeRange(pos - nc, nc)];
                    }
                }
                else {
                    if ([self.AppDelegate debugMode])
                        NSLog(@"nc = %lu", nc);
                    [sender insertText:output replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
                }
            }
            
            // Even if the characters to insert are pending, we want to append them to the context buffer now.
            // Waiting until they are inserted would probably be safe, but on the off-chance that the engine
            // generates additional actions beyond this current one, we want to be sure that the context reflects
            // the state as it *will* be when all the posted/pending events have been processed.
            [self.contextBuffer appendString:output];
        }
        else if ([actionType isEqualToString:Q_BACK]) {
            if ([self.AppDelegate debugMode])
            {
                NSLog(@"About to start handling Q_BACK action...");
                NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
            }
            [self.contextBuffer deleteLastNullChars];
            NSUInteger dk = [self.contextBuffer deleteLastDeadkeys];
            NSInteger n = [[action objectForKey:actionType] integerValue] - dk;
            NSUInteger dc = [[self.contextBuffer lastNChars:n] deadKeyCount];
            // n is now the number of characters to delete from the context buffer
            // (which could include deadkeys the client doesn't know about).
            [self.contextBuffer deleteLastNChars:n];
            n -= dc;
            
            // n is now the number of characters to delete from the client.
            if (n > 0) {
                if ([self.AppDelegate debugMode])
                {
                    NSLog(@"Q_BACK");
                    NSLog(@"dk = %lu", dk);
                    NSLog(@"n = %li", n);
                    NSLog(@"dc = %lu", dc);
                    if (_legacyMode)
                        NSLog(@"Using Legacy mode.");
                    else
                        NSLog(@"Using Apple IM-compliant mode.");
                }
                NSRange selectedRange = [sender selectedRange];
                NSInteger pos = selectedRange.location;
                if (_legacyMode && self.contextBuffer != nil && (pos == 0 || pos == NSNotFound)) {
                    pos = self.contextBuffer.length + n;
                }
                if ([self.AppDelegate debugMode])
                    NSLog(@"pos = %lu", pos);
                
                if (!_legacyMode && pos >= n && pos != NSNotFound) {
                    if ((pos - (n+1)) >= 0) {
                        NSString *preChar = [[sender attributedSubstringFromRange:NSMakeRange(pos - (n+1), 1)] string];
                        if (preChar) {
                            if ([self.AppDelegate debugMode]) {
                                NSLog(@"preChar (to insert at %lu) = \"%@\"", pos - (n+1), preChar);
                            }
                            [sender insertText:preChar replacementRange:NSMakeRange(pos - (n+1), n+1)];
                        }
                        else {
                            if ([self.AppDelegate debugMode]) {
                                NSLog(@"Switching to leacy mode - client apparently doesn't implement attributedSubstringFromRange.");
                            }
                            _legacyMode = YES; // client apparently doesn't implement attributedSubstringFromRange.
                        }
                    }
                    else {
                        if ([self.AppDelegate debugMode])
                            NSLog(@"No previous character to use for replacement - replacing range with space");
                        [sender insertText:@" " replacementRange:NSMakeRange(pos - n, n)];
                        [self.contextBuffer appendNullChar];
                    }
                }
                if (_legacyMode && pos >= n) {
                    // n is now the number of delete-backs we need to post (plus one more if there is selected text)
                    if ([self.AppDelegate debugMode])
                        NSLog(@"Legacy mode: calling deleteBack");
                    
                    // Note: If pos is "not found", most likely the client can't accurately report the location. This might be
                    // dangerous, but for now let's go ahead and attempt to delete the characters we think should be there.
                    if (_pendingBuffer != nil && [[self pendingBuffer] length] > 0) {
                        NSException* exception = [NSException
                                                  exceptionWithName:@"InvalidOperationException"
                                                  reason:@"Cannot process subsequent Q_BACK after Q_STR"
                                                  userInfo:nil];
                        @throw exception;
                    }
                    if (selectedRange.length > 0)
                        n++; // First delete-back will delete the existing selection.
                    [self deleteBack:n for:event];
                    deleteBackPosted = YES;
                    
                    CFRelease(_sourceFromOriginalEvent);
                    _sourceFromOriginalEvent = nil;
                }
            }
        }
        else if ([actionType isEqualToString:Q_DEADKEY]) {
            [self.contextBuffer deleteLastNullChars];
            NSUInteger x = [[action objectForKey:actionType] unsignedIntegerValue];
            [self.contextBuffer appendDeadkey:x];
        }
        else if ([actionType isEqualToString:Q_NUL]) {
            continue;
        }
        else if ([actionType isEqualToString:Q_RETURN]) {
            if (_legacyMode)
                _previousSelRange = [sender selectedRange];
            return YES;
        }
        else if ([actionType isEqualToString:Q_BEEP]) {
            [[NSSound soundNamed:@"Tink"] play];
        }
        
        handled = YES;
    }
    
    // Apply context changes if not handled
    if (!handled) {
        NSUInteger nc = [self.contextBuffer deleteLastNullChars];
        if (nc > 0) {
            // This can presumably only happen if a previous even resulted in a chain of
            // actions that had a Q_BACK not followed by a Q_STR (assuming that can happen).
            // REVIEW: Need to test this scenario in Atom and Googe Docs in Safari
            self.willDeleteNullChar = YES;
            [self deleteBack:nc for:event];
            _keyCodeOfOriginalEvent = event.keyCode;
            
            //[self sendEvent:event];
            return YES;
        }
        
        BOOL didUpdateContext = NO;
        unsigned short keyCode = event.keyCode;
        if (keyCode <= 0x33) { // Main keys
            if (keyCode == kVK_Return) // Enter/Return
                [self.contextBuffer appendString:@"\n"];
            else if (keyCode == kVK_Delete) {
                // If we have pending characters to insert following the delete-back, then
                // the context buffer has already been properly set to reflect the deletions.
                if ((_legacyMode && (_pendingBuffer == nil || _pendingBuffer.length == 0)) ||
                    (!_legacyMode && (!self.willDeleteNullChar)))
                {
                    // Backspace clears last "real" character from buffer, plus any surrounding deadkeys
                    [self.contextBuffer deleteLastDeadkeys];
                    [self.contextBuffer deleteLastNChars:1];
                    [self.contextBuffer deleteLastDeadkeys];
                    didUpdateContext = YES;
                }
                if (_numberOfPostedDeletesToExpect > 0)
                {
                    if (--_numberOfPostedDeletesToExpect == 0)
                    {
                        if ([self.AppDelegate debugMode])
                            NSLog(@"Processing final posted delete-back...");
                        
                        self.willDeleteNullChar = NO;
                        if (_legacyMode && _pendingBuffer != nil && _pendingBuffer.length > 0)
                        {
                            if ([self.AppDelegate debugMode])
                                NSLog(@"Posting special code to tell IM to insert characters from pending buffer.");
                            [self postKeyPressToFrontProcess:kProcessPendingBuffer from:NULL];
                        }
                        else
                        {
                            if ([self.AppDelegate debugMode])
                                NSLog(@"Re-posting original (unhandled) code: %d", (int)_keyCodeOfOriginalEvent);
                            [self postKeyPressToFrontProcess:_keyCodeOfOriginalEvent from:_sourceFromOriginalEvent];
                            _keyCodeOfOriginalEvent = 0;
                            CFRelease(_sourceFromOriginalEvent);
                            _sourceFromOriginalEvent = nil;
                            _keyCodeOfOriginalEvent = 0;
                        }
                        didUpdateContext = YES;
                    }
                }
                else
                    self.willDeleteNullChar = NO;
            }
            else {
                [self.contextBuffer appendString:event.characters];
                didUpdateContext = YES;
            }
        }
        else {
            unichar ch = [event.characters characterAtIndex:0];
            if (ch >= 0x2A && ch <= 0x39) // Numpad char range + normal punctuation
                [self.contextBuffer appendString:event.characters];
            else if (keyCode == 0x4C) // Enter (Numpad)
                [self.contextBuffer appendString:@"\n"];
            else if (keyCode >= 0x7B && keyCode <= 0x7E) {
                // Arrow keys
                [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
                didUpdateContext = YES;
            }
            else if (keyCode == 0x73 || keyCode == 0x77 || keyCode == 0x74 || keyCode == 0x79) {
                // Home, End, Page Up, Page Down
                [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
                didUpdateContext = YES;
            }
            else {
                // Other keys
            }
        }
        
        if (!didUpdateContext)
            [self.kme setContextBuffer:self.contextBuffer];
    }
    
    if ([self.AppDelegate debugMode]) {
        NSLog(@"handledEvent: %@", handled?@"YES":@"NO");
        NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
        NSLog(@"kme.contextBuffer = \"%@\"", self.kme.contextBuffer.length?[self.kme.contextBuffer codeString]:@"{empty}");
        NSRange range = [sender markedRange];
        NSLog(@"sender.markedRange.location = \"%lu\"", range.location);
        NSLog(@"sender.markedRange.length = \"%lu\"", range.length);
        NSLog(@"***");
    }
    
    if (_legacyMode)
        _previousSelRange = [sender selectedRange];
    return handled;
}

- (void)activateServer:(id)sender {
    [sender overrideKeyboardWithKeyboardNamed:@"com.apple.keylayout.US"];
    [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
}

- (void)deactivateServer:(id)sender {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"*** deactivateServer ***");
        NSLog(@"sender: %@", sender);
        NSLog(@"***");
    }
}

/*
- (NSDictionary *)modes:(id)sender {
    if ([self.AppDelegate debugMode])
        NSLog(@"*** Modes ***");
    if (_kmModes == nil) {
        NSDictionary *amhMode = [[NSDictionary alloc] initWithObjectsAndKeys:@"keyman.png", kTSInputModeAlternateMenuIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeIsVisibleKey,
                                 @"A", kTSInputModeKeyEquivalentKey,
                                 [NSNumber numberWithInteger:4608], kTSInputModeKeyEquivalentModifiersKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 @"keyman.png", kTSInputModeMenuIconFileKey,
                                 @"keyman.png", kTSInputModePaletteIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModePrimaryInScriptKey,
                                 @"smUnicodeScript", kTSInputModeScriptKey,
                                 @"amh", @"TISIntendedLanguage", nil];
        
        NSDictionary *hinMode = [[NSDictionary alloc] initWithObjectsAndKeys:@"keyman.png", kTSInputModeAlternateMenuIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeIsVisibleKey,
                                 @"H", kTSInputModeKeyEquivalentKey,
                                 [NSNumber numberWithInteger:4608], kTSInputModeKeyEquivalentModifiersKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 @"keyman.png", kTSInputModeMenuIconFileKey,
                                 @"keyman.png", kTSInputModePaletteIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModePrimaryInScriptKey,
                                 @"smUnicodeScript", kTSInputModeScriptKey,
                                 @"hin", @"TISIntendedLanguage", nil];
        
        NSDictionary *modeList = [[NSDictionary alloc] initWithObjectsAndKeys:amhMode, @"com.apple.inputmethod.amh", hinMode, @"com.apple.inputmethod.hin", nil];
        NSArray *modeOrder = [[NSArray alloc] initWithObjects:@"com.apple.inputmethod.amh", @"com.apple.inputmethod.hin", nil];
        _kmModes = [[NSDictionary alloc] initWithObjectsAndKeys:modeList, kTSInputModeListKey,
                               modeOrder, kTSVisibleInputModeOrderedArrayKey, nil];
    }

    return _kmModes;
}
*/

- (NSMenu *)menu {
    return self.AppDelegate.menu;
}

- (KMEngine *)kme {
    return self.AppDelegate.kme;
}

- (KMXFile *)kmx {
    return self.AppDelegate.kmx;
}

- (NSMutableString *)contextBuffer {
    return self.AppDelegate.contextBuffer;
}

- (void)updateContextBuffer:(id)sender {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"*** updateContextBuffer ***");
        NSLog(@"sender: %@", sender);
    }
    
    NSRange selRange = [sender selectedRange];
    NSUInteger len = [sender length];
    if ([self.AppDelegate debugMode]) {
        NSLog(@"selRange.location: %lu", (unsigned long)selRange.location);
        NSLog(@"selRange.length: %lu", (unsigned long)selRange.length);
        NSLog(@"sender length: %lu", len);
    }
    if (selRange.location != NSNotFound && selRange.location > 0)
        len = selRange.location;
    NSString *preBuffer = [[sender attributedSubstringFromRange:NSMakeRange(0, len)] string];
    if ([self.AppDelegate debugMode])
        NSLog(@"preBuffer = \"%@\"", preBuffer);
    // REVIEW: If there is ever a situation where preBuffer gets some text but the client reports its
    // selectedRange as not found, we probably can't reliably assume that the current location is really
    // at the end of the "preBuffer", so maybe we just need to assume no context.
    [self.AppDelegate setContextBuffer:preBuffer.length?[NSMutableString stringWithString:preBuffer]:nil];
    if ([self.AppDelegate debugMode]) {
        NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
        NSLog(@"***");
    }
    _previousSelRange = selRange;
}

- (void)clearContextBuffer {
    if ([self.AppDelegate debugMode])
        NSLog(@"*** clearContextBuffer ***");
    [self.AppDelegate setContextBuffer:nil];
    if ([self.AppDelegate debugMode]) {
        NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?self.contextBuffer:@"{empty}");
        NSLog(@"***");
    }
}

- (void)menuAction:(id)sender {
    NSMenuItem *mItem = [sender objectForKey:kIMKCommandMenuItemName];
    NSInteger itag = mItem.tag;
    if (itag == 2) {
        [self showPreferences:sender];
    }
    else if (itag == 3) {
        [self.AppDelegate showOSK];
    }
    else if (itag == 4) {
        [self.AppDelegate showAboutWindow];
    }
    else if (itag >= 1000) {
        NSMenuItem *keyboards = [self.AppDelegate.menu itemWithTag:1];
        for (NSMenuItem *item in keyboards.submenu.itemArray) {
            if (item.tag == itag)
                [item setState:NSOnState];
            else
                [item setState:NSOffState];
        }
        
        NSString *path = [self.AppDelegate.activeKeyboards objectAtIndex:itag%1000];
        KMXFile *kmx = [[KMXFile alloc] initWithFilePath:path];
        [self.AppDelegate setKmx:kmx];
        KVKFile *kvk = nil;
        NSDictionary *kmxInfo = [KMXFile infoDictionaryFromFilePath:path];
        NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
        if (kvkFilename != nil) {
            NSString *kvkFilePath = [self.AppDelegate kvkFilePathFromFilename:kvkFilename];
            if (kvkFilePath != nil)
                kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
        }
        [self.AppDelegate setKvk:kvk];
        [self.AppDelegate setKeyboardName:[kmxInfo objectForKey:kKMKeyboardNameKey]];
        [self.AppDelegate setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
        [self.AppDelegate setContextBuffer:nil];
        [self.AppDelegate setSelectedKeyboard:path];
        if (kvk != nil && self.AppDelegate.alwaysShowOSK)
            [self.AppDelegate showOSK];
    }
}

// Return the pending buffer.  If it is NIL create it.
-(NSMutableString*)pendingBuffer;
{
    if ( _pendingBuffer == nil ) {
        _pendingBuffer = [[NSMutableString alloc] init];
    }
    return _pendingBuffer;
}

// Change the pending buffer.
-(void)setPendingBuffer:(NSString*)string
{
    NSMutableString*        buffer = [self pendingBuffer];
    [buffer setString:string];
}

// Append to (creating if necessary) the pending buffer.
-(void)appendPendingBuffer:(NSString*)string
{
    NSMutableString*        buffer = [self pendingBuffer];
    [buffer appendString:string];
}

- (void)sendEvent:(NSEvent *)event {
    ProcessSerialNumber psn;
    GetFrontProcess(&psn);
    
    CGEventSourceRef source = CGEventCreateSourceFromEvent([event CGEvent]);
    CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(source, event.keyCode, true);
    CGEventRef keyUpEvent = CGEventCreateKeyboardEvent(source, event.keyCode, false);
    
    CGEventPostToPSN(&psn, keyDownEvent);
    CGEventPostToPSN(&psn, keyUpEvent);
    
    CFRelease(source);
    CFRelease(keyDownEvent);
    CFRelease(keyUpEvent);
}

- (void)deleteBack:(NSUInteger)count for:(NSEvent *) event {
    _numberOfPostedDeletesToExpect = count;
    CGEventRef ev;
    _sourceFromOriginalEvent = CGEventCreateSourceFromEvent([event CGEvent]);

    for (int db = 0; db < count; db++)
    {
        if ([self.AppDelegate debugMode])
            NSLog(@"Posting a delete (down/up) at kCGHIDEventTap.");
        //delete-back down
        ev = CGEventCreateKeyboardEvent (_sourceFromOriginalEvent, kVK_Delete, true);
        CGEventPost(kCGHIDEventTap, ev);
        CFRelease(ev);
        
        //delete-back up
        ev = CGEventCreateKeyboardEvent (_sourceFromOriginalEvent, kVK_Delete, false);
        CGEventPost(kCGHIDEventTap, ev);
        CFRelease(ev);
    }
}

- (void)postKeyPressToFrontProcess:(CGKeyCode)code from:(CGEventSourceRef) source {
    ProcessSerialNumber psn;
    GetFrontProcess(&psn);
    
    CGEventRef event = CGEventCreateKeyboardEvent(source, code, true);
    CGEventPostToPSN(&psn, event);
    CFRelease(event);
    
    if (code != kProcessPendingBuffer) { // special 0xFF code is not a real key-press, so no "up" is needed
        event = CGEventCreateKeyboardEvent(source, code, false);
        CGEventPostToPSN(&psn, event);
        CFRelease(event);
    }
}

@end
