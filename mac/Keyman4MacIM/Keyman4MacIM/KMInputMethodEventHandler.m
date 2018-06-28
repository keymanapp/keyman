//
//  KMInputMethodEventHandler.m
//  Keyman4MacIM
//
//  Created by Tom Bogle on 11/22/17.
//  Copyright © 2017-2018 SIL International. All rights reserved.
//
#import "KMInputMethodEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#include <Carbon/Carbon.h> /* For kVK_ constants. */
#import <Crashlytics/Crashlytics.h>

@implementation KMInputMethodEventHandler

const CGKeyCode kProcessPendingBuffer = 0xFF;
const NSUInteger kMaxContext = 80;

//_pendingBuffer contains text that is ready to be sent to the client when all delete-backs are finished.
NSMutableString* _pendingBuffer;
NSUInteger _numberOfPostedDeletesToExpect = 0;
CGKeyCode _keyCodeOfOriginalEvent;
CGEventSourceRef _sourceFromOriginalEvent = nil;
NSMutableString* _easterEggForCrashlytics = nil;
NSString* const kEasterEggText = @"Crashlytics force now";
NSString* const kEasterEggKmxName = @"EnglishSpanish.kmx";

NSRange _previousSelRange;

// Protected initializer for use by subclasses
- (instancetype)initWithLegacyMode:(BOOL)legacy clientSelectionCanChangeUnexpectedly:(BOOL) flagClientSelectionCanChangeUnexpectedly {
    self = [super init];
    if (self) {
        _previousSelRange = NSMakeRange(NSNotFound, NSNotFound);
        _clientSelectionCanChangeUnexpectedly = flagClientSelectionCanChangeUnexpectedly;
        _clientCanProvideSelectionInfo = Unknown;
        _legacyMode = NO;
        _contextOutOfDate = YES;
        if (legacy) {
            [self switchToLegacyMode];
        }
    }
    return self;
}

// This is the public initializer.
- (instancetype)initWithClient:(NSString *)clientAppId {
    // TODO: Pages and Keynote (and possibly lots of other undiscovered apps that are otherwise compliant
    // with Apple's IM faramework) have a problem in that if the user selects a different font (or other
    // formatting) and then types a sequence that causes characters to be added to the document and then
    // subsequently replaced, the replacement causes the formatting decision to be forgotten. This can be
    // "fixed" by treating them as legacy apps, but it causes other problems.
    BOOL legacy = ([clientAppId isEqual: @"com.github.atom"] ||
            [clientAppId isEqual: @"com.collabora.libreoffice-free"] ||
            [clientAppId isEqual: @"org.libreoffice.script"] ||
            [clientAppId isEqual: @"com.axosoft.gitkraken"] ||
            [clientAppId isEqual: @"org.sil.app.builder.scripture.ScriptureAppBuilder"] ||
            [clientAppId isEqual: @"org.sil.app.builder.reading.ReadingAppBuilder"] ||
            [clientAppId isEqual: @"org.sil.app.builder.dictionary.DictionaryAppBuilder"] ||
            [clientAppId isEqual: @"com.microsoft.Word"] ||
            [clientAppId isEqual: @"org.openoffice.script"] ||
            [clientAppId isEqual: @"com.adobe.InDesign"]
               /*||[clientAppId isEqual: @"ro.sync.exml.Oxygen"] - Oxygen has worse problems */);
    
    // We used to default to NO, so these were the obvious exceptions. But then we realized that
    // in any app, command keys can change the selection, so now we default to YES, and only have
    // a few situations where we pretend it can't. This flag should probably be renamed to something
    // like "disregardPossibleSelectionChanges".
    //    if ([clientAppId isEqual: @"com.google.Chrome"] ||
    //        [clientAppId isEqual: @"com.apple.Terminal"] ||
    //        [clientAppId isEqual: @"com.apple.dt.Xcode"]) {
    //        _clientSelectionCanChangeUnexpectedly = YES;
    //    }
    
    // In Xcode, if Keyman is the active IM and is in "debugMode" and "English plus Spanish" is the current keyboard and you type "Crashlytics force now", it will force a simulated crash to test reporting to fabric.io.
    if ([self.AppDelegate debugMode] && [clientAppId isEqual: @"com.apple.dt.Xcode"]) {
        NSLog(@"Crashlytics - Preparing to detect Easter egg.");
        _easterEggForCrashlytics = [[NSMutableString alloc] init];
    }
    else
        _easterEggForCrashlytics = nil;
    
    // For the Atom editor, this isn't really true (the context CAN change unexpectedly), but we can't get
    // the context, so we pretend/hope it won't.
    BOOL selectionCanChangeUnexpectedly = (![clientAppId isEqual: @"com.github.atom"]);
    return [self initWithLegacyMode:legacy clientSelectionCanChangeUnexpectedly:selectionCanChangeUnexpectedly];
}

- (void)switchToLegacyMode {
    _legacyMode = YES;
    if ([self.AppDelegate debugMode])
        NSLog(@"Using legacy mode for this app.");
}

- (void)deactivate {
    if (_numberOfPostedDeletesToExpect > 0 || (_pendingBuffer != nil && _pendingBuffer.length > 0) ||
        _keyCodeOfOriginalEvent != 0 || _sourceFromOriginalEvent != nil)
    {
        if ([self.AppDelegate debugMode]) {
            NSLog(@"ERROR: new app activated before previous app finished processing pending events!");
            NSLog(@"  _numberOfPostedDeletesToExpect = %lu", _numberOfPostedDeletesToExpect);
            NSLog(@"  pendingBuffer = \"%@\"", _pendingBuffer == nil ? @"(NIL)" : (NSString*)[self pendingBuffer]);
            NSLog(@"  _keyCodeOfOriginalEvent = %hu", _keyCodeOfOriginalEvent);
        }
        _numberOfPostedDeletesToExpect = 0;
        // If _sourceFromOriginalEvent != nil, we should probably attempt to release and clear it.
        // [self dealloc]
        _pendingBuffer = nil;
        _keyCodeOfOriginalEvent = 0;
    }
}

- (void)dealloc {
    if (_sourceFromOriginalEvent != nil) {
        CFRelease(_sourceFromOriginalEvent);
        _sourceFromOriginalEvent = nil;
    }
}

- (void)handleCommand:(NSEvent *)event {
    // There are a bunch of common navigation/selection command-key combinations, but individual
    // apps may implement specific commands that also change the selection. There is probably no
    // situation where a user could reasonably hope that any dead-keys typed before using a
    // command shortcut would be remembered, so other than an insignificant performance penalty,
    // the only downside to treating all commands as having the potential to change the selection
    // is that some "legacy" apps can't get their context at all. This can be overridden so that
    // legacy apps can mitigate this problem as appropriate.
    if ([self.AppDelegate debugMode])
        NSLog(@"Command key - context needs to be re-gotten.");
    _contextOutOfDate = YES;
}

- (void)checkContextIn:(id)client {
    // Base implementation is no-op
}

- (void)replaceExistingSelectionIn:(id)client with:(NSString *) text {
    [client insertText:text replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
    _previousSelRange.location += text.length;
    _previousSelRange.length = 0;
}

- (void)insertPendingBufferTextIn:(id)client {
    NSUInteger length = [self pendingBuffer].length;
    if (!length) {
        if ([self.AppDelegate debugMode])
            NSLog(@"Error - expected text in pending buffer!");
        return;
    }
    NSString* text = [self pendingBuffer];
    
    if ([self.AppDelegate debugMode])
        NSLog(@"Inserting text from pending buffer: \"%@\"", text);
        
    [client insertText:text replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
    _previousSelRange.location += text.length;
    _previousSelRange.length = 0;
    
    [self setPendingBuffer:@""];
}

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (KMEngine *)kme {
    return self.AppDelegate.kme;
}

- (NSMutableString *)contextBuffer {
    if (_contextOutOfDate) {
        NSException* exception = [NSException
                                  exceptionWithName:@"InvalidOperationException"
                                  reason:@"Attempt to access out-of-date context."
                                  userInfo:nil];
        @throw exception;
    }
    return self.AppDelegate.contextBuffer;
}

- (NSRange)getSelectionRangefromClient:(id)client {
    switch (_clientCanProvideSelectionInfo) {
        case Unknown:
            if ([client respondsToSelector:@selector(selectedRange)]) {
                NSRange selRange = [client selectedRange];
                if (selRange.location != NSNotFound || selRange.length != NSNotFound) {
                    _clientCanProvideSelectionInfo = Yes;
                    if ([self.AppDelegate debugMode]) {
                        NSLog(@"Client can provide selection info.");
                        NSLog(@"selRange.location: %lu", (unsigned long)selRange.location);
                        NSLog(@"selRange.length: %lu", (unsigned long)selRange.length);
                    }
                    return selRange;
                }
            }
            _clientCanProvideSelectionInfo = No;
            // REVIEW: For now, if the client reports its location as "not found", we're stuck assuming that we're
            // still in the same location in the context. This may be totally untrue, but if the client can't report
            // its location, we have nothing else to go on.
            _clientSelectionCanChangeUnexpectedly = NO;
            if ([self.AppDelegate debugMode])
                NSLog(@"Client can NOT provide selection info!");
            // fall through
        case No:
            return NSMakeRange(NSNotFound, NSNotFound);
        case Yes:
        case Unreliable:
        {
            NSRange selRange = [client selectedRange];
            if ([self.AppDelegate debugMode]) {
                NSLog(@"selRange.location: %lu", (unsigned long)selRange.location);
            }
            return selRange;
        }
    }
}

- (void)updateContextBuffer:(id)sender {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"*** updateContextBuffer ***");
        NSLog(@"sender: %@", sender);
    }
    
    NSRange selRange = [self getSelectionRangefromClient: sender];
    
    // Since client can't tell us the actual current position, we assume the previously known location in the context.
    // (It won't matter anyway if the client also fails to report its context.)
    NSUInteger len = (_clientCanProvideSelectionInfo != Yes) ? _previousSelRange.length : selRange.location;
    NSString *preBuffer = [self getLimitedContextFrom:sender at:len];

    // REVIEW: If there is ever a situation where preBuffer gets some text but the client reports its
    // selectedRange as not found, we probably can't reliably assume that the current location is really
    // at the end of the "preBuffer", so maybe we just need to assume no context.
    [self.AppDelegate setContextBuffer:preBuffer.length?[NSMutableString stringWithString:preBuffer]:nil];
    _contextOutOfDate = NO;
    if ([self.AppDelegate debugMode]) {
        NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
        NSLog(@"***");
    }
    _previousSelRange = selRange;
}

-(NSString *)getLimitedContextFrom:(id)sender at:(NSUInteger) len {
    if (![sender respondsToSelector:@selector(attributedSubstringFromRange:)])
        return nil;
        
    NSUInteger start = 0;
    if (len > kMaxContext) {
        if ([self.AppDelegate debugMode])
            NSLog(@"Limiting context to %lu characters", kMaxContext);
        start = len - kMaxContext;
        len = kMaxContext;
    }
    
    NSString *preBuffer = [[sender attributedSubstringFromRange:NSMakeRange(start, len)] string];
    if ([self.AppDelegate debugMode]) {
        NSLog(@"preBuffer = \"%@\"", preBuffer ? preBuffer : @"nil");
        if (preBuffer.length)
            NSLog(@"First character: '%x'", [preBuffer characterAtIndex:0]);
        else
            NSLog(@"preBuffer has a length of 0");
    }
    
    return preBuffer;
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

- (void)updateContextBufferIfNeeded:(id)client {
    // REVIEW: if self.AppDelegate.lowLevelEventTap == nil under what circumstances might we be able to safely
    // re-get the context? (Probably clientCanProvideSelectionInfo would need to be true, and it would only
    // actually be useful if client respondsToSelector:@selector(attributedSubstringFromRange:), but even then
    // we'd only want to do it if we have actually moved from our previous location. Otherwise, we wouldn't be
    // able to handle dead keys correctly.)
    if (self.AppDelegate.contextChangingEventDetected)
    {
        if (!_contextOutOfDate && [self.AppDelegate debugMode]) {
            NSLog(@"Low-level event requires context to be re-retrieved.");
        }
        _contextOutOfDate = YES;
        self.AppDelegate.contextChangingEventDetected = NO;
    }
  
    if (_contextOutOfDate)
        [self updateContextBuffer:client];
}

- (BOOL) handleKeymanEngineActions:(NSEvent *)event in:(id) sender {
    BOOL deleteBackPosted = NO;
    NSArray *actions = nil;
    if (![self willDeleteNullChar]) {
        if (self.AppDelegate.lowLevelEventTap != nil) {
            NSEvent *eventWithOriginalModifierFlags = [NSEvent keyEventWithType:event.type location:event.locationInWindow modifierFlags:self.AppDelegate.currentModifierFlags timestamp:event.timestamp windowNumber:event.windowNumber context:event.context characters:event.characters charactersIgnoringModifiers:event.charactersIgnoringModifiers isARepeat:event.isARepeat keyCode:event.keyCode];
            actions = [self.kme processEvent:eventWithOriginalModifierFlags];
        }
        else {
            // Depending on the client app and the keyboard, using the passed-in event as it is should work okay.
            // Keyboards that depend on chirality support will not work. And command-key actions that change the
            // context might go undetected in some apps, resulting in errant behavior for subsequent typing.
            actions = [self.kme processEvent:event];
        }
        if (actions.count == 0) {
            if (_easterEggForCrashlytics != nil) {
                NSString * kmxName = [[self.kme.kmx filePath] lastPathComponent];
                NSLog(@"Crashlytics - KMX name: %@", kmxName);
                if ([kmxName isEqualToString:kEasterEggKmxName]) {
                    NSUInteger len = [_easterEggForCrashlytics length];
                    NSLog(@"Crashlytics - Processing character(s): %@", [event characters]);
                    if ([[event characters] characterAtIndex:0] == [kEasterEggText characterAtIndex:len]) {
                        NSString *characterToAdd = [kEasterEggText substringWithRange:NSMakeRange(len, 1)];
                        NSLog(@"Crashlytics - Adding character to Easter Egg code string: %@", characterToAdd);
                        [_easterEggForCrashlytics appendString:characterToAdd];
                        if ([_easterEggForCrashlytics isEqualToString:kEasterEggText]) {
                            NSLog(@"Crashlytics - Forcing crash now with API Key: %@", [[Crashlytics sharedInstance] APIKey]);
                            [[Crashlytics sharedInstance] crash];
                        }
                    }
                    else if (len > 0) {
                        NSLog(@"Crashlytics - Clearing Easter Egg code string.");
                        [_easterEggForCrashlytics setString:@""];
                    }
                }
            }
            return NO;
        }
    }
    else
        return NO;
    
    if ([self.AppDelegate debugMode]) {
        NSLog(@"actions = %@", actions);
    }
    for (NSDictionary *action in actions) {
        NSString *actionType = [[action allKeys] objectAtIndex:0];
        if ([self.AppDelegate debugMode]) {
            NSLog(@"Handling %@ action...", actionType);
            NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
        }
        
        if ([actionType isEqualToString:Q_STR]) {
            NSString *output = [action objectForKey:actionType];
            if ([self.AppDelegate debugMode])
                NSLog(@"output = %@", output);
            if (deleteBackPosted)
            {
                [self appendPendingBuffer:output];
                if ([self.AppDelegate debugMode])
                    NSLog(@"pendingBuffer = %@", [self pendingBuffer]);
            }
            else {
                NSUInteger nc = [self.contextBuffer deleteLastNullChars];
                // Each null in the context buffer presumably corresponds to a space we inserted when
                // processing the Q_BACK, which we now need to replace with the text we're inserting.
                if (nc > 0) {
                    if ([self.AppDelegate debugMode])
                        NSLog(@"nc = %lu", nc);
                    NSUInteger pos = [self getSelectionRangefromClient:sender].location;
                    if (pos >= nc && pos != NSNotFound) {
                        if ([self.AppDelegate debugMode]) {
                            NSLog(@"Replacement index = %lu", pos - nc);
                            NSLog(@"Replacement length = %lu", nc);
                        }
                        [sender insertText:output replacementRange:NSMakeRange(pos - nc, nc)];
                        _previousSelRange.location += output.length - nc;
                        _previousSelRange.length = 0;
                    }
                }
                else {
                    [self replaceExistingSelectionIn:sender with:output];
                }
            }
            
            // Even if the characters to insert are pending, we want to append them to the context buffer now.
            // Waiting until they are inserted would probably be safe, but on the off-chance that the engine
            // generates additional actions beyond this current one, we want to be sure that the context reflects
            // the state as it *will* be when all the posted/pending events have been processed.
            [self.contextBuffer appendString:output];
        }
        else if ([actionType isEqualToString:Q_BACK]) {
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
                deleteBackPosted = [self deleteBack:n in:sender for: event];
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
            return YES;
        }
        else if ([actionType isEqualToString:Q_BEEP]) {
            [[NSSound soundNamed:@"Tink"] play];
        }
    }
    return YES;
}

- (void)processUnhandledDeleteBack:(id)client updateEngineContext:(BOOL *)updateEngineContext {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"Processing an unhandled delete-back...");
        NSLog(@"_numberOfPostedDeletesToExpect = %lu", _numberOfPostedDeletesToExpect);
    }
    
    // If we have pending characters to insert following the delete-back, then
    // the context buffer has already been properly set to reflect the deletions.
    if ((_legacyMode && (_pendingBuffer == nil || _pendingBuffer.length == 0)) ||
        (!_legacyMode && (!self.willDeleteNullChar)))
    {
        // Backspace clears last "real" character from buffer, plus any surrounding deadkeys
        [self.contextBuffer deleteLastDeadkeys];
        [self.contextBuffer deleteLastNChars:1];
        if (_legacyMode) {
            _previousSelRange.location -= 1;
            _previousSelRange.length = 0;
        }
        [self.contextBuffer deleteLastDeadkeys];
    }
    if (_numberOfPostedDeletesToExpect > 0) {
        if (--_numberOfPostedDeletesToExpect == 0) {
            if ([self.AppDelegate debugMode])
                NSLog(@"Processing final posted delete-back...");
            
            self.willDeleteNullChar = NO;
            if (_legacyMode) {
                if (_pendingBuffer != nil && _pendingBuffer.length > 0) {
                    if ([self.AppDelegate debugMode])
                        NSLog(@"Posting special code to tell IM to insert characters from pending buffer.");
                    [self performSelector:@selector(initiatePendingBufferProcessing:) withObject:client afterDelay:0.1];
                }
            }
            else {
                if (!_sourceFromOriginalEvent) {
                    NSLog(@"----- If not legacy mode, _sourceFromOriginalEvent should be retained until original code is posted -----");
                }
                else {
                    if ([self.AppDelegate debugMode]) {
                        NSLog(@"Re-posting original (unhandled) code: %d", (int)_keyCodeOfOriginalEvent);
                    }
                    [self postKeyPressToFrontProcess:_keyCodeOfOriginalEvent from:_sourceFromOriginalEvent];
                    _keyCodeOfOriginalEvent = 0;
                    CFRelease(_sourceFromOriginalEvent);
                    _sourceFromOriginalEvent = nil;
                }
            }
            *updateEngineContext = NO;
        }
    }
    else {
        self.willDeleteNullChar = NO;
    }
}

- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
    // OSK key feedback from hardware keyboard is disabled
    /*if (event.type == NSKeyDown)
     [self.AppDelegate handleKeyEvent:event];*/
    
    if (event.type == NSLeftMouseDown || event.type == NSLeftMouseUp ) {
        _contextOutOfDate = YES;
        return NO;
    }
    else if (event.type != NSKeyDown)
        return NO; // We ignore NSLeftMouseDragged events (because we'll eventually get a mouse-up).
    else if ((event.modifierFlags & NSEventModifierFlagCommand) == NSEventModifierFlagCommand) {
        [self handleCommand:event];
        return NO; // We let the client app handle all Command-key events.
    }
    
    if (_legacyMode && event.keyCode == kProcessPendingBuffer)
    {
        if ([self.AppDelegate debugMode]) {
            NSLog(@"Processing the special %hu code", kProcessPendingBuffer);
            
            NSUInteger length = [self pendingBuffer].length;
            if (length > 0) {
                for (NSUInteger ich = 0; ich < length; ich++)
                    NSLog(@"Char %li: '%x'", ich, [[self pendingBuffer] characterAtIndex:ich]);
            }
        }

        [self insertPendingBufferTextIn:sender];
        return YES;
    }
    
    [self checkContextIn:sender];

    [self updateContextBufferIfNeeded:sender];
    
    if ([self.AppDelegate debugMode]) {
        NSLog(@"sender type = %@", NSStringFromClass([sender class]));
        if (_clientCanProvideSelectionInfo == Yes)
            NSLog(@"sender selection range location = %lu", [self getSelectionRangefromClient:sender].location);
    }
    
    BOOL handled = [self handleKeymanEngineActions:event in: sender];
    
    if (!handled) {
        NSUInteger nc = [self.contextBuffer deleteLastNullChars];
        if (nc > 0) {
            if ([self.AppDelegate debugMode])
                NSLog(@"Deleted %li null characters from context buffer", nc);
            // This can presumably only happen if a previous event resulted in a chain of
            // actions that had a Q_BACK not followed by a Q_STR.
            // REVIEW: Need to test this scenario in Atom and Googe Docs in Safari
            self.willDeleteNullChar = YES;
            [self postDeleteBacks:nc for:event];
            _keyCodeOfOriginalEvent = event.keyCode;
            
            return YES;
        }
        
        // For other events that the Keyman engine does not have rules, just apply context changes
        // and let client handle the event
        NSString* charactersToAppend = nil;
        BOOL updateEngineContext = YES;
        unsigned short keyCode = event.keyCode;
        switch (keyCode) {
            case kVK_Delete:
                [self processUnhandledDeleteBack:sender updateEngineContext:&updateEngineContext];
                break;
                
            case kVK_LeftArrow:
                // I had started some code to try to guess where in the context we ended up after a left arrow,
                // but too many potential pitfalls. Marc says it's better to just let it be dumb.
            case kVK_RightArrow:
            case kVK_UpArrow:
            case kVK_DownArrow:
            case kVK_Home:
            case kVK_End:
            case kVK_PageUp:
            case kVK_PageDown:
                _contextOutOfDate = YES;
                updateEngineContext = NO;
                break;
                
            case kVK_Return:
            case kVK_ANSI_KeypadEnter:
                charactersToAppend = @"\n";
                break;
                
            default:
            {
                // NOTE: Although ch is usually the same as keyCode, when the option key is depressed (and
                // perhaps in some other cases) it may not be (keyCode can be 0). Likewise, the option key
                // can generate more than one character in event.characters.
                unichar ch = [event.characters characterAtIndex:0];
                if (keyCode < 0x33 || (ch >= 0x2A && ch <= 0x39)) { // Main keys, Numpad char range, normal punctuation
                    charactersToAppend = event.characters;
                }
                else {
                    // Other keys
                }
            }
                break;
        }
        if (charactersToAppend != nil) {
            if ([self.AppDelegate debugMode]) {
                NSLog(@"Adding \"%@\" to context buffer", charactersToAppend);
            }
            [self.contextBuffer appendString:charactersToAppend];
            if (_legacyMode) {
                _previousSelRange.location += charactersToAppend.length;
                _previousSelRange.length = 0;
            }
        }
        
        if (updateEngineContext) {
            [self.kme setContextBuffer:self.contextBuffer];
        }
    }
    
    if ([self.AppDelegate debugMode]) {
        if (_contextOutOfDate)
            NSLog(@"Context now out of date.");
        else
            NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
        NSLog(@"kme.contextBuffer = \"%@\"", self.kme.contextBuffer.length?[self.kme.contextBuffer codeString]:@"{empty}");
        if (_clientCanProvideSelectionInfo == Yes) {
            NSRange range = [self getSelectionRangefromClient:sender]; // This call (in debug) reports location to Console
            NSLog(@"sender.selectedRange.length = %lu", range.length);
        }
        NSLog(@"***");
    }
    
    // Note: Although this would seem to be the obvious place to set _previousSelRange (in legacy mode), we can't
    // because the selection range doesn't get updated until after we return from this method.
    return handled;
}

- (BOOL)deleteBack:(NSUInteger)n in:(id) client for:(NSEvent *) event {
    if ([self.AppDelegate debugMode])
        NSLog(@"Attempting to back-delete %li characters.", n);
    NSRange selectedRange = [self getSelectionRangefromClient:client];
    NSInteger pos = selectedRange.location;
    if (!_legacyMode)
        [self deleteBack:n at: pos in: client];
    if (_legacyMode)
        return [self deleteBackLegacy:n at: pos with: selectedRange for: event];
    
    return NO;
}

- (void)deleteBack:(NSUInteger)n at:(NSUInteger) pos in:(id)client {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"Using Apple IM-compliant mode.");
        NSLog(@"pos = %lu", pos);
    }
    
    if (pos >= n && pos != NSNotFound) {
        NSInteger preCharPos = pos - (n+1);
        if ((preCharPos) >= 0) {
            NSUInteger nbrOfPreCharacters;
            NSString *preChar = nil;
            
            // This regex will look back through the context until it finds a *known* base
            // character because some (non-legacy) apps (e.g., Mail) do not properly handle sending
            // combining marks on their own via insertText. One potentially negative implication
            // of this is that if the script should happen to contain characters whose class is
            // not known, it will skip over them and keep looking, so it could end up using a
            // longer string of characters than otherwise necessary. This could result in a
            // mildly jarring visual experience for the user if the app refreshes the diplay
            // between the time the characters are removed and re-inserted. But presumbly this
            // algorithm will eventually find either a known base character or get all the way back
            // to the start of the context, so if it doesn't find a known base character, it will
            // fall back to just attempting the insert with whatever it does find. I believe this
            // will always work and should at least work as reliably as the old version of the code,
            // which always used just a single character regardless of its class.
            NSError *error = NULL;
            NSRegularExpression *regexNonCombiningMark = [NSRegularExpression regularExpressionWithPattern:@"\\P{M}" options:NSRegularExpressionCaseInsensitive error:&error];
            
            for (nbrOfPreCharacters = 1; YES; nbrOfPreCharacters++, preCharPos--) {
                if ([client respondsToSelector:@selector(attributedSubstringFromRange:)])
                    preChar = [[client attributedSubstringFromRange:NSMakeRange(preCharPos, nbrOfPreCharacters)] string];
                if (!preChar) {
                    if ([self.AppDelegate debugMode]) {
                        NSLog(@"Client apparently doesn't implement attributedSubstringFromRange. Attempting to get preChar from context...");
                    }
                    if (self.contextBuffer != nil && preCharPos < self.contextBuffer.length) {
                        preChar = [self.contextBuffer substringWithRange:NSMakeRange(preCharPos, 1)];
                    }
                    if (!preChar)
                        break;
                }
                if ([self.AppDelegate debugMode])
                    NSLog(@"Testing preChar: %@", preChar);
                
                if ([regexNonCombiningMark numberOfMatchesInString:preChar options:NSMatchingAnchored range:NSMakeRange(0, 1)] > 0)
                    break;
                if (preCharPos == 0) {
                    if ([self.AppDelegate debugMode]) {
                        NSLog(@"Failed to find a base character!");
                    }
                    break;
                }
                if ([self.AppDelegate debugMode]) {
                    NSLog(@"Have not yet found a base character. nbrOfPreCharacters = %lu", nbrOfPreCharacters);
                }
            }
            if (preChar) {
                if ([self.AppDelegate debugMode]) {
                    NSLog(@"preChar (to insert at %lu) = \"%@\"", preCharPos, preChar);
                }
                [client insertText:preChar replacementRange:NSMakeRange(preCharPos, n+nbrOfPreCharacters)];
            }
            else {
                if ([self.AppDelegate debugMode]) {
                    NSLog(@"Switching to legacy mode - client apparently doesn't implement attributedSubstringFromRange and no previous character in context buffer.");
                }
                _legacyMode = YES; // client apparently doesn't implement attributedSubstringFromRange.
            }
        }
        else {
            if ([self.AppDelegate debugMode])
                NSLog(@"No previous character to use for replacement - replacing range with space");
            [client insertText:@" " replacementRange:NSMakeRange(pos - n, n)];
            [self.contextBuffer appendNullChar];
        }
    }
}

- (BOOL)deleteBackLegacy:(NSUInteger)n at:(NSUInteger) pos with:(NSRange) selectedRange for:(NSEvent *) event {
    if (self.contextBuffer != nil && (pos == 0 || pos == NSNotFound)) {
        pos = self.contextBuffer.length + n;
    }
    
    if ([self.AppDelegate debugMode]) {
        NSLog(@"Using Legacy mode.");
        NSLog(@"pos = %lu", pos);
    }
    
    if (pos >= n) {
        // n is now the number of delete-backs we need to post (plus one more if there is selected text)
        if ([self.AppDelegate debugMode]) {
            NSLog(@"Legacy mode: calling postDeleteBacks");
            if (_clientCanProvideSelectionInfo == No || _clientCanProvideSelectionInfo == Unreliable)
                NSLog(@"Cannot trust client to report accurate selection length - assuming no selection.");
        }
        
        // Note: If pos is "not found", most likely the client can't accurately report the location. This might be
        // dangerous, but for now let's go ahead and attempt to delete the characters we think should be there.
        if (_pendingBuffer != nil && [[self pendingBuffer] length] > 0) {
            NSException* exception = [NSException
                                      exceptionWithName:@"InvalidOperationException"
                                      reason:@"Cannot process subsequent Q_BACK after Q_STR"
                                      userInfo:nil];
            @throw exception;
        }
        
        if (_clientCanProvideSelectionInfo == Yes && selectedRange.length > 0)
            n++; // First delete-back will delete the existing selection.
        [self postDeleteBacks:n for:event];
        
        CFRelease(_sourceFromOriginalEvent);
        _sourceFromOriginalEvent = nil;
        return YES;
    }
    return NO;
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

- (void)postDeleteBacks:(NSUInteger)count for:(NSEvent *) event {
    _numberOfPostedDeletesToExpect = count;
    
    _sourceFromOriginalEvent = CGEventCreateSourceFromEvent([event CGEvent]);
    
    for (int db = 0; db < count; db++)
    {
        if ([self.AppDelegate debugMode]) {
            NSLog(@"Posting a delete (down/up) at kCGHIDEventTap.");
        }
        [self.AppDelegate postKeyboardEventWithSource:_sourceFromOriginalEvent code:kVK_Delete postCallback:^(CGEventRef eventToPost) {
            CGEventPost(kCGHIDEventTap, eventToPost);
        }];
    }
}

- (void)initiatePendingBufferProcessing:(id)sender {
    [self postKeyPressToFrontProcess:kProcessPendingBuffer from:NULL];
}

- (void)postKeyPressToFrontProcess:(CGKeyCode)code from:(CGEventSourceRef) source {
    ProcessSerialNumber psn;
    GetFrontProcess(&psn);
    
    if ([self.AppDelegate debugMode]) {
        if (code == kProcessPendingBuffer) {
            NSLog(@"Posting code to tell Keyman to process characters in pending buffer.");
        }
        else {
            NSLog(@"Posting a keypress (down/up) to the 'front process' for the %hu key.", code);
        }
    }
    
    [self.AppDelegate postKeyboardEventWithSource:source code:code postCallback:^(CGEventRef eventToPost) {
        CGEventPostToPSN((ProcessSerialNumberPtr)&psn, eventToPost);
    }];
}

@end
