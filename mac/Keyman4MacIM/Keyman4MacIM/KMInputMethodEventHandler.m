//
//  KMInputMethodEventHandler.m
//  Keyman4MacIM
//
//  Created by Tom Bogle on 11/22/17.
//  Copyright © 2017-2018 SIL International. All rights reserved.
//
#import "KMInputMethodEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>
#import <Carbon/Carbon.h> /* For kVK_ constants. */
#import "KMContext.h"
#import "KeySender.h"
#import "KMCoreActionHandler.h"
#import "TextApiCompliance.h"
//TODO: remove debug call to PrivacyConsent
#import "PrivacyConsent.h"

@import Sentry;

@interface KMInputMethodEventHandler ()
@property KMContext *cachedContext; // TODO: rename after eliminating other context references
@property (nonatomic, retain) KeySender* keySender;
@property (nonatomic, retain) TextApiCompliance* apiCompliance;
@property int generatedBackspaceCount;
@property (nonatomic, retain) NSString* clientApplicationId;
@property NSString *queuedText;
@end

@implementation KMInputMethodEventHandler

const CGKeyCode kProcessPendingBuffer = 0xFF;
const NSUInteger kMaxContext = 80;

//_pendingBuffer contains text that is ready to be sent to the client when all delete-backs are finished.
NSMutableString* _pendingBuffer;
NSUInteger _numberOfPostedDeletesToExpect = 0;
CGKeyCode _keyCodeOfOriginalEvent;
CGEventSourceRef _sourceFromOriginalEvent = nil;
NSMutableString* _easterEggForSentry = nil;
NSString* const kEasterEggText = @"Sentry force now";
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
- (instancetype)initWithClient:(NSString *)clientAppId client:(id) sender {
    self.senderForDeleteBack = sender;
    _keySender = [[KeySender alloc] init];
    _cachedContext = [[KMContext alloc] init];
    _clientApplicationId = clientAppId;
    _generatedBackspaceCount = 0;
  
    
    //BOOL legacy = [self isClientAppLegacy:clientAppId];

    // In Xcode, if Keyman is the active IM and is in "debugMode" and "English plus Spanish" is 
    // the current keyboard and you type "Sentry force now", it will force a simulated crash to 
    // test reporting to sentry.keyman.com
    if ([self.appDelegate debugMode] && [clientAppId isEqual: @"com.apple.dt.Xcode"]) {
        NSLog(@"Sentry - Preparing to detect Easter egg.");
        _easterEggForSentry = [[NSMutableString alloc] init];
    }
    else
        _easterEggForSentry = nil;

    // For the Atom editor, this isn't really true (the context CAN change unexpectedly), but we can't get
    // the context, so we pretend/hope it won't.
    BOOL selectionCanChangeUnexpectedly = (![clientAppId isEqual: @"com.github.atom"]);
    return [self initWithLegacyMode:self.apiCompliance.mustBackspaceUsingEvents clientSelectionCanChangeUnexpectedly:selectionCanChangeUnexpectedly];
}

- (void)switchToLegacyMode {
    _legacyMode = YES;
    if ([self.appDelegate debugMode])
        NSLog(@"Using legacy mode for this app.");
}

- (void)deactivate {
    if (_numberOfPostedDeletesToExpect > 0 || (_pendingBuffer != nil && _pendingBuffer.length > 0) ||
        _keyCodeOfOriginalEvent != 0 || _sourceFromOriginalEvent != nil)
    {
        if ([self.appDelegate debugMode]) {
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
    if ([self.appDelegate debugMode])
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
        [self.appDelegate logDebugMessage:@"Error - expected text in pending buffer!"];
        return;
    }
    NSString* text = [self pendingBuffer];

    [self.appDelegate logDebugMessage:@"Inserting text from pending buffer: \"%@\"", text];

    [client insertText:text replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
    _previousSelRange.location += text.length;
    _previousSelRange.length = 0;

    [self setPendingBuffer:@""];
}

- (KMInputMethodAppDelegate *)appDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (KMEngine *)kme {
    return self.appDelegate.kme;
}

- (NSMutableString *)contextBuffer {
    if (_contextOutOfDate) {
        NSException* exception = [NSException
                                  exceptionWithName:@"InvalidOperationException"
                                  reason:@"Attempt to access out-of-date context."
                                  userInfo:nil];
        @throw exception;
    }
    return self.appDelegate.contextBuffer;
}

- (NSRange)getSelectionRangefromClient:(id)client {
    switch (_clientCanProvideSelectionInfo) {
        case Unknown:
            if ([client respondsToSelector:@selector(selectedRange)]) {
                NSRange selRange = [client selectedRange];
                if (selRange.location != NSNotFound || selRange.length != NSNotFound) {
                    _clientCanProvideSelectionInfo = Yes;
                    [self.appDelegate logDebugMessage:@"Client can provide selection info."];
                    [self.appDelegate logDebugMessage:@"selRange.location: %lu", (unsigned long)selRange.location];
                    [self.appDelegate logDebugMessage:@"selRange.length: %lu", (unsigned long)selRange.length];
                    return selRange;
                }
            }
            _clientCanProvideSelectionInfo = No;
            // REVIEW: For now, if the client reports its location as "not found", we're stuck assuming that we're
            // still in the same location in the context. This may be totally untrue, but if the client can't report
            // its location, we have nothing else to go on.
            _clientSelectionCanChangeUnexpectedly = NO;
            [self.appDelegate logDebugMessage:@"Client can NOT provide selection info!"];
            // fall through
        case No:
            return NSMakeRange(NSNotFound, NSNotFound);
        case Yes:
        case Unreliable:
        {
            NSRange selRange = [client selectedRange];
            [self.appDelegate logDebugMessage:@"selRange.location: %lu", (unsigned long)selRange.location];
            return selRange;
        }
    }
}

- (void)updateContextBuffer:(id)sender {
      [self.appDelegate logDebugMessage:@"*** updateContextBuffer ***"];
      [self.appDelegate logDebugMessage:@"sender: %@", sender];

    NSRange selRange = [self getSelectionRangefromClient: sender];

    // Since client can't tell us the actual current position, we assume the previously known location in the context.
    // (It won't matter anyway if the client also fails to report its context.)
    NSUInteger len = (_clientCanProvideSelectionInfo != Yes) ? _previousSelRange.length : selRange.location;
    NSString *preBuffer = [self getLimitedContextFrom:sender at:len];

    // REVIEW: If there is ever a situation where preBuffer gets some text but the client reports its
    // selectedRange as not found, we probably can't reliably assume that the current location is really
    // at the end of the "preBuffer", so maybe we just need to assume no context.
    [self.appDelegate setContextBuffer:preBuffer.length?[NSMutableString stringWithString:preBuffer]:nil];
    _contextOutOfDate = NO;
      [self.appDelegate logDebugMessage:@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}"];
      [self.appDelegate logDebugMessage:@"***"];
    _previousSelRange = selRange;
}

-(NSString *)getLimitedContextFrom:(id)sender at:(NSUInteger) len {
    if (![sender respondsToSelector:@selector(attributedSubstringFromRange:)])
        return nil;

    NSUInteger start = 0;
    if (len > kMaxContext) {
        [self.appDelegate logDebugMessage:@"Limiting context to %lu characters", kMaxContext];
        start = len - kMaxContext;
        len = kMaxContext;
    }

    NSString *preBuffer = [[sender attributedSubstringFromRange:NSMakeRange(start, len)] string];
    [self.appDelegate logDebugMessage:@"preBuffer = \"%@\"", preBuffer ? preBuffer : @"nil"];
    if (preBuffer.length)
      [self.appDelegate logDebugMessage:@"First character: '%x'", [preBuffer characterAtIndex:0]];
    else
      [self.appDelegate logDebugMessage:@"preBuffer has a length of 0"];

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
    // REVIEW: if self.appDelegate.lowLevelEventTap == nil under what circumstances might we be able to safely
    // re-get the context? (Probably clientCanProvideSelectionInfo would need to be true, and it would only
    // actually be useful if client respondsToSelector:@selector(attributedSubstringFromRange:), but even then
    // we'd only want to do it if we have actually moved from our previous location. Otherwise, we wouldn't be
    // able to handle dead keys correctly.)
    if (self.appDelegate.contextChangingEventDetected)
    {
        if (!_contextOutOfDate) {
          [self.appDelegate logDebugMessage:@"Low-level event requires context to be re-retrieved."];
        }
        _contextOutOfDate = YES;
        self.appDelegate.contextChangingEventDetected = NO;
    }

    if (_contextOutOfDate)
        [self updateContextBuffer:client];
}

- (BOOL) handleEventWithKeymanEngine:(NSEvent *)event in:(id) sender {
  NSArray *actions = nil;

  // TODO: remove willDeleteNullChar with core code in place
  if ([self willDeleteNullChar]) {
      [self.appDelegate logDebugMessage:@"willDeleteNullChar = true"];
      return NO;
  }
  else {
      actions = [self processEventWithKeymanEngine:event in:sender];
      if (actions.count == 0) {
        [self checkEventForSentryEasterEgg:event];
        return NO;
      }
  }
  
  return [self applyKeymanCoreActions:actions event:event client:sender];
}

/*
-(void)applyKeymanEngineActions:(NSArray*)actions event: (NSEvent *)event in:(id) sender  {
  BOOL deleteBackPosted = NO;
  
  for (NSDictionary *action in actions) {
      NSString *actionType = [[action allKeys] objectAtIndex:0];
      if ([self.appDelegate debugMode]) {
          NSLog(@"Handling %@ action...", actionType);
          NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
      }

      if ([actionType isEqualToString:Q_STR]) {
          NSString *output = [action objectForKey:actionType];
          if ([self.appDelegate debugMode])
              NSLog(@"output = %@", output);
          if (deleteBackPosted)
          {
              [self appendPendingBuffer:output];
              if ([self.appDelegate debugMode])
                  NSLog(@"pendingBuffer = %@", [self pendingBuffer]);
          }
          else {
              NSUInteger nc = [self.contextBuffer deleteLastNullChars];
              // Each null in the context buffer presumably corresponds to a space we inserted when
              // processing the Q_BACK, which we now need to replace with the text we're inserting.
              if (nc > 0) {
                  NSUInteger pos = [self getSelectionRangefromClient:sender].location;
                  if ([self.appDelegate debugMode]) {
                      NSLog(@"nc = %lu", nc);
                      NSLog(@"pos = %lu", pos);
                  }
                  if (pos >= nc && pos != NSNotFound) {
                      if ([self.appDelegate debugMode]) {
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
          NSLog(@"self.contextBuffer %@", self.contextBuffer);

          [self.contextBuffer deleteLastNullChars];
          NSUInteger dk = [self.contextBuffer deleteLastDeadkeys];
          NSInteger n = [[action objectForKey:actionType] integerValue] - dk;
          NSUInteger dc = [[self.contextBuffer lastNChars:n] deadKeyCount];
          // n is now the number of characters to delete from the context buffer
          // (which could include deadkeys the client doesn't know about).
          [self.contextBuffer deleteLastNChars:n];
          n -= dc;

        NSLog(@"number of characters to delete %ld", (long)n);
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
      }
      else if ([actionType isEqualToString:Q_BEEP]) {
          [[NSSound soundNamed:@"Tink"] play];
      }
      else if ([actionType isEqualToString:Q_SAVEOPT]) {
          NSDictionary *save = [action objectForKey:actionType];
          NSNumber *storeKey = [save allKeys][0];
          NSString *value = [save objectForKey:storeKey];
          [self.appDelegate saveStore:storeKey withValue:value];
      }
  }
}
*/

- (NSArray*) processEventWithKeymanEngine:(NSEvent *)event in:(id) sender {
  NSArray* actions = nil;
  if (self.appDelegate.lowLevelEventTap != nil) {
      NSEvent *eventWithOriginalModifierFlags = [NSEvent keyEventWithType:event.type location:event.locationInWindow modifierFlags:self.appDelegate.currentModifierFlags timestamp:event.timestamp windowNumber:event.windowNumber context:event.context characters:event.characters charactersIgnoringModifiers:event.charactersIgnoringModifiers isARepeat:event.isARepeat keyCode:event.keyCode];
    actions = [self.kme processEvent:eventWithOriginalModifierFlags];
    [self.appDelegate logDebugMessage:@"processEventWithKeymanEngine, using AppDelegate.currentModifierFlags %lu, instead of event.modifiers = %lu", (unsigned long)self.appDelegate.currentModifierFlags, (unsigned long)event.modifierFlags];
  }
  else {
      // Depending on the client app and the keyboard, using the passed-in event as it is should work okay.
      // Keyboards that depend on chirality support will not work. And command-key actions that change the
      // context might go undetected in some apps, resulting in errant behavior for subsequent typing.
    actions = [self.kme processEvent:event];
  }
  [self.appDelegate logDebugMessage:@"processEventWithKeymanEngine, actions = %@", actions];
  return actions;
}

- (void)checkEventForSentryEasterEgg:(NSEvent *)event {
  if (_easterEggForSentry != nil) {
      NSString * kmxName = [[self.kme.kmx filePath] lastPathComponent];
      NSLog(@"Sentry - KMX name: %@", kmxName);
      if ([kmxName isEqualToString:kEasterEggKmxName]) {
          NSUInteger len = [_easterEggForSentry length];
          NSLog(@"Sentry - Processing character(s): %@", [event characters]);
          if ([[event characters] characterAtIndex:0] == [kEasterEggText characterAtIndex:len]) {
              NSString *characterToAdd = [kEasterEggText substringWithRange:NSMakeRange(len, 1)];
              NSLog(@"Sentry - Adding character to Easter Egg code string: %@", characterToAdd);
              [_easterEggForSentry appendString:characterToAdd];
              if ([_easterEggForSentry isEqualToString:kEasterEggText]) {
                  NSLog(@"Sentry - Forcing crash now");
                  [SentrySDK crash];
              }
          }
          else if (len > 0) {
              NSLog(@"Sentry - Clearing Easter Egg code string.");
              [_easterEggForSentry setString:@""];
          }
      }
  }
}


- (void)processUnhandledDeleteBack:(id)client updateEngineContext:(BOOL *)updateEngineContext {
    if ([self.appDelegate debugMode]) {
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
            if ([self.appDelegate debugMode])
                NSLog(@"Processing final posted delete-back...");

            self.willDeleteNullChar = NO;
            if (_legacyMode) {
                if (_pendingBuffer != nil && _pendingBuffer.length > 0) {
                    if ([self.appDelegate debugMode])
                        NSLog(@"Posting special code to tell IM to insert characters from pending buffer.");
                    [self performSelector:@selector(initiatePendingBufferProcessing:) withObject:client afterDelay:0.1];
                }
            }
            else {
                if (!_sourceFromOriginalEvent) {
                    NSLog(@"----- If not legacy mode, _sourceFromOriginalEvent should be retained until original code is posted -----");
                }
                else {
                    if ([self.appDelegate debugMode]) {
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

// handleDeleteBackLowLevel: handles the situation for Delete Back for
// some 'legacy' mode apps such as Adobe apps, because when we inject
// the Delete Back, it is passed through to the app but we never see it
// in our normal handleEvent function. However, other apps, such as Word,
// show the event both in the low level tap and in the normal IM
// handleEvent. Thus, we set a flag after processing a Delete Back here so
// that we don't accidentally process it twice. Note that a Delete Back that
// we handle here should never be passed on to Keyman Engine for transform,
// as it will be part of the output from the transform.
- (BOOL)handleDeleteBackLowLevel:(NSEvent *)event {
    self.ignoreNextDeleteBackHighLevel = NO;
    if(event.keyCode == kVK_Delete && _legacyMode && [self pendingBuffer].length > 0) {
        BOOL updateEngineContext = YES;
        if ([self.appDelegate debugMode]) {
            NSLog(@"legacy: delete-back received, processing");
        }
        [self processUnhandledDeleteBack:self.senderForDeleteBack updateEngineContext: &updateEngineContext];
        self.ignoreNextDeleteBackHighLevel = YES;
    }

    return self.ignoreNextDeleteBackHighLevel;
}

- (BOOL)deleteBack:(NSUInteger)n in:(id) client for:(NSEvent *) event {
    if ([self.appDelegate debugMode])
        NSLog(@"Attempting to back-delete %li characters.", n);
    NSRange selectedRange = [self getSelectionRangefromClient:client];
    NSInteger pos = selectedRange.location;
  
    NSLog(@"delete at position %ld", (long)pos);
    NSLog(@"_legacyMode: %s", _legacyMode?"yes":"no");
    if (!_legacyMode)
        [self deleteBack:n at: pos in: client];
    if (_legacyMode)
        return [self deleteBackLegacy:n at: pos with: selectedRange for: event];

    return NO;
}

- (void)deleteBack:(NSUInteger)n at:(NSUInteger) pos in:(id)client {
    if ([self.appDelegate debugMode]) {
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
                    if ([self.appDelegate debugMode]) {
                        NSLog(@"Client apparently doesn't implement attributedSubstringFromRange. Attempting to get preChar from context...");
                    }
                    if (self.contextBuffer != nil && preCharPos < self.contextBuffer.length) {
                        preChar = [self.contextBuffer substringWithRange:NSMakeRange(preCharPos, 1)];
                    }
                    if (!preChar)
                        break;
                }
                if ([self.appDelegate debugMode])
                    NSLog(@"Testing preChar: %@", preChar);

                if ([regexNonCombiningMark numberOfMatchesInString:preChar options:NSMatchingAnchored range:NSMakeRange(0, 1)] > 0)
                    break;
                if (preCharPos == 0) {
                    if ([self.appDelegate debugMode]) {
                        NSLog(@"Failed to find a base character!");
                    }
                    break;
                }
                if ([self.appDelegate debugMode]) {
                    NSLog(@"Have not yet found a base character. nbrOfPreCharacters = %lu", nbrOfPreCharacters);
                }
            }
            if (preChar) {
                if ([self.appDelegate debugMode]) {
                    NSLog(@"preChar (to insert at %lu) = \"%@\"", preCharPos, preChar);
                }
                [client insertText:preChar replacementRange:NSMakeRange(preCharPos, n+nbrOfPreCharacters)];
            }
            else {
                if ([self.appDelegate debugMode]) {
                    NSLog(@"Switching to legacy mode - client apparently doesn't implement attributedSubstringFromRange and no previous character in context buffer.");
                }
                _legacyMode = YES; // client apparently doesn't implement attributedSubstringFromRange.
            }
        }
        else {
            if ([self.appDelegate debugMode])
                NSLog(@"No previous character to use for replacement - replacing range with space");
            [client insertText:@" " replacementRange:NSMakeRange(pos - n, n)];
            [self.contextBuffer appendNullChar];
        }
    }
}

- (BOOL)deleteBackLegacy:(NSUInteger)n at:(NSUInteger) pos with:(NSRange) selectedRange for:(NSEvent *) event {
    NSLog(@"deleteBackLegacy");
   if (self.contextBuffer != nil && (pos == 0 || pos == NSNotFound)) {
        pos = self.contextBuffer.length + n;
    }

    if ([self.appDelegate debugMode]) {
        NSLog(@"Using Legacy mode.");
        NSLog(@"pos = %lu", pos);
    }

    if (pos >= n) {
        // n is now the number of delete-backs we need to post (plus one more if there is selected text)
        if ([self.appDelegate debugMode]) {
            NSLog(@"Legacy mode: calling postDeleteBacks");
            if (_clientCanProvideSelectionInfo == No || _clientCanProvideSelectionInfo == Unreliable)
                NSLog(@"Cannot trust client to report accurate selection length - assuming no selection.");
        }

        if (_pendingBuffer != nil && [[self pendingBuffer] length] > 0) {
            // We shouldn't be sending out characters before the corresponding Delete Back events are received
            // if this does happen, that's unexpected...
            NSLog(@"Legacy mode: ERROR: did not find expected Delete Back event");
        }

        // Note: If pos is "not found", most likely the client can't accurately report the location. This might be
        // dangerous, but for now let's go ahead and attempt to delete the characters we think should be there.

        if (_clientCanProvideSelectionInfo == Yes && selectedRange.length > 0)
            n++; // First delete-back will delete the existing selection.
        [self postDeleteBacks:n for:event];

        CFRelease(_sourceFromOriginalEvent);
        _sourceFromOriginalEvent = nil;
        return YES;
    }
    return NO;
}

- (void)postDeleteBacks:(NSUInteger)count for:(NSEvent *) event {
    _numberOfPostedDeletesToExpect = count;

    _sourceFromOriginalEvent = CGEventCreateSourceFromEvent([event CGEvent]);

    for (int db = 0; db < count; db++)
    {
        if ([self.appDelegate debugMode]) {
            NSLog(@"Posting a delete (down/up) at kCGHIDEventTap.");
        }
        [self.appDelegate postKeyboardEventWithSource:_sourceFromOriginalEvent code:kVK_Delete postCallback:^(CGEventRef eventToPost) {
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

    if ([self.appDelegate debugMode]) {
        if (code == kProcessPendingBuffer) {
            NSLog(@"Posting code to tell Keyman to process characters in pending buffer.");
        }
        else {
            NSLog(@"Posting a keypress (down/up) to the 'front process' for the %hu key.", code);
        }
    }

    [self.appDelegate postKeyboardEventWithSource:source code:code postCallback:^(CGEventRef eventToPost) {
        CGEventPostToPSN((ProcessSerialNumberPtr)&psn, eventToPost);
    }];
}

//MARK: Core-related key processing

- (void)checkTextApiCompliance:(id)client {
  // if TextApiCompliance object is null or stale,
  // then create a new one for the current application
  
  if ((self.apiCompliance == nil) || (![self.apiCompliance.clientApplicationId isEqualTo:self.clientApplicationId])) {
    self.apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:self.clientApplicationId];
    [self.appDelegate logDebugMessage:@"KMInputMethodHandler initWithClient checkTextApiCompliance: %@", _apiCompliance];
  } else if (self.apiCompliance.isComplianceUncertain) {
    // if it is valid but compliance is uncertain, then test it
    [self.apiCompliance testCompliance:client];
  }
}

- (void) checkIfContextChangingEventDetected {
  if (self.appDelegate.contextChangingEventDetected)
  {
    if (!self.cachedContext.isInvalid) {
      [self.appDelegate logDebugMessage:@"Low-level event has invalidated the context."];
      [self.cachedContext invalidateContext];
    }
    self.appDelegate.contextChangingEventDetected = NO;
  }
}

// handleEvent implementation for core event processing
- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
  [self.appDelegate logDebugMessage:@"handleEvent event = %@", event];

  [self checkTextApiCompliance:sender];
  
  // mouse movement requires that the context be invalidated
  [self checkIfContextChangingEventDetected];

  if (event.type == NSKeyDown) {
    [self loadContextIfInvalid:event forClient:sender];
    
    // indicates that our generated backspace event(s) are consumed
    // and we can insert text that followed the backspace(s)
    if (event.keyCode == kKeymanEventKeyCode) {
      [self insertQueuedText: event client:sender];
      return YES;
    }

    if(event.keyCode == kVK_Delete && self.generatedBackspaceCount > 0) {
      self.generatedBackspaceCount--;
      [self.cachedContext deleteLastCodePoint];
      [self.appDelegate logDebugMessage:@"handleEvent KVK_Delete, reducing generatedBackspaceCount to %d ", self.generatedBackspaceCount];
      return NO;
    }
  }
    if (event.type == NSEventTypeFlagsChanged) {
        // Mark the context as out of date for the command keys
        switch([event keyCode]) {
            case kVK_RightCommand:
            case kVK_Command:
            [self.cachedContext invalidateContext];
                break;
            case kVK_Shift:
            case kVK_RightShift:
            case kVK_CapsLock:
            case kVK_Option:
            case kVK_RightOption:
            case kVK_Control:
            case kVK_RightControl:
                break;
        }
        return NO;
    }

    if ((event.modifierFlags & NSEventModifierFlagCommand) == NSEventModifierFlagCommand) {
        [self handleCommand:event];
        return NO; // let the client app handle all Command-key events.
    }

    BOOL handled = [self handleEventWithKeymanEngine:event in: sender];
    if (event.type == NSKeyDown) {
      [self.appDelegate logDebugMessage:@"event, keycode: %u, characters='%@' handled by KeymanEngine: %@", event.keyCode, event.characters, handled?@"yes":@"no"];
    }

    return handled;
}

-(void)loadContextIfInvalid:(NSEvent *)event forClient:(id) client {
  NSString *contextString = nil;
  NSAttributedString *attributedString = nil;

  if ([self.cachedContext isInvalid]) {
    if (self.apiCompliance.canReadText) {
      NSRange selectionRange = [client selectedRange];
      NSRange contextRange = NSMakeRange(0, selectionRange.location);
      attributedString = [client attributedSubstringFromRange:contextRange];
    }
    
    if(attributedString == nil) {
      contextString = @"";
    } else {
      contextString = attributedString.string;
    }
    
    [self.appDelegate logDebugMessage:@"loadContext, new context='%@'", contextString];
    [self.cachedContext resetContext:contextString];
    [self.kme setCoreContext:contextString];
    }
}

/*
 * Returns YES if we have applied an event to the client.
 * If NO, then (passthrough case) we are instructing the OS to apply the original event.
 * If any of the returned actions result in a change to the client, then return YES.
 * Changes to the client may be executed with insertText or by generating a KeyDown event.
 * For a CharacterAction, return YES after applying the character to the client.
 * For a BackspaceAction:
 * -return NO if this is the only action (besides End)
 * -return YES after generating a backspace event
 */
-(BOOL)applyKeymanCoreActions:(NSArray*)actions event: (NSEvent*)event client:(id) client {
  [self.appDelegate logDebugMessage:@"applyKeymanCoreActions invoked, actions.count = %lu ", (unsigned long)actions.count];
  
  KMCoreActionHandler *actionHandler = [[KMCoreActionHandler alloc] initWithActions:(NSArray*)actions keyCode: event.keyCode];
  KMActionHandlerResult *result = actionHandler.handleActions;

  for (KMActionOperation *operation in [result.operations objectEnumerator]) {
    if (operation.isForCompositeAction) {
      [self executeCompositeOperation:operation keyDownEvent:event client:client];
    } else {
      [self executeSimpleOperation:operation keyDownEvent:event];
    }
  }

  return result.handledEvent;
}

/**
 * If we need to do something in response to a single action, then do it here.
 * Most of the interesting work is done in executeCompositeOperation, and
 * much of the other work is done when updating the context.
 */
-(void)executeSimpleOperation:(KMActionOperation*)operation keyDownEvent:(nonnull NSEvent *)event {
  CoreAction *action = operation.action;
  if (action != nil) {
    [self.appDelegate logDebugMessage:@"KXMInputMethodHandler executeSimpleOperation: %@", operation];
  }
  switch(action.actionType) {
    case AlertAction:
      NSBeep();
      break;
    case PersistOptionAction:
      if(action.key && action.value) {
        [self.appDelegate logDebugMessage:@"PersistOptionAction calling writePersistedOptions, key: %@, value: %@", action.key, action.value];
        [self.appDelegate writePersistedOptions:action.key withValue:action.value];
      }
      else {
        [self.appDelegate logDebugMessage:@"Invalid values for PersistOptionAction, not writing to UserDefaults, key: %@, value: %@", action.key, action.value];
      }
      break;
    case InvalidateContextAction:
      [self.cachedContext invalidateContext];
      break;
    case CapsLockAction: {
      //TODO: handle this somewhere
      break;
    }
    default:
      break;
  }

  [self.cachedContext applyAction:operation.action keyDownEvent:event];
}

-(void)executeCompositeOperation:(KMActionOperation*)operation keyDownEvent:(nonnull NSEvent *)event client:(id) client {
  [self.appDelegate logDebugMessage:@"KXMInputMethodHandler executeCompositeOperation, composite operation: %@", operation];
  
  if ((operation.isTextOnlyScenario)) {
    [self.appDelegate logDebugMessage:@"KXMInputMethodHandler executeCompositeOperation, text only scenario"];
    [self insertAndReplaceTextForOperation:operation client:client];
  }
      
  if (operation.isBackspaceOnlyScenario) {
    [self.appDelegate logDebugMessage:@"KXMInputMethodHandler executeCompositeOperation, backspace only scenario"];
    [self sendEventsForOperation: event actionOperation:operation];
  }

  if ((operation.isTextAndBackspaceScenario)) {
    if (self.apiCompliance.mustBackspaceUsingEvents) {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler executeCompositeOperation, text and backspace scenario with events"];
     [self sendEventsForOperation: event actionOperation:operation];
    } else {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler executeCompositeOperation, text and backspace scenario with insert API"];
      // directly insert text and handle backspaces by using replace
      [self insertAndReplaceTextForOperation:operation client:client];
    }
  }
}

-(void)insertAndReplaceTextForOperation:(KMActionOperation*)operation client:(id) client {
  [self insertAndReplaceText:operation.textToInsert backspaceCount:operation.backspaceCount client:client];
}

/**
 * This directly inserts text and applies backspaces for the operation by replacing existing text with the new text.
 * Because this method depends on the selectedRange API which is not implemented correctly for some client applications,
 * this method can only be used if approved by TextApiCompliance
 */
-(void)insertAndReplaceText:(NSString *)text backspaceCount:(int) replacementCount client:(id) client {

  [self.appDelegate logDebugMessage:@"KXMInputMethodHandler insertAndReplaceText: %@ replacementCount: %d", text, replacementCount];
  int actualReplacementCount = 0;
  NSRange replacementRange;
  NSRange notFoundRange = NSMakeRange(NSNotFound, NSNotFound);
  
  if (replacementCount > 0) {
    NSRange selectionRange = [client selectedRange];
    
    // make sure we have room to apply backspaces from current location
    actualReplacementCount = MIN(replacementCount, (int)selectionRange.location);
    
    // log this: it is a sign that we are out of sync
    if (actualReplacementCount < replacementCount) {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler insertAndReplaceText, replacement of %d characters limited by start of context to actual count: %d", replacementCount, actualReplacementCount];
    }
    
    if (actualReplacementCount > 0) {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler insertAndReplaceText, actualReplacementCount=%d, selectionRange.location=%lu", actualReplacementCount, selectionRange.location];
      
      replacementRange = NSMakeRange(selectionRange.location-actualReplacementCount, replacementCount);
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler insertAndReplaceText, insertText %@ in replacementRange.start=%lu, replacementRange.length=%lu", text, replacementRange.location, replacementRange.length];
    } else {
      replacementRange = notFoundRange;
    }
  } else {
    replacementRange = notFoundRange;
  }
  [client insertText:text replacementRange:replacementRange];
  [self.cachedContext replaceSubstring:text count:actualReplacementCount];
  if (self.apiCompliance.isComplianceUncertain) {
    [self.apiCompliance testComplianceAfterInsert:client];
  }
}


-(void)sendEventsForOperation: (NSEvent *)event actionOperation:(KMActionOperation*)operation {
  [self.appDelegate logDebugMessage:@"sendEventsForOperation backspaceCount: %d, textToInsert: %@, hasAccessibility = %@", operation.backspaceCount, operation.textToInsert, [PrivacyConsent.shared checkAccessibility]?@"YES":@"***NO!***"];
  
  if (operation.hasBackspaces) {
    for (int i = 0; i < operation.backspaceCount; i++)
    {
      self.generatedBackspaceCount++;
      [self.keySender sendBackspaceforSourceEvent:event];
    }
  }
  
  if (operation.hasTextToInsert) {
    self.queuedText = operation.textToInsert;
    [self.keySender sendKeymanKeyCodeForEvent:event];
  }
}

-(void)insertQueuedText: (NSEvent *)event client:(id) client  {
  if (self.queuedText.length> 0) {
    [self.appDelegate logDebugMessage:@"insertQueuedText, inserting %@", self.queuedText];
    [self insertAndReplaceText:self.queuedText backspaceCount:0 client:client];
  } else {
    [self.appDelegate logDebugMessage:@"handleQueuedText called but no text to insert"];
  }
}

@end
