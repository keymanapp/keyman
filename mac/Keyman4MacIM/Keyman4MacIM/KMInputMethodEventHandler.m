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
#import "KeySender.h"
#import "TextApiCompliance.h"

@import Sentry;

@interface KMInputMethodEventHandler ()
@property (nonatomic, retain) KeySender* keySender;
@property (nonatomic, retain) TextApiCompliance* apiCompliance;
@property NSUInteger generatedBackspaceCount;
@property BOOL backspaceHandledLowLevel;
@property (nonatomic, retain) NSString* clientApplicationId;
@property NSString *queuedText;
@property BOOL contextChanged;
@end

@implementation KMInputMethodEventHandler

const CGKeyCode kProcessPendingBuffer = 0xFF;
const NSUInteger kMaxContext = 80;
const NSTimeInterval kQueuedTextEventDelayInSeconds = 0.1;

//_pendingBuffer contains text that is ready to be sent to the client when all delete-backs are finished.
NSMutableString* _pendingBuffer;
CGKeyCode _keyCodeOfOriginalEvent;
CGEventSourceRef _sourceFromOriginalEvent = nil;
CGEventSourceRef _sourceForGeneratedEvent = nil;
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
    if (_generatedBackspaceCount > 0 || (_queuedText != nil && _queuedText.length > 0) ||
        _keyCodeOfOriginalEvent != 0 || _sourceFromOriginalEvent != nil)
    {
        if ([self.appDelegate debugMode]) {
            NSLog(@"ERROR: new app activated before previous app finished processing pending events!");
            NSLog(@"  _generatedBackspaceCount = %lu", _generatedBackspaceCount);
            NSLog(@"  _queuedText = \"%@\"", _queuedText == nil ? @"(NIL)" : (NSString*)[self queuedText]);
            NSLog(@"  _keyCodeOfOriginalEvent = %hu", _keyCodeOfOriginalEvent);
        }
        _keyCodeOfOriginalEvent = 0;
        _generatedBackspaceCount = 0;
        _queuedText = nil;
    }
}

- (void)dealloc {
    if (_sourceFromOriginalEvent != nil) {
        CFRelease(_sourceFromOriginalEvent);
        _sourceFromOriginalEvent = nil;
    }
  if (_sourceForGeneratedEvent != nil) {
    CFRelease(_sourceForGeneratedEvent);
    _sourceForGeneratedEvent = nil;
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
    [self.appDelegate logDebugMessage:@"KMInputMethodEventHandler handleCommand, event type=%@, must refresh context", event.type];
    self.contextChanged = YES;
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

- (BOOL) handleEventWithKeymanEngine:(NSEvent *)event in:(id) sender {
  [self.appDelegate logDebugMessage:@"handleEventWithKeymanEngine, event = %@", event];
  CoreKeyOutput *output = nil;

  output = [self processEventWithKeymanEngine:event in:sender];
  if (output == nil) {
      [self checkEventForSentryEasterEgg:event];
      return NO;
  }
  
  return [self applyKeymanCoreActions:output event:event client:sender];
}

- (CoreKeyOutput*) processEventWithKeymanEngine:(NSEvent *)event in:(id) sender {
  CoreKeyOutput* coreKeyOutput = nil;
  if (self.appDelegate.lowLevelEventTap != nil) {
      NSEvent *eventWithOriginalModifierFlags = [NSEvent keyEventWithType:event.type location:event.locationInWindow modifierFlags:self.appDelegate.currentModifierFlags timestamp:event.timestamp windowNumber:event.windowNumber context:event.context characters:event.characters charactersIgnoringModifiers:event.charactersIgnoringModifiers isARepeat:event.isARepeat keyCode:event.keyCode];
      coreKeyOutput = [self.kme processEvent:eventWithOriginalModifierFlags];
      [self.appDelegate logDebugMessage:@"processEventWithKeymanEngine, using AppDelegate.currentModifierFlags %lu, instead of event.modifiers = %lu", (unsigned long)self.appDelegate.currentModifierFlags, (unsigned long)event.modifierFlags];
  }
  else {
      // Depending on the client app and the keyboard, using the passed-in event as it is should work okay.
      // Keyboards that depend on chirality support will not work. And command-key actions that change the
      // context might go undetected in some apps, resulting in errant behavior for subsequent typing.
      coreKeyOutput = [self.kme processEvent:event];
  }
  [self.appDelegate logDebugMessage:@"processEventWithKeymanEngine, coreKeyOutput = %@", coreKeyOutput];
  return coreKeyOutput;
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

/**
 handleBackspace: handles generated for a subset of non-compliant apps (such as Adobe apps) that do
 not support replacing text with the insertText API but also intercept events so that we do not see them in handleEvent.
 Other apps, such as Word, show the event both in the low
 level EventTap and in the normal IM handleEvent. Thus, we set a flag after
 processing a backspace here so that we do not process it again in handleEvent.
 Note that a backspace that we handle here should never be passed on to Keyman
 Core for processing, as it is already the result of processing.
*/
- (void)handleBackspace:(NSEvent *)event {
  [self.appDelegate logDebugMessage:@"KMInputMethodEventHandler handleBackspace, event = %@", event];
  NSLog(@"  backspaceHandledLowLevel: %d, generatedBackspaceCount: %lu", _backspaceHandledLowLevel, (unsigned long)_generatedBackspaceCount);
  self.backspaceHandledLowLevel = NO;

  if (self.generatedBackspaceCount > 0) {
    self.generatedBackspaceCount--;
    self.backspaceHandledLowLevel = YES;
    
    // if we just encountered the last backspace, then send event to insert queued text
    if (self.generatedBackspaceCount == 0) {
      [self performSelector:@selector(triggerInsertQueuedText:) withObject:event afterDelay:kQueuedTextEventDelayInSeconds];
    }
  }
}

- (void)triggerInsertQueuedText:(NSEvent *)event {
  [self.appDelegate logDebugMessage:@"KMInputMethodEventHandler triggerInsertQueuedText"];
  [self.keySender sendKeymanKeyCodeForEvent:event];
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

- (void) handleContextChangedByLowLevelEvent {
  if (self.appDelegate.contextChangedByLowLevelEvent) {
    if (!self.contextChanged) {
      [self.appDelegate logDebugMessage:@"Low-level event has changed the context."];
      self.contextChanged = YES;
    }
    self.appDelegate.contextChangedByLowLevelEvent = NO;
  }
}

// handleEvent implementation for core event processing
- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
  BOOL handled = NO;
  [self.appDelegate logDebugMessage:@"handleEvent event = %@", event];

  [self checkTextApiCompliance:sender];
  
  // mouse movement requires that the context be invalidated
  [self handleContextChangedByLowLevelEvent];

  if (event.type == NSKeyDown) {
    // indicates that our generated backspace event(s) are consumed
    // and we can insert text that followed the backspace(s)
    if (event.keyCode == kKeymanEventKeyCode) {
      [self.appDelegate logDebugMessage:@"handleEvent, handling kKeymanEventKeyCode"];
      [self insertQueuedText: event client:sender];
      return YES;
    }

    // for some apps, handleEvent will not be called for the generated backspace
    // but if it is, we need to let it pass through rather than process again in core
    if((event.keyCode == kVK_Delete) && (self.backspaceHandledLowLevel)) {
      [self.appDelegate logDebugMessage:@"handleEvent, allowing generated backspace to pass through"];
      self.backspaceHandledLowLevel = NO;
      // return NO to pass through to client app
      return NO;
    }
  }
  
  if (event.type == NSEventTypeFlagsChanged) {
    [self handleFlagsChangedEvent:[event keyCode]];
    return NO;
  }

  if ((event.modifierFlags & NSEventModifierFlagCommand) == NSEventModifierFlagCommand) {
    [self handleCommand:event];
    return NO; // let the client app handle all Command-key events.
  }

  if (event.type == NSKeyDown) {
    [self reportContext:event forClient:sender];
    handled = [self handleEventWithKeymanEngine:event in: sender];
  }

  [self.appDelegate logDebugMessage:@"event, keycode: %u, characters='%@' handled = %@", event.keyCode, event.characters, handled?@"yes":@"no"];
  return handled;
}

-(void)handleFlagsChangedEvent:(short)keyCode {
  // Mark the context as out of date for the command keys
  switch(keyCode) {
    case kVK_RightCommand:
    case kVK_Command:
      self.contextChanged = YES;
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
}

/**
 * Called for every keystroke.
 * If we can read the context, send it to Keyman Core.
 * For non-compliant apps, we cannot read the context.
 * In the non-compliant app case, if the context has changed then clear it in Core.
 */
-(void)reportContext:(NSEvent *)event forClient:(id) client {
  NSString *contextString = nil;

  // if we can read the text, then get the context and send it to Core
  // we do this whether the context has changed or not
  if (self.apiCompliance.canReadText) {
    contextString = [self readContext:event forClient:client];
    [self.appDelegate logDebugMessage:@"reportContext, setting new context='%@' for compliant app (if needed)", contextString];
    [self.kme setCoreContextIfNeeded:contextString];
  } else if (self.contextChanged) {
  // we cannot read the text but know the context has changed, so we must clear it
    [self.appDelegate logDebugMessage:@"reportContext, clearing context for non-compliant app"];
    [self.kme clearCoreContext];
  }
  
  // the context is now current, reset the flag
  self.contextChanged = NO;
}

-(NSString*)readContext:(NSEvent *)event forClient:(id) client {
  NSString *contextString = nil;
  NSAttributedString *attributedString = nil;
  
  // if we can read the text, then get the context
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
  return contextString;
}

/**
 * Returns NO if we have not applied an event to the client or if Keyman Core determines that we should emit the keystroke
 */
-(BOOL)applyKeymanCoreActions:(CoreKeyOutput*)output event: (NSEvent*)event client:(id) client {
    [self.appDelegate logDebugMessage:@"   *** InputMethodEventHandler applyKeymanCoreActions: output = %@ ", output];

    BOOL handledEvent = [self applyKeyOutputToTextInputClient:output keyDownEvent:event client:client];
    [self applyNonTextualOutput:output];

    // if output from Keyman Core indicates to emit the keystroke,
    // then return NO, so that the OS still handles the event
    if (handledEvent && output.emitKeystroke) {
        [self.appDelegate logDebugMessage:@"   *** InputMethodEventHandler applyKeymanCoreActions: emit keystroke true, returning false for handledEvent"];
        handledEvent = NO;
    }
    
    return handledEvent;
}

-(BOOL)applyKeyOutputToTextInputClient:(CoreKeyOutput*)output keyDownEvent:(nonnull NSEvent *)event client:(id) client {
  BOOL handledEvent = YES;
  
  if (output.isInsertOnlyScenario) {
    [self.appDelegate logDebugMessage:@"KXMInputMethodHandler applyOutputToTextInputClient, insert only scenario"];
    [self insertAndReplaceTextForOutput:output client:client];
  } else if (output.isDeleteOnlyScenario) {
    if ((event.keyCode == kVK_Delete) && output.codePointsToDeleteBeforeInsert == 1) {
      // let the delete pass through in the original event rather than sending a new delete
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler applyOutputToTextInputClient, delete only scenario"];
      handledEvent = NO;
    } else {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler applyOutputToTextInputClient, delete only scenario"];
      [self sendEvents:event forOutput:output];
    }
  } else if (output.isDeleteAndInsertScenario) {
    if (self.apiCompliance.mustBackspaceUsingEvents) {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler applyOutputToTextInputClient, delete and insert scenario with events"];
      [self sendEvents:event forOutput:output];
    } else {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler applyOutputToTextInputClient, delete and insert scenario with insert API"];
      // directly insert text and handle backspaces by using replace
      [self insertAndReplaceTextForOutput:output client:client];
    }
  }
  
  return handledEvent;
}

-(void)applyNonTextualOutput:(CoreKeyOutput*)output {
  [self persistOptions:output.optionsToPersist];
  
  [self applyCapsLock:output.capsLockState];
  
  if (output.alert) {
    NSBeep();
  }
}

-(void) persistOptions:(NSDictionary*)options{
  if(options.count>0) {
    for(NSString *key in options) {
      NSString *value = [options objectForKey:key];
      if(key && value) {
        [self.appDelegate logDebugMessage:@"applyNonTextualOutput calling writePersistedOptions, key: %@, value: %@", key, value];
        [self.appDelegate writePersistedOptions:key withValue:value];
      }
      else {
        [self.appDelegate logDebugMessage:@"applyNonTextualOutput, invalid values in optionsToPersist, not writing to UserDefaults, key: %@, value: %@", key, value];
      }
    }
  }
}

-(void) applyCapsLock:(CapsLockState)capsLockState {
  switch(capsLockState) {
    case On:
      //TODO: handle this
      break;
    case Off:
      //TODO: handle this
      break;
    case Unchanged:
      // do nothing
      break;
  }
}

-(void)insertAndReplaceTextForOutput:(CoreKeyOutput*)output client:(id) client {
  [self insertAndReplaceText:output.textToInsert deleteCount:(int)output.codePointsToDeleteBeforeInsert client:client];
}

/**
 * This directly inserts text and applies backspaces for the operation by replacing existing text with the new text.
 * Because this method depends on the selectedRange API which is not implemented correctly for some client applications,
 * this method can only be used if approved by TextApiCompliance
 */
-(void)insertAndReplaceText:(NSString *)text deleteCount:(int) replacementCount client:(id) client {

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
  if (self.apiCompliance.isComplianceUncertain) {
    [self.apiCompliance testComplianceAfterInsert:client];
  }
}

-(void)sendEvents:(NSEvent *)event forOutput:(CoreKeyOutput*)output {
  [self.appDelegate logDebugMessage:@"sendEvents called"];
  
  _sourceForGeneratedEvent = CGEventCreateSourceFromEvent([event CGEvent]);

  self.generatedBackspaceCount = output.codePointsToDeleteBeforeInsert;
  
  if (output.hasCodePointsToDelete) {
    for (int i = 0; i < output.codePointsToDeleteBeforeInsert; i++)
    {
      [self.appDelegate logDebugMessage:@"sendEvents queueing backspace"];
      [self.keySender sendBackspaceforEventSource:_sourceForGeneratedEvent];
    }
  }
  
  if (output.hasTextToInsert) {
    [self.appDelegate logDebugMessage:@"sendEvents, queueing text to insert: %@", output.textToInsert];
    
    self.queuedText = output.textToInsert;
  }
}

-(void)insertQueuedText: (NSEvent *)event client:(id) client  {
  if (self.queuedText.length> 0) {
    [self.appDelegate logDebugMessage:@"insertQueuedText, inserting %@", self.queuedText];
    [self insertAndReplaceText:self.queuedText deleteCount:0 client:client];
    self.queuedText = nil;
  } else {
    [self.appDelegate logDebugMessage:@"handleQueuedText called but no text to insert"];
  }
}

@end
