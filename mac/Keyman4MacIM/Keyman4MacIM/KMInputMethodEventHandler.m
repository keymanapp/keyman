//
//  KMInputMethodEventHandler.m
//  Keyman4MacIM
//
//  Created by Tom Bogle on 11/22/17.
//  Copyright © 2017-2018 SIL International. All rights reserved.
//
#import "KMInputMethodEventHandler.h"
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>
#import <Carbon/Carbon.h> /* For kVK_ constants. */
#import "KeySender.h"
#import "TextApiCompliance.h"
@import Sentry;

@interface KMInputMethodEventHandler ()
@property (nonatomic, retain) KeySender* keySender;
@property (nonatomic, retain) TextApiCompliance* apiCompliance;
@property NSUInteger generatedBackspaceCount;
@property NSUInteger lowLevelBackspaceCount;
@property (nonatomic, retain) NSString* clientApplicationId;
@property NSString *queuedText;
@property BOOL contextChanged;
@end

@implementation KMInputMethodEventHandler

const CGKeyCode kProcessPendingBuffer = 0xFF;
const NSUInteger kMaxContext = 80;
const NSTimeInterval kQueuedTextEventDelayInSeconds = 0.1;

CGKeyCode _keyCodeOfOriginalEvent;
CGEventSourceRef _sourceFromOriginalEvent = nil;
CGEventSourceRef _sourceForGeneratedEvent = nil;
NSMutableString* _easterEggForSentry = nil;
NSString* const kEasterEggText = @"Sentry force now";
NSString* const kEasterEggKmxName = @"EnglishSpanish.kmx";

// This is the public initializer.
- (instancetype)initWithClient:(NSString *)clientAppId client:(id) sender {
  _keySender = [[KeySender alloc] init];
  _clientApplicationId = clientAppId;
  _generatedBackspaceCount = 0;
  _lowLevelBackspaceCount = 0;
  _queuedText = nil;
  
  // In Xcode, if Keyman is the active IM and is in "debugMode" and "English plus Spanish" is 
  // the current keyboard and you type "Sentry force now", it will force a simulated crash to 
  // test reporting to sentry.keyman.com
  if ([self.appDelegate debugMode] && [clientAppId isEqual: @"com.apple.dt.Xcode"]) {
    NSLog(@"Sentry - Preparing to detect Easter egg.");
    _easterEggForSentry = [[NSMutableString alloc] init];
  }
  else
    _easterEggForSentry = nil;
  
  return self;
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
  // is that some non-compliant apps can't get their context at all. This can be overridden so that
  // non-compliant apps can mitigate this problem as appropriate.
  [self.appDelegate logDebugMessage:@"KMInputMethodEventHandler handleCommand, event type=%@, must refresh context", event.type];
  self.contextChanged = YES;
}

- (KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (KMEngine *)kme {
  return self.appDelegate.kme;
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
    NSEvent *eventWithOriginalModifierFlags = [NSEvent keyEventWithType:event.type location:event.locationInWindow modifierFlags:self.appDelegate.currentModifierFlags timestamp:event.timestamp windowNumber:event.windowNumber context:[NSGraphicsContext currentContext] characters:event.characters charactersIgnoringModifiers:event.charactersIgnoringModifiers isARepeat:event.isARepeat keyCode:event.keyCode];
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
 handleBackspace: handles generated backspaces for a subset of non-compliant apps (such as Adobe apps) that do
 not support replacing text with the insertText API but also intercept events so that we do not see them in handleEvent.
 Other apps, such as Word, show the event both in the low
 level EventTap and in the normal IM handleEvent. Thus, we set a flag after
 processing a backspace here so that we do not process it again in handleEvent.
 Note that a backspace that we handle here should never be passed on to Keyman
 Core for processing, as it is already the result of processing.
 */
- (void)handleBackspace:(NSEvent *)event {
  [self.appDelegate logDebugMessage:@"KMInputMethodEventHandler handleBackspace, event = %@", event];
  
  if (self.generatedBackspaceCount > 0) {
    self.generatedBackspaceCount--;
    self.lowLevelBackspaceCount++;
    
    // if we just encountered the last backspace, then send event to insert queued text
    if ((self.generatedBackspaceCount == 0) && (self.queuedText.length > 0)) {
      [self performSelector:@selector(triggerInsertQueuedText:) withObject:event afterDelay:kQueuedTextEventDelayInSeconds];
    }
  }
}

- (void)triggerInsertQueuedText:(NSEvent *)event {
  [self.appDelegate logDebugMessage:@"KMInputMethodEventHandler triggerInsertQueuedText"];
  [self.keySender sendKeymanKeyCodeForEvent:event];
}

//MARK: Core-related key processing

- (void)checkTextApiCompliance:(id)client {
  // If the TextApiCompliance object is nil or the app or the context has changed,
  // then create a new object for the current application and context.
  // The text api compliance may vary from one text field to another of the same app.
  
  if ((self.apiCompliance == nil) ||
      (![self.apiCompliance.clientApplicationId isEqualTo:self.clientApplicationId]) ||
      self.contextChanged) {
    self.apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:self.clientApplicationId];
    [self.appDelegate logDebugMessage:@"KMInputMethodHandler initWithClient checkTextApiCompliance: %@", _apiCompliance];
  } else if (self.apiCompliance.isComplianceUncertain) {
    // We have a valid TextApiCompliance object, but compliance is uncertain, so test it
    [self.apiCompliance checkCompliance:client];
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
  
  if (event.type == NSEventTypeKeyDown) {
    // indicates that our generated backspace event(s) are consumed
    // and we can insert text that followed the backspace(s)
    if (event.keyCode == kKeymanEventKeyCode) {
      [self.appDelegate logDebugMessage:@"handleEvent, handling kKeymanEventKeyCode"];
      [self insertQueuedText: event client:sender];
      return YES;
    }
    
    // for some apps, handleEvent will not be called for the generated backspace
    // but if it is, we need to let it pass through rather than process again in core
    if((event.keyCode == kVK_Delete) && (self.lowLevelBackspaceCount > 0)) {
      [self.appDelegate logDebugMessage:@"handleEvent, allowing generated backspace to pass through"];
      self.lowLevelBackspaceCount--;
      
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
  
  if (event.type == NSEventTypeKeyDown) {
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
  NSString *contextString = @"";
  NSAttributedString *attributedString = nil;
  
  // if we can read the text, then get the context for up to kMaxContext characters
  if (self.apiCompliance.canReadText) {
    NSRange selectionRange = [client selectedRange];
    NSUInteger contextLength = MIN(kMaxContext, selectionRange.location);
    NSUInteger contextStart = selectionRange.location - contextLength;
    
    if (contextLength > 0) {
      [self.appDelegate logDebugMessage:@"   *** InputMethodEventHandler readContext, %d characters", contextLength];
      NSRange contextRange = NSMakeRange(contextStart, contextLength);
      attributedString = [client attributedSubstringFromRange:contextRange];
      
      // adjust string in case that we receive half of a surrogate pair at context start
      // the API appears to always return a full code point, but this could vary by app
      if (attributedString.length > 0) {
        if (CFStringIsSurrogateLowCharacter([attributedString.string characterAtIndex:0])) {
          [self.appDelegate logDebugMessage:@"   *** InputMethodEventHandler readContext, first char is low surrogate, reducing context by one character"];
          contextString = [attributedString.string substringFromIndex:1];
        } else {
          contextString = attributedString.string;
        }
      }
    }
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
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler applyOutputToTextInputClient, delete only scenario with passthrough"];
      handledEvent = NO;
    } else {
      [self.appDelegate logDebugMessage:@"KXMInputMethodHandler applyOutputToTextInputClient, delete only scenario"];
      [self sendEvents:event forOutput:output];
    }
  } else if (output.isDeleteAndInsertScenario) {
    // TODO: fix issue #10246
    /*
     // needs further testing to fix backspace bug in google docs (without breaking other apps)
     // assume that all non-compliant apps which require backspaces apply an extra backspace if the original event is a backspace
     if ((self.apiCompliance.mustBackspaceUsingEvents) && (event.keyCode == kVK_Delete)) {
     output.codePointsToDeleteBeforeInsert--;
     os_log_info(keymanLog, "isDeleteAndInsertScenario, after delete pressed subtracting one backspace to reach %d and insert text '%{public}@'", output.codePointsToDeleteBeforeInsert, output.textToInsert);
     if (output.codePointsToDeleteBeforeInsert == 0) {
     // no backspace events needed
     [self insertAndReplaceTextForOutput:output client:client];
     } else {
     // still need at least one backspace event
     [self sendEvents:event forOutput:output];
     }
     }
     */
    
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

-(void) applyCapsLock:(CapsLockState)capsLockState {
  switch(capsLockState) {
    case On:
      //TODO: handle this, Issue #10244
      break;
    case Off:
      //TODO: handle this, Issue #10244
      break;
    case Unchanged:
      // do nothing
      break;
  }
}

-(void)insertAndReplaceTextForOutput:(CoreKeyOutput*)output client:(id) client {
  [self insertAndReplaceText:output.textToInsert deleteCount:(int)output.codePointsToDeleteBeforeInsert toReplace:output.textToDelete client:client];
}

/**
 * This directly inserts text and applies backspaces for the operation by replacing existing text with the new text.
 * Because this method depends on the selectedRange API which is not implemented correctly for some client applications,
 * this method can only be used if approved by TextApiCompliance
 */
-(void)insertAndReplaceText:(NSString *)text deleteCount:(int) replacementCount toReplace:(NSString*)textToDelete client:(id) client {
  [self.appDelegate logDebugMessage:@"KXMInputMethodHandler insertAndReplaceText: %@ replacementCount: %d", text, replacementCount];
  NSRange selectionRange = [client selectedRange];
  NSRange replacementRange = [self calculateInsertRangeForDeletedText:textToDelete selectionRange:selectionRange];
  
  [client insertText:text replacementRange:replacementRange];
  if (self.apiCompliance.isComplianceUncertain) {
    [self.apiCompliance checkComplianceAfterInsert:client delete:textToDelete insert:text];
  }
}

/*
 * Calculates the range where text will be inserted and replace existing text.
 * Returning {NSNotFound, NSNotFound} for range signifies to insert at current location without replacement.
 */
-(NSRange) calculateInsertRangeForDeletedText:(NSString*)textToDelete selectionRange:(NSRange) selection {
  NSRange notFoundRange = NSMakeRange(NSNotFound, NSNotFound);
  NSRange resultingReplacementRange = NSMakeRange(0, 0);
  
  // range = current location if either
  // 1. no text has been designated to be deleted, or
  // 2. the length of the text to delete is impossible (greater than the location)
  if ((textToDelete.length == 0) || (textToDelete.length > selection.location)) {
    // magic value of {NSNotFound, NSNotFound} passed to insertText means "do not replace, insert in current location"
    return notFoundRange;
  } else {
    resultingReplacementRange.length = textToDelete.length + selection.length;
    resultingReplacementRange.location = selection.location - textToDelete.length;
  }
  
  return resultingReplacementRange;
}

-(void)sendEvents:(NSEvent *)event forOutput:(CoreKeyOutput*)output {
  [self.appDelegate logDebugMessage:@"sendEvents called, output = %@", output];
  
  _sourceForGeneratedEvent = CGEventCreateSourceFromEvent([event CGEvent]);
  
  self.generatedBackspaceCount = output.codePointsToDeleteBeforeInsert;
  
  
  if (output.hasCodePointsToDelete) {
    for (int i = 0; i < output.codePointsToDeleteBeforeInsert; i++) {
      [self.appDelegate logDebugMessage:@"sendEvents sending backspace key event"];
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
    [self insertAndReplaceText:self.queuedText deleteCount:0 toReplace:nil client:client];
    self.queuedText = nil;
  } else {
    [self.appDelegate logDebugMessage:@"insertQueuedText called but no text to insert"];
  }
}

@end
