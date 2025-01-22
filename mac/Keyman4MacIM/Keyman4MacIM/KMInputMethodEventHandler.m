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
#import "KMSettingsRepository.h"
#import "KMLogs.h"
#import "KMSentryHelper.h"
@import Sentry;

@interface KMInputMethodEventHandler ()
@property (nonatomic, retain) KeySender* keySender;
@property (nonatomic, retain) TextApiCompliance* apiCompliance;
@property NSUInteger generatedBackspaceCount;
@property NSUInteger lowLevelBackspaceCount;
@property (nonatomic, retain) NSString* clientApplicationId;
@property NSString *queuedText;
@property BOOL contextChanged;
@property BOOL sentryTestingEnabled;
@end

@implementation KMInputMethodEventHandler

const CGKeyCode kProcessPendingBuffer = 0xFF;
const NSUInteger kMaxContext = 80;
const NSTimeInterval kQueuedTextEventDelayInSeconds = 0.1;

CGKeyCode _keyCodeOfOriginalEvent;
CGEventSourceRef _sourceFromOriginalEvent = nil;
CGEventSourceRef _sourceForGeneratedEvent = nil;

// This is the public initializer.
- (instancetype)initWithClient:(NSString *)clientAppId client:(id) sender {
  _keySender = [[KeySender alloc] init];
  _clientApplicationId = clientAppId;
  _generatedBackspaceCount = 0;
  _lowLevelBackspaceCount = 0;
  _queuedText = nil;

  _apiCompliance = [[TextApiCompliance alloc]initWithClient:sender applicationId:clientAppId];
  os_log_info([KMLogs lifecycleLog], "KMInputMethodEventHandler initWithClient, clientAppId: %{public}@", clientAppId);
  [KMSentryHelper addBreadCrumb:@"lifecycle" message:[NSString stringWithFormat:@"KMInputMethodEventHandler initWithClient, clientAppId '%@'", clientAppId]];

  [KMSentryHelper addClientAppIdTag:clientAppId];
  return self;
}

- (void)deactivate {
  if (_generatedBackspaceCount > 0 || (_queuedText != nil && _queuedText.length > 0) ||
      _keyCodeOfOriginalEvent != 0 || _sourceFromOriginalEvent != nil)
  {
    os_log_error([KMLogs lifecycleLog], "ERROR: new app activated before previous app finished processing pending events! _generatedBackspaceCount: %lu, _queuedText: '%{public}@' _keyCodeOfOriginalEvent: %hu", _generatedBackspaceCount, _queuedText == nil ? @"(NIL)" : (NSString*)[self queuedText], _keyCodeOfOriginalEvent);
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
  os_log_debug([KMLogs keyLog], "KMInputMethodEventHandler handleCommand, event type=%lu, must refresh context", (unsigned long)event.type);
  self.contextChanged = YES;
}

- (KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (KMEngine *)kme {
  return self.appDelegate.kme;
}

- (BOOL) handleEventWithKeymanEngine:(NSEvent *)event in:(id) sender {
  CoreKeyOutput *output = nil;
  
  if ([self.appDelegate canForceSentryEvents]) {
    if ([self forceSentryEvent:event]) {
      return NO;
    }
  }
  
  output = [self processEventWithKeymanEngine:event in:sender];

  if (output == nil) {
    return NO;
  }
  
  return [self applyKeymanCoreActions:output event:event client:sender];
}

- (CoreKeyOutput*) processEventWithKeymanEngine:(NSEvent *)event in:(id) sender {
  CoreKeyOutput* coreKeyOutput = nil;
  if (self.appDelegate.lowLevelEventTap != nil) {
    NSEventModifierFlags modifierFlags = [self.appDelegate determineModifiers];
    os_log_debug([KMLogs eventsLog], "processEventWithKeymanEngine, using modifierFlags 0x%lX, instead of event.modifiers 0x%lX", modifierFlags, event.modifierFlags);

    NSEvent *eventWithAppropriateModifierFlags = [NSEvent keyEventWithType:event.type location:event.locationInWindow modifierFlags:modifierFlags timestamp:event.timestamp windowNumber:event.windowNumber context:[NSGraphicsContext currentContext] characters:event.characters charactersIgnoringModifiers:event.charactersIgnoringModifiers isARepeat:event.isARepeat keyCode:event.keyCode];
    coreKeyOutput = [self.kme processEvent:eventWithAppropriateModifierFlags];
  }
  else {
    /** 
     * Without the low level event tap, there will be no way to distinguish left and right option keys.
     * Also command-key actions that should invalidate the context may go undetected.
     * Have yet to determine which scenarios prevent an event tap from be created
     */
    coreKeyOutput = [self.kme processEvent:event];
  }
  return coreKeyOutput;
}

/**
 * When in the mode to test sentry, this method will force certain Sentry APIs to be called,
 * such as forcing a crash or capturing a message, depending on the key being typed.
 */
- (BOOL)forceSentryEvent:(NSEvent *)event {
  BOOL forcedEvent = YES;
  
  NSString *sentryCommand = event.characters;
  if ([sentryCommand isEqualToString:@"{"]) {
    os_log_debug([KMLogs testLog], "forceSentryEvent: forcing crash now");
    NSBeep();
    [SentrySDK crash];
  } else if ([sentryCommand isEqualToString:@"["]) {
    os_log_debug([KMLogs testLog], "forceSentryEvent: forcing message now");
    NSBeep();
    [SentrySDK captureMessage:@"Forced test message"];
  } else {
    os_log_debug([KMLogs testLog], "forceSentryEvent: unrecognized command");
    forcedEvent = NO;
  }
  
  return forcedEvent;
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
  os_log_debug([KMLogs eventsLog], "KMInputMethodEventHandler handleBackspace, event = %{public}@", event);
  [KMSentryHelper addBreadCrumb:@"user" message:@"handle backspace for non-compliant app"];

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
  os_log_debug([KMLogs eventsLog], "KMInputMethodEventHandler triggerInsertQueuedText");
  [self.keySender sendKeymanKeyCodeForEvent:event];
}

//MARK: Core-related key processing

- (void)checkTextApiCompliance:(id)client {
  // If the TextApiCompliance object is stale, create a new one for the current application and context.
  // The text api compliance may vary from one text field to another of the same app.
  
  if ([self textApiComplianceIsStale]) {
    self.apiCompliance = [[TextApiCompliance alloc]initWithClient:client applicationId:self.clientApplicationId];
    os_log_debug([KMLogs complianceLog], "KMInputMethodHandler initWithClient checkTextApiCompliance: %{public}@", _apiCompliance);
  } else if (self.apiCompliance.isComplianceUncertain) {
    // We have a valid TextApiCompliance object, but compliance is uncertain, so test it
    [self.apiCompliance checkCompliance:client];
  }
}

- (BOOL)textApiComplianceIsStale {
  BOOL stale = false;
  NSString *complianceAppId = nil;
  TextApiCompliance *currentApiCompliance = self.apiCompliance;
  
  // test for three scenarios in which the api compliance is stale
  if (currentApiCompliance == nil) { // if we have no previous api compliance object
    stale = true;
  } else { // if we have one but for a different client
    complianceAppId = self.apiCompliance.clientApplicationId;
    if ([complianceAppId isNotEqualTo:self.clientApplicationId]) {
      stale = true;
    } else {
      stale = false;
    }
  }
  if (self.contextChanged) { // if the context has changed
    stale = true;
  }

  NSString *message = [NSString stringWithFormat:@"textApiComplianceIsStale = %@ for appId: %@, apiCompliance: %p, complianceAppId: %@", stale?@"true":@"false", self.clientApplicationId, currentApiCompliance, complianceAppId];
  [KMSentryHelper addDebugBreadCrumb:@"compliance" message:message];
  os_log_debug([KMLogs complianceLog], "%{public}@", message);

  return stale;
}

- (void) handleContextChangedByLowLevelEvent {
  if (self.appDelegate.contextChangedByLowLevelEvent) {
    if (!self.contextChanged) {
      os_log_debug([KMLogs complianceLog], "Low-level event has changed the context.");
      self.contextChanged = YES;
    }
    self.appDelegate.contextChangedByLowLevelEvent = NO;
  }
}

// handleEvent implementation for core event processing
- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
  BOOL handled = NO;
  os_log_debug([KMLogs eventsLog], "handleEvent፡ event = %{public}@", event);

  [self checkTextApiCompliance:sender];
  
  // mouse movement requires that the context be invalidated
  [self handleContextChangedByLowLevelEvent];
  
  if (event.type == NSEventTypeKeyDown) {
    [KMSentryHelper addDebugBreadCrumb:@"event" message:@"handling keydown event"];
    // indicates that our generated backspace event(s) are consumed
    // and we can insert text that followed the backspace(s)
    if (event.keyCode == kKeymanEventKeyCode) {
      os_log_debug([KMLogs eventsLog], "handleEvent, handling kKeymanEventKeyCode");
      [self insertQueuedText: event client:sender];
      return YES;
    }
    
    // for some apps, handleEvent will not be called for the generated backspace
    // but if it is, we need to let it pass through rather than process again in core
    if((event.keyCode == kVK_Delete) && (self.lowLevelBackspaceCount > 0)) {
      os_log_debug([KMLogs eventsLog], "handleEvent, allowing generated backspace to pass through");
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
  
  os_log_debug([KMLogs eventsLog], "event, keycode: %u, characters='%{public}@' handled = %{public}@", event.keyCode, event.characters, handled?@"yes":@"no");
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
    os_log_debug([KMLogs eventsLog], "reportContext, setting new context='%{public}@' for compliant app (if needed)", contextString);
    [self.kme setCoreContextIfNeeded:contextString];
  } else if (self.contextChanged) {
    // we cannot read the text but know the context has changed, so we must clear it
    os_log_debug([KMLogs eventsLog], "reportContext, clearing context for non-compliant app");
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
      os_log_debug([KMLogs eventsLog], "   *** InputMethodEventHandler readContext, %lu characters", (unsigned long)contextLength);
      NSRange contextRange = NSMakeRange(contextStart, contextLength);
      attributedString = [client attributedSubstringFromRange:contextRange];
      
      // adjust string in case that we receive half of a surrogate pair at context start
      // the API appears to always return a full code point, but this could vary by app
      if (attributedString.length > 0) {
        if (CFStringIsSurrogateLowCharacter([attributedString.string characterAtIndex:0])) {
          os_log_debug([KMLogs eventsLog], "   *** InputMethodEventHandler readContext, first char is low surrogate, reducing context by one character");
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
  //os_log_debug([KMLogs eventsLog], "InputMethodEventHandler applyKeymanCoreActions: output = %{public}@ ", output);
  
  BOOL handledEvent = [self applyKeyOutputToTextInputClient:output keyDownEvent:event client:client];
  [self applyNonTextualOutput:output];
  
  // if output from Keyman Core indicates to emit the keystroke,
  // then return NO, so that the OS still handles the event
  if (handledEvent && output.emitKeystroke) {
    os_log_debug([KMLogs eventsLog], "   *** emitKeystroke, not handling event");
    handledEvent = NO;
  }
  
  return handledEvent;
}

-(BOOL)applyKeyOutputToTextInputClient:(CoreKeyOutput*)output keyDownEvent:(nonnull NSEvent *)event client:(id) client {
  BOOL handledEvent = YES;
  
  if (output.isInsertOnlyScenario) {
    os_log_debug([KMLogs keyLog], "applyOutputToTextInputClient, insert only scenario");
    [self insertAndReplaceTextForOutput:output client:client];
  } else if (output.isDeleteOnlyScenario) {
    if ((event.keyCode == kVK_Delete) && output.codePointsToDeleteBeforeInsert == 1) {
      // let the delete pass through in the original event rather than sending a new delete
      os_log_debug([KMLogs keyLog], "applyOutputToTextInputClient, delete only scenario with passthrough");
      handledEvent = NO;
    } else {
      os_log_debug([KMLogs keyLog], "applyOutputToTextInputClient, delete only scenario");
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
      os_log_debug([KMLogs keyLog], "applyOutputToTextInputClient, delete and insert scenario with events");
      [self sendEvents:event forOutput:output];
    } else {
      os_log_debug([KMLogs keyLog], "applyOutputToTextInputClient, delete and insert scenario with insert API");
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
      os_log_debug([KMLogs keyLog], "persistOptions, key: %{public}@, value: %{public}@", key, value);
      [KMSentryHelper addBreadCrumb:@"event" message:@"persist options"];
      [[KMSettingsRepository shared] writeOptionForSelectedKeyboard:key withValue:value];
    }
    else {
      os_log_debug([KMLogs keyLog], "invalid values in persistOptions, not writing to UserDefaults, key: %{public}@, value: %{public}@", key, value);
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
  os_log_debug([KMLogs keyLog], "insertAndReplaceText, insert: %{public}@, delete: %{public}@, replacementCount: %d", text, textToDelete, replacementCount);
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
  os_log_debug([KMLogs keyLog], "sendEvents called, output = %{public}@", output);

  _sourceForGeneratedEvent = CGEventCreateSourceFromEvent([event CGEvent]);
  
  self.generatedBackspaceCount = output.codePointsToDeleteBeforeInsert;
  
  
  if (output.hasCodePointsToDelete) {
    for (int i = 0; i < output.codePointsToDeleteBeforeInsert; i++) {
      os_log_debug([KMLogs keyLog], "sendEvents sending backspace key event");
      [self.keySender sendBackspaceforEventSource:_sourceForGeneratedEvent];
    }
  }
  
  if (output.hasTextToInsert) {
    os_log_debug([KMLogs keyLog], "sendEvents, queueing text to insert: %{public}@", output.textToInsert);
    
    self.queuedText = output.textToInsert;
  }
}

-(void)insertQueuedText: (NSEvent *)event client:(id) client  {
  if (self.queuedText.length> 0) {
    os_log_debug([KMLogs keyLog], "insertQueuedText, inserting %{public}@", self.queuedText);
    [self insertAndReplaceText:self.queuedText deleteCount:0 toReplace:nil client:client];
    self.queuedText = nil;
  } else {
    os_log_debug([KMLogs keyLog], "insertQueuedText called but no text to insert");
  }
}

@end
