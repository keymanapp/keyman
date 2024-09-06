//
//  KMInputController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 29/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInputController.h"
#import "KMInputMethodEventHandler.h"
#import "KMOSVersion.h"
#include <Carbon/Carbon.h> /* For kVK_ constants. */
#import "KMSettingsRepository.h"
#import "KMLogs.h"
#import "InputMethodKit/InputMethodKit.h"


@implementation KMInputController
const double inactivityTimeout = 0.7;

KMInputMethodEventHandler* _eventHandler;
NSMutableDictionary *textInputClients;

- (KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (id)initWithServer:(IMKServer *)server delegate:(id)delegate client:(id)inputClient
{
  NSRunningApplication *currApp = [[NSWorkspace sharedWorkspace] frontmostApplication];
  NSString *clientAppId = [currApp bundleIdentifier];
  os_log_debug([KMLogs lifecycleLog], "initWithServer, active app: '%{public}@'", clientAppId);

  self = [super initWithServer:server delegate:delegate client:inputClient];
  if (self) {
    textInputClients = [[NSMutableDictionary alloc] initWithCapacity:2];
    self.appDelegate.inputController = self;
    if ((self.appDelegate.kvk != nil) && ([KMSettingsRepository.shared readShowOskOnActivate])) {
      os_log_debug([KMLogs oskLog], "   initWithServer, readShowOskOnActivate= YES, showing OSK");
      [self.appDelegate showOSK];
    }
  }
  
  return self;
}

- (NSUInteger)recognizedEvents:(id)sender {
  return NSEventMaskKeyDown | NSEventMaskFlagsChanged;
}

- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
  if (event == nil || sender == nil || self.kmx == nil || _eventHandler == nil) {
    os_log_error([KMLogs eventsLog], "IMInputController handleEvent: not prepared to handle event = %{public}@", event);
    return NO; // Not sure this can ever happen.
  }
  
  return [_eventHandler handleEvent:event client:sender];
}

// Passthrough from the app delegate low level event hook
// to the input method event handler for handleBackspace.
- (void)handleBackspace:(NSEvent *)event {
  os_log_debug([KMLogs keyLog], "KMInputController handleBackspace, event = %{public}@", event);
  if(_eventHandler != nil) {
    [_eventHandler handleBackspace:event];
  }
}

- (void)activateServer:(id)sender {
  @synchronized(textInputClients) {
    os_log_debug([KMLogs lifecycleLog], "KMInputController activateServer, sender %{public}@", sender);
    [sender overrideKeyboardWithKeyboardNamed:@"com.apple.keylayout.US"];
    NSRunningApplication *currentApp = [[NSWorkspace sharedWorkspace] frontmostApplication];
    NSString *clientAppId = [currentApp bundleIdentifier];
    NSUInteger key = ((NSObject*)sender).hash;
    NSString *keyString = [@(key) stringValue];
    //NSValue *key = [NSValue valueWithNonretainedObject:sender];
    os_log_debug([KMLogs lifecycleLog], "  +++adding client application '%{public}@' to textInputClients map, derived key: %{public}@", clientAppId, keyString);

    [textInputClients setObject:clientAppId forKey:keyString];
    os_log_debug([KMLogs lifecycleLog], "  textInputClients map: %{public}@", textInputClients.description);

    [self.appDelegate wakeUpWith:sender];

    if (_eventHandler != nil) {
      [_eventHandler deactivate];
    }
    
    _eventHandler = [[KMInputMethodEventHandler alloc] initWithClient:clientAppId client:sender];
  }
}

- (void)deactivateServer:(id)sender {
  os_log_debug([KMLogs lifecycleLog], "KMInputController deactivateServer, sender %{public}@", sender);
  @synchronized(textInputClients) {
    NSUInteger key = ((NSObject*)sender).hash;
    NSString *keyString = [@(key) stringValue];
    //NSValue *key = [NSValue valueWithNonretainedObject:sender];
    NSString *clientAppId = [textInputClients objectForKey:keyString];
    
    if (clientAppId) {
      os_log_debug([KMLogs lifecycleLog], "  ---removing client application '%{public}@' from textInputClients map, key: %{public}@", clientAppId, keyString);
      [textInputClients removeObjectForKey:keyString];
    } else {
      os_log_debug([KMLogs lifecycleLog], "  key %{public}@ not found in textInputClients map", keyString);
    }
    os_log_debug([KMLogs lifecycleLog], "  textInputClients map: %{public}@", textInputClients.description);
    if (textInputClients.count == 0) {
      os_log_debug([KMLogs lifecycleLog], "no text input clients found in textInputClients map; delay for %f seconds and call sleepIfNoClients", inactivityTimeout);
      [self performSelector:@selector(sleepIfNoClients:) withObject:sender afterDelay:inactivityTimeout];
    }
  }
}

- (void)sleepIfNoClients:(id)lastClient {
  @synchronized(textInputClients) {
    if (textInputClients.count == 0) {
      os_log_debug([KMLogs lifecycleLog], "sleepIfNoClients found no clients, time to sleep");
      if (_eventHandler != nil) {
        [_eventHandler deactivate];
        _eventHandler = nil;
      }
      [self.appDelegate sleepFollowingInactivityTimeout:lastClient];
    } else {
      NSArray*keys=[textInputClients allKeys];
      NSObject *key = keys[0];
      NSString *clientAppId = [textInputClients objectForKey:key];
      os_log_debug([KMLogs lifecycleLog], "sleepIfNoClients found a newly activated client, clientAppId '%{public}@', key: %{public}@", clientAppId, key);
    }
  }
}

- (NSMenu *)menu {
  return self.appDelegate.menu;
}

- (KMXFile *)kmx {
  return self.appDelegate.kmx;
}

- (void)menuAction:(id)sender {
  NSMenuItem *mItem = [sender objectForKey:kIMKCommandMenuItemName];
  NSInteger itag = mItem.tag;
  os_log_debug([KMLogs uiLog], "Keyman menu clicked - tag: %lu", itag);
  if (itag == CONFIG_MENUITEM_TAG) {
    [self showConfigurationWindow:sender];
  }
  else if (itag == OSK_MENUITEM_TAG) {
    [KMSettingsRepository.shared writeShowOskOnActivate:YES];
    os_log_debug([KMLogs oskLog], "menuAction OSK_MENUITEM_TAG, updating settings writeShowOsk to YES");
    [self.appDelegate showOSK];
  }
  else if (itag == ABOUT_MENUITEM_TAG) {
    [self.appDelegate showAboutWindow];
  }
  else if (itag >= KEYMAN_FIRST_KEYBOARD_MENUITEM_TAG) {
    [self.appDelegate selectKeyboardFromMenu:itag];
  }
}

- (void)showConfigurationWindow:(id)sender {
  // Using `showConfigurationWindow` instead of `showPreferences:` because `showPreferences:` is missing in
  // High Sierra (10.13.1 - 10.13.3). See: https://bugreport.apple.com/web/?problemID=35422518
  // rrb: where Apple's API is broken (10.13.1-10.13.3) call our workaround, otherwise, call showPreferences
  u_int16_t systemVersion = [KMOSVersion SystemVersion];
  if ([KMOSVersion Version_10_13_1] <= systemVersion && systemVersion <= [KMOSVersion Version_10_13_3]) // between 10.13.1 and 10.13.3 inclusive
  {
    os_log_info([KMLogs uiLog], "Input Menu: calling workaround instead of showPreferences (sys ver %x)", systemVersion);
    [self.appDelegate showConfigurationWindow]; // call our workaround
  }
  else
  {
    os_log_info([KMLogs uiLog], "Input Menu: calling Apple's showPreferences (sys ver %x)", systemVersion);
    [self showPreferences:sender]; // call Apple API
  }
}

@end
