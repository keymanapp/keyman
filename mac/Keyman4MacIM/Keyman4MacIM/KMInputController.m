/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Serkan Kurt on 2015-01-29.
 *
 */

#import "KMInputController.h"
#import "KMInputMethodEventHandler.h"
#import "KMOSVersion.h"
#include <Carbon/Carbon.h> /* For kVK_ constants. */
#import "KMSettingsRepository.h"
#import "KMLogs.h"
#import "InputMethodKit/InputMethodKit.h"
#import "KMInputMethodLifecycle.h"


@implementation KMInputController

KMInputMethodEventHandler* _eventHandler;

- (KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (id)initWithServer:(IMKServer *)server delegate:(id)delegate client:(id)inputClient
{
  os_log_debug([KMLogs lifecycleLog], "initWithServer, active app: '%{public}@'", [KMInputMethodLifecycle getRunningApplicationId]);

  self = [super initWithServer:server delegate:delegate client:inputClient];
  if (self) {
    self.appDelegate.inputController = self;
  }
  
  /**
   * Register to receive the Deactivated and ChangedClient notification generated from KMInputMethodLifecycle so
   * that the eventHandler can be changed. There is no need to receive the Activated notification because
   * the InputController does it all it needs to when it receives the ChangedClient.
   */

  [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(inputMethodDeactivated:) name:kInputMethodDeactivatedNotification object:nil];
  [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(inputMethodChangedClient:) name:kInputMethodClientChangeNotification object:nil];

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

/**
 * The Keyman input method is deactivating because the user chose a different input method: notification from KMInputMethodLifecycle
 */
- (void)inputMethodDeactivated:(NSNotification *)notification {
  os_log_debug([KMLogs lifecycleLog], "***KMInputController inputMethodDeactivated, deactivating eventHandler");
  if (_eventHandler != nil) {
    [_eventHandler deactivate];
  }
}

/**
 * The user has switched to a different text input client: notification from KMInputMethodLifecycle
 */
- (void)inputMethodChangedClient:(NSNotification *)notification {
  os_log_debug([KMLogs lifecycleLog], "***KMInputController inputMethodChangedClient, deactivating old eventHandler and activating new one");
  if (_eventHandler != nil) {
    [_eventHandler deactivate];
  }
  _eventHandler = [[KMInputMethodEventHandler alloc] initWithClient:[KMInputMethodLifecycle getRunningApplicationId] client:self.client];

}

- (void)activateServer:(id)sender {
  [sender overrideKeyboardWithKeyboardNamed:@"com.apple.keylayout.US"];
  [KMInputMethodLifecycle.shared activateClient:sender];
}

- (void)deactivateServer:(id)sender {
  [KMInputMethodLifecycle.shared deactivateClient:sender];
  [[NSNotificationCenter defaultCenter] removeObserver:self];
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
