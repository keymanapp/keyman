/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-09.
 * 
 * This class is responsible for determining the state of the Keyman input method.
 * It is called from the KMInputController (a subclass of IMKInputController), and
 * shares changes in the state of the input method by synchronously posting
 * notifications to NSNotificationCenter.
 */

/**
 * This class is needed because many activateServer and deactivateServer messages sent from macOS
 * to KMInputController, but they are not particularly reliable. Keyman receives some messages when it
 * is not active and should not become active. It also receives messages when it is active, but there is no
 * need to change state. For example, when a menu is clicked with Keyman active, macOS will send a
 * deactivateServer message followed by an activateServer message when the menu is released.
 * The messages may also arrive in an unexpected order.
 *
 * Instead of relying on the information conveyed in these messages, this class interprets them as a notification
 * that the input method state may have changed. For the actual state of the input method, it gets the current
 * input source using the Carbon APIs TISCopyCurrentKeyboardInputSource and TISGetInputSourceProperty.
 * If the result is equal to "keyman.inputmethod.Keyman", then Keyman is the active input method. If, for
 * example, the U.S. keyboard were selected, then the result would be "com.apple.keylayout.US".
 *
 * The state of the text input client is discovered using the NSRunningApplication frontmostApplication API.
 * Knowing the current input method and the current text input client enables us to determine whether the
 * state has actually changed and how to adjust to the new state.
 *
 * It is important for state to be known so that the On-screen keyboard can be appropriately shown or hidden
 * and the low-level event tap can be stopped or started.
 */

#import "KMInputMethodLifecycle.h"
#import "KMLogs.h"
#import <AppKit/AppKit.h>
#import "KMSettingsRepository.h"
#import <Carbon/Carbon.h>

NSString *const kInputMethodActivatedNotification = @"kInputMethodActivatedNotification";
NSString *const kInputMethodDeactivatedNotification = @"kInputMethodDeactivatedNotification";
NSString *const kInputMethodClientChangeNotification = @"kInputMethodClientChangeNotification";
NSString *const keymanInputMethodName = @"keyman.inputmethod.Keyman";

typedef enum {
  Started,
  Active,
  Inactive
} LifecycleState;

typedef enum {
  None,
  Activate,
  Deactivate,
  ChangeClients,
} TransitionType;


@interface KMInputMethodLifecycle()

@property LifecycleState lifecycleState;
@property NSString *inputSourceId;
@property NSString *clientApplicationId;
@end

@implementation KMInputMethodLifecycle

+ (KMInputMethodLifecycle *)shared {
  static KMInputMethodLifecycle *shared = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    shared = [[KMInputMethodLifecycle alloc] init];
  });
  
  return shared;
}

- (instancetype)init {
  self = [super init];
  if (self) {
    _lifecycleState = Started;
    _inputSourceId = @"";
    _clientApplicationId = @"";
  }
  return self;
}

/**
 * called from Application Delgate during init
 */
- (void)startLifecycle {
  _lifecycleState = Started;
}

/**
 * Use Carbon APIs to get the current input source or input method. Even though many Carbon APIs were deprecated and removed
 * from the OS years ago, these and other low-level APIs are still supported (but apparently completely undocumented).
 */
+ (NSString*)getCurrentInputSourceId {
  TISInputSourceRef inputSource = TISCopyCurrentKeyboardInputSource();
  NSString *inputSourceId = (__bridge NSString *)(TISGetInputSourceProperty(inputSource, kTISPropertyInputSourceID));
  return inputSourceId;
}

/**
 * Get the bundle ID of the currently active text input client..
 */
+ (NSString*)getClientApplicationId {
  NSRunningApplication *currentApp = [[NSWorkspace sharedWorkspace] frontmostApplication];
  NSString *clientAppId = [currentApp bundleIdentifier];
  return clientAppId;
}

/**
 * Based on the current lifecycleState and the input method state from the OS, determine how the state must transition.
 */
- (TransitionType)determineTransition:(NSString*)newInputSourceId withAppId:(NSString*)newClientAppId {
  TransitionType transition = None;
  BOOL inputSourceIsKeyman = [newInputSourceId isEqualTo:keymanInputMethodName];
  BOOL clientHasChanged = [self.clientApplicationId isNotEqualTo:newClientAppId];
  os_log_debug([KMLogs lifecycleLog], "determineTransition, current InputSourceId: %{public}@, new InputSourceId: %{public}@, current ClientAppId: %{public}@, new ClientAppId: %{public}@, inputSourceIsKeyman: %d, clientHasChanged: %d", self.inputSourceId, newInputSourceId, self.clientApplicationId, newClientAppId, inputSourceIsKeyman, clientHasChanged);

  switch (self.lifecycleState) {
    case Started:
     transition = Activate;
      break;
    case Active:
      if (inputSourceIsKeyman) {
        if (clientHasChanged) {
          transition = ChangeClients;
        }
      } else {
        transition = Deactivate;
      }
      break;
    case Inactive:
      if (inputSourceIsKeyman) {
        transition = Activate;
      }
      break;
  }
  return transition;
}

/**
 * Update the input method state, consisting of the input source ID and the client application ID.
 */
- (void)saveNewInputMethodState:(NSString*)newInputSourceId withAppId:(NSString*)newClientAppId {
  self.inputSourceId = newInputSourceId;
  self.clientApplicationId = newClientAppId;
}

/**
 * Called when IMKInputController receives an activateServer message
 */
- (void)performTransition:(id)client {
  NSString *currentInputSource = [KMInputMethodLifecycle getCurrentInputSourceId];
  NSString *currentClientAppId = [KMInputMethodLifecycle getClientApplicationId];
  
  TransitionType transition = [self determineTransition:currentInputSource withAppId:currentClientAppId];
  [self saveNewInputMethodState:currentInputSource withAppId:currentClientAppId];
  
  switch(transition) {
    case None:
      os_log_info([KMLogs lifecycleLog], "performTransition: None, new InputSourceId: %{public}@, new application ID: %{public}@", currentInputSource, currentClientAppId);
     break;
    case Activate:
      os_log_info([KMLogs lifecycleLog], "performTransition: Activate, new InputSourceId: %{public}@, new application ID: %{public}@", currentInputSource, currentClientAppId);
      [self changeClient];
      [self activateInputMethod];
      break;
    case Deactivate:
      os_log_info([KMLogs lifecycleLog], "performTransition: Deactivate, new InputSourceId: %{public}@, new application ID: %{public}@", currentInputSource, currentClientAppId);
      [self deactivateInputMethod];
      break;
    case ChangeClients:
      os_log_info([KMLogs lifecycleLog], "performTransition: ChangeClients, new InputSourceId: %{public}@, new application ID: %{public}@", currentInputSource, currentClientAppId);
      [self changeClient];
      break;
  }
}
  
/**
 * Called when IMKInputController receives an activateServer message
 */
- (void)activateClient:(id)client {
  os_log_debug([KMLogs lifecycleLog], "KMInputMethodLifecycle activateClient");
  [self performTransition:client];
}

/**
 * Called when IMKInputController receives an deactivateServer message
 */
- (void)deactivateClient:(id)client {
  os_log_debug([KMLogs lifecycleLog], "KMInputMethodLifecycle deactivateClient");
  [self performTransition:client];
}

/**
 * Change lifecycleState to Active and send notification.
 */
- (void)activateInputMethod {
  os_log_debug([KMLogs lifecycleLog], "activateInputMethod");
  _lifecycleState = Active;
  [[NSNotificationCenter defaultCenter] postNotificationName:kInputMethodActivatedNotification object:self];
}

/**
 * Change lifecycleState to Inactive and send notification.
 */
- (void)deactivateInputMethod {
  os_log_debug([KMLogs lifecycleLog], "deactivateInputMethod");
  _lifecycleState = Inactive;
  [[NSNotificationCenter defaultCenter] postNotificationName:kInputMethodDeactivatedNotification object:self];
}

/**
 * Does not change lifecycleState, just fires notification so that InputController knows to change the event handler
 */
- (void)changeClient {
  os_log_debug([KMLogs lifecycleLog], "changeClient");
  [[NSNotificationCenter defaultCenter] postNotificationName:kInputMethodClientChangeNotification object:self];
}

/**
 * Returns true if Started or Active
 */
- (BOOL)shouldEnableEventTap {
  return ((self.lifecycleState == Started) || (self.lifecycleState == Active));
}

/**
 * Returns true if lifecycleState is  Active and the Settings require us to show the OSK
 */
- (BOOL)shouldShowOskOnActivate {
  return [KMSettingsRepository.shared readShowOskOnActivate]
    && (self.lifecycleState == Active);
}

@end
