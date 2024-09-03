/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyConsent.m
 * Keyman4MacIM
 * 
 * Created by Shawn Schantz on 2022-09-22.
 * 
 * Used to determine if the user has provided consent to the services it needs
 * and, if not, to request that consent. For versions of macOS prior to 11.0,
 * Keyman requires Accessibility access. For 11.0 and later, Keyman requires
 * PostEvent access. Both are presented to the user as a need for Accessibility
 * when prompted by the system and are listed under Accessibility in the Privacy
 * tab in System Preferences, Security & Privacy. Both Accessibility and
 * PostEvent encapsulate ListenEvent (or Keyboard Monitoring) permission, so
 * that access is not explicitly needed as long as one of the others is provided
 * first.
 *
 * Note that Apple's documentation states that the PostEvent access APIs are
 * available with macOS 10.15 (Catalina) but testing showed that it was only
 * available with macOS 11.0 (Big Sur). This is documented with issue #12295.
 *
 * To access this functionality, used the shared instance. Call
 * requestPrivacyAccess which will check whether access has been granted. If
 * not, then a dialog will be presented to the user to inform them that
 * Accessibility is required. Dismissing this dialog will trigger the API call
 * to prompt the user for permission. However, the prompt from the system will
 * only happen once. If the user was prompted earlier and did not grant access,
 * they can still go to System Preferences and enable it there. So it is useful
 * to prompt with a dialog from Keyman at startup. Without Accessibility, the
 * shift layer and the OSK do not work.
 */

#import "PrivacyConsent.h"

@implementation PrivacyConsent

+ (PrivacyConsent *)shared
{
    static PrivacyConsent *shared = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
      shared = [[PrivacyConsent alloc] init];
    });
    return shared;
}

/**
 * For macOS earlier than 11.0: check for Accessibility access.
 */
- (BOOL)checkAccessibility
{
  BOOL hasAccessibility = NO;
  
  hasAccessibility = AXIsProcessTrusted();
  NSLog(@"  hasAccessibility: %@",hasAccessibility ? @"YES" : @"NO");
  return hasAccessibility;
}

/**
 * Principal method to set privacy checks in motion. May cause multiple dialogs to
 * be presented to the user.
 *
 * The completionHandler is provided to continue with work that the client
 * must execute after privacy is requested.
 */
- (void)requestPrivacyAccess:(void (^)(void))withCompletionHandler
{
  BOOL hasAccessibility = NO;
  
  // check if we already have accessibility
  if (@available(macOS 11.0, *)) {
    hasAccessibility = [self checkPostEventAccess];
  } else {
    hasAccessibility = [self checkAccessibility];
  }
  
  if (hasAccessibility) {
    NSLog(@"already have Accessibility: no need to make request.");
    // call completionHandler immediately -> privacyDialog will not be presented
    withCompletionHandler();
  } else {
    // set completionHandler to be called after privacyDialog is dismissed
    _completionHandler = withCompletionHandler;
    NSLog(@"do not have Accessibility, present Privacy Dialog");
    [self showPrivacyDialog];
  }
}

/**
 * For macOS earlier than 11.0: request Accessibility access.
 */
- (void)requestAccessibility
{
  NSDictionary *options = @{(id)CFBridgingRelease(kAXTrustedCheckOptionPrompt): @YES};
  BOOL hasAccessibility = AXIsProcessTrustedWithOptions((CFDictionaryRef)options);
  NSLog(@"  hasAccessibility: %@",hasAccessibility ? @"YES" : @"NO, requesting...");
}

/**
 * getter for privacyDialog -- does lazy load of controller as privacyDialog is not
 * needed when consent has already been provided by user.
 */
- (NSWindowController *)privacyDialog {
  if (!_privacyDialog) {
        _privacyDialog = [[PrivacyWindowController alloc] initWithWindowNibName:@"PrivacyWindowController"];
    NSLog(@"privacyDialog created");
    [self configureDialogForOsVersion];
  }

  return _privacyDialog;
}

/**
 * Initialize privacy alert appropriately as determined by macOS version.
 */
- (void)configureDialogForOsVersion {
  if (@available(macOS 11.0, *)) {
    [self configureDialogForBigSurAndLater];
  } else {
    [self configureDialogForPreBigSur];
  }
}

/**
 * Initialize privacy dialog for macOS versions prior to Big Sur (11.0).
 * In this case, request Accessibility access, not PostEvent.
 */
- (void)configureDialogForPreBigSur {
  void (^consentPrompt)(void) = ^(void) {
    [self requestAccessibility];
    
    if (self.completionHandler) {
      self.completionHandler();
    }
  };
  [_privacyDialog setCompletionHandler:consentPrompt];
}

/**
 * Initialize privacy dialog for macOS versions of Big Sur (11.0) or later.
 * In this case, request PostEvent access, not Accessibility.
 */
- (void)configureDialogForBigSurAndLater {
  void (^consentPrompt)(void) = ^(void) {
    [self requestPostEventAccess];
    
    if (self.completionHandler) {
      self.completionHandler();
    }
  };
  [_privacyDialog setCompletionHandler:consentPrompt];
}

/**
 * Present the PrivacyDialog alert to the user.
 */
- (void)showPrivacyDialog {
    [[self.privacyDialog window] setLevel:NSModalPanelWindowLevel];
    [[self.privacyDialog window] makeKeyAndOrderFront:nil];
}

/**
 * Check whether the user has allowed ListenEvent access.
 * Only available with macOS Big Sur (11.0) or later.
 *
 * If user has granted Accessibility or PostEvent access, then
 * ListenEvent access is also granted.
 */
- (BOOL)checkListenEventAccess
{
  BOOL hasAccess = NO;
  
  // below checks for ListenEvent access
  if (@available(macOS 11.0, *)) {
    hasAccess = CGPreflightListenEventAccess();
    NSLog(@"CGPreflightListenEventAccess() returned %@", hasAccess ? @"YES" : @"NO");
  } else {
    NSLog(@"CGPreflightListenEventAccess not available before macOS version 11.0");
  }
  return hasAccess;
}

/**
 * Check whether the user has allowed PostEvent access.
 * Only available with macOS Big Sur (11.0) or later.
 *
 * If user has granted Accessibility, then PostEvent access is also granted.
 */
- (BOOL)checkPostEventAccess
{
  BOOL hasAccess = NO;
  
  // below checks for PostEvent access
  if (@available(macOS 11.0, *)) {
    hasAccess = CGPreflightPostEventAccess();
    
    NSLog(@"CGPreflightPostEventAccess() returned %@", hasAccess ? @"YES" : @"NO");
  } else {
    NSLog(@"CGPreflightPostEventAccess not available before macOS version 11.0");
  }
  return hasAccess;
}

/**
 * Request ListenEvent access.
 * Only available with macOS Big Sur (11.0) or later.
 */
- (BOOL)requestListenEventAccess
{
  BOOL granted = NO;
  
  if (@available(macOS 11.0, *)) {
    granted = CGRequestListenEventAccess();
    NSLog(@"CGRequestListenEventAccess() returned %@", granted ? @"YES" : @"NO");
  } else {
    NSLog(@"CGRequestListenEventAccess not available before macOS version 11.0");
  }
  return granted;
}

/**
 * Request PostEvent access.
 * Only available with macOS Big Sur (11.0) or later.
 */
- (BOOL)requestPostEventAccess
{
  BOOL granted = NO;
  
  if (@available(macOS 11.0, *)) {
    granted = CGRequestPostEventAccess();
    NSLog(@"CGRequestPostEventAccess() returned %@", granted ? @"YES" : @"NO");
  } else {
    NSLog(@"CGRequestPostEventAccess not available before macOS version 11.0");
  }
  return granted;
}

@end
