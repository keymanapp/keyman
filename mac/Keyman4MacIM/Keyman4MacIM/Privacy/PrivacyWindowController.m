/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyWindowController.m
 * Keyman4MacIM
 * 
 * Created by Shawn Schantz on 2022-09-20.
 * 
 * Controller for the alert dialog that is displayed at startup by the
 * PrivacyConsent class when it detects that the application has not been
 * granted sufficient access (Accessibility or PostEvent, depending on macOS
 * version) to run properly.
 */

#import "PrivacyWindowController.h"

@interface PrivacyWindowController ()
@end

@implementation PrivacyWindowController

- (void)windowDidLoad {
  [super windowDidLoad];
  
  NSString *bundleDisplayName = [[[NSBundle mainBundle] localizedInfoDictionary]
                                 objectForKey:@"CFBundleDisplayName"];
  [self.window setTitle:bundleDisplayName];
  [_alertText setStringValue:NSLocalizedString(@"privacy-alert-text", nil)];
  
  NSImage *keymanLogo = [NSImage imageNamed:NSImageNameApplicationIcon];
  if (keymanLogo) {
    [_appLogo setImage:keymanLogo];
  }
  [_okButton setEnabled:YES];
}

- (IBAction)closeAction:(id)sender {
  [self close];
  
  // jump to System Preferences Security and Privacy settings
  NSString *urlString = @"x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility";
  [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:urlString]];
  
  // if callback to check for user consent has been set, execute it
  if (self.completionHandler) {
    self.completionHandler();
  }
}

@end
