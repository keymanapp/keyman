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
  
  // need to provide a defaultValue for the possibility that:
  // 1. the app is localized for a given language x
  // 2. the string with the specified key is not found in Localizable.strings for language x
  // if not, then UI will display the key instead of the string with that language selected

  NSString *defaultValue = [self defaultLocalizableString: @"privacy-alert-text"];
  [_alertText setStringValue:NSLocalizedStringWithDefaultValue(@"privacy-alert-text", nil, NSBundle.mainBundle, defaultValue, nil)];
  
  NSImage *keymanLogo = [NSImage imageNamed:NSImageNameApplicationIcon];
  if (keymanLogo) {
    [_appLogo setImage:keymanLogo];
  }
  [_okButton setEnabled:YES];
}

/**
 * utility method to get the English string for the localizable key
 * move to some generic place if it will be widely used
 */
- (NSString*) defaultLocalizableString:(NSString*) key {
  NSBundle *main = [NSBundle mainBundle];
  NSString *resourcePath = [main pathForResource:@"en" ofType:@"lproj"];
  NSBundle *bundle = [NSBundle bundleWithPath:resourcePath];
  return NSLocalizedStringFromTableInBundle(key, nil, bundle, nil);
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
