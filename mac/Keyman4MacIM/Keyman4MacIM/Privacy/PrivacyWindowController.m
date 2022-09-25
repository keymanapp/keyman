/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyWindowController.m
 * TestInputMethod2
 * 
 * Created by Shawn Schantz on 2022-09-20.
 * 
 * Controller for the alert dialog that is displayed at startup by the PrivacyConsent class
 * when it detects that the application has not been granted sufficient access
 * (Accessibility or PostEvent, depending on macOS version) to run properly.
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
  
  NSFileManager *fileManager  = [NSFileManager defaultManager];
  NSString *logoPath = [[NSBundle mainBundle] pathForResource:@"keyman-alert-logo" ofType:@"png"];
  if ([fileManager fileExistsAtPath:logoPath]) {
    NSLog(@"keyman-alert-logo exists at path %@", logoPath);
    NSImage *logo = [[NSImage alloc]initWithContentsOfFile:logoPath];
    [_appLogo setImage:logo];
  } else {
    NSLog(@"keyman-alert-logo does not exist at path %@", logoPath);
  }
  
  [_okButton setEnabled:YES];
  
  // main need to add button for easier access if we can detect that system will not prompt
  /*  if (!hasAccessibility) {
        NSString *urlString = @"x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility";
        [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:urlString]];
    }
   */

}

- (IBAction)closeAction:(id)sender {
  [self close];
  
  // if callback to check for user consent has been set, execute it
  if (self.consentCallback) {
    self.consentCallback();
  }
}

@end
