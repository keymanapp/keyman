//
//  KMAboutWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/11/2015.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import "KMAboutWindowController.h"
#import "KMInputMethodAppDelegate.h"
#import "KMOSVersion.h"
#import "KMLogs.h"

@interface KMAboutWindowController ()
@property (nonatomic, weak) IBOutlet NSTextField *versionLabel;
@property (nonatomic, weak) IBOutlet NSTextField *copyrightLabel;
@property (nonatomic, weak) IBOutlet NSButton *licenseButton;
// not needed at the moment but might be:
@property (nonatomic, weak) IBOutlet NSButton *closeButton;
@property (nonatomic, weak) IBOutlet NSButton *configureButton;
@end

@implementation KMAboutWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)windowDidLoad {
  [super windowDidLoad];
  
  // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
  [self.window setBackgroundColor:[NSColor whiteColor]];
  
  KeymanVersionInfo versionInfo = [[self AppDelegate] versionInfo];
  NSString *versionString = NSLocalizedString(@"version-label-text", nil);
  [self.versionLabel setStringValue:[NSString localizedStringWithFormat:versionString, versionInfo.versionWithTag]];
  
  NSMutableString *copyrightInfo = [[NSMutableString alloc] initWithString: [[[NSBundle mainBundle] infoDictionary] objectForKey:@"NSHumanReadableCopyright"]];
  [self.copyrightLabel setStringValue:copyrightInfo];
  
  NSTrackingArea *trackingArea = [[NSTrackingArea alloc] initWithRect:[self.licenseButton bounds]
                                                              options:NSTrackingMouseEnteredAndExited | NSTrackingActiveAlways
                                                                owner:self
                                                             userInfo:nil];
  [self.licenseButton addTrackingArea:trackingArea];
  [self setLicenseButtonTitle:self.licenseButton.title underlined:NO];
}

- (IBAction)configAction:(id)sender {
  // Using `showConfigurationWindow` instead of `showPreferences:` because `showPreferences:` is missing in
  // High Sierra (10.13.1 - 10.13.3). See: https://bugreport.apple.com/web/?problemID=35422518
  // rrb: where Apple's API is broken (10.13.1-10.13.3) call our workaround, otherwise, call showPreferences
  u_int16_t systemVersion = [KMOSVersion SystemVersion];
  if ([KMOSVersion Version_10_13_1] <= systemVersion && systemVersion <= [KMOSVersion Version_10_13_3]) // between 10.13.1 and 10.13.3 inclusive
  {
    os_log([KMLogs uiLog], "About Box: calling workaround instead of showPreferences (sys ver %x)", systemVersion);
    [self.AppDelegate showConfigurationWindow]; // call our workaround
  }
  else
  {
    os_log([KMLogs uiLog], "About Box: calling Apple's showPreferences (sys ver %x)", systemVersion);
    [self.AppDelegate.inputController showPreferences:sender]; // call Apple API
  }
  [self close];
}

- (IBAction)closeAction:(id)sender {
  [self close];
}

- (IBAction)licenseAction:(id)sender {
  [[NSWorkspace sharedWorkspace] openURL:[NSURL fileURLWithPath:[[NSBundle mainBundle] pathForResource:@"keyman-for-mac-os-license" ofType:@"html"]]];
}

- (void)mouseEntered:(NSEvent *)theEvent{
  [self setLicenseButtonTitle:self.licenseButton.title underlined:YES];
}

- (void)mouseExited:(NSEvent *)theEvent{
  [self setLicenseButtonTitle:self.licenseButton.title underlined:NO];
}

- (void)setLicenseButtonTitle:(NSString *)title underlined:(BOOL)underlined {
  NSMutableAttributedString *aTitle = [[NSMutableAttributedString alloc] initWithString:title];
  [aTitle addAttribute:NSFontAttributeName value:[self.licenseButton font] range:NSMakeRange(0, title.length)];
  NSMutableParagraphStyle *paragraphStyle = [[NSMutableParagraphStyle alloc] init];
  [paragraphStyle setAlignment:NSTextAlignmentCenter];
  [aTitle addAttribute:NSParagraphStyleAttributeName value:paragraphStyle range:NSMakeRange(0, title.length)];
  if (underlined)
    [aTitle addAttribute:NSUnderlineStyleAttributeName value:[NSNumber numberWithInt:NSUnderlineStyleSingle] range:NSMakeRange(0, title.length)];
  [self.licenseButton setAttributedTitle:aTitle];
}

@end
