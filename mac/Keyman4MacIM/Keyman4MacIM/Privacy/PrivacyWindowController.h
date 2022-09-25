/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyWindowController.h
 * TestInputMethod2
 * 
 * Created by Shawn Schantz on 2022-09-20.
 * 
 * Controller for the alert dialog that is displayed at startup by the PrivacyConsent class
 * when it detects that the application has not been granted sufficient access
 * (Accessibility or PostEvent, depending on macOS version) to run properly.
 */

#import <Cocoa/Cocoa.h>

NS_ASSUME_NONNULL_BEGIN

@interface PrivacyWindowController : NSWindowController
@property (weak) IBOutlet NSButton *okButton;
@property (weak) IBOutlet NSTextField *alertText;
@property (weak) IBOutlet NSImageView *appLogo;
@property (nonatomic, copy, nullable) void (^consentCallback)(void);

- (IBAction)closeAction:(id)sender;

@end

NS_ASSUME_NONNULL_END
