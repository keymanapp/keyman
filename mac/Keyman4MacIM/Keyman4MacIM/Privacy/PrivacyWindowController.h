/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyWindowController.h
 * Keyman4MacIM
 * 
 * Created by Shawn Schantz on 2022-09-20.
 * 
 */

#import <Cocoa/Cocoa.h>

NS_ASSUME_NONNULL_BEGIN

@interface PrivacyWindowController : NSWindowController
@property (weak) IBOutlet NSButton *okButton;
@property (weak) IBOutlet NSTextField *alertText;
@property (weak) IBOutlet NSImageView *appLogo;
@property (nonatomic, copy, nullable) void (^completionHandler)(void);

- (IBAction)closeAction:(id)sender;

@end

NS_ASSUME_NONNULL_END
