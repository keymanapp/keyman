/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyConsent.h
 * TestInputMethod2
 * 
 * Created by Shawn Schantz on 2022-09-22.
 * 
 * Used to determine if the user has provided consent to the services it needs
 * and, if not, to request that consent.
 * For versions of macOS prior to 10.15, Keyman requires Accessibility access.
 * For 10.15 and later, Keyman requires PostEvent access.
 * Both are presented to the user as a need for Accessibility when prompted by
 * the system and are listed under Accessibility in the Privacy tab in
 * System Preferences, Security & Privacy.
 * Both Accessibility and PostEvent encapsulate ListenEvent (or Keyboard Monitoring)
 * permission, so that access is not explicitly needed as long as one of
 * the others is provided first.
 *
 * Access this functionality through the the shared instance. Call
 * requestPrivacyAccess which will check whether access has been granted.
 * If not, then a dialog will be presented to the user to inform them
 * that Accessibility is required. Dismissing this dialog will trigger
 * the API call to prompt the user for permission. However, the prompt
 * from the sytem will only happen once.
 * If the user was prompted earlier and did not grant access, they can
 * still go to Sytem Preferences and enable it there. So it is useful to prompt
 * with a dialog from Keyman at startup.
 * Without Accessibility, the shift layer and the OSK do not work -- so Keyman
 * is basically useless.
 */

#import <Foundation/Foundation.h>
#import "PrivacyWindowController.h"

NS_ASSUME_NONNULL_BEGIN

@interface PrivacyConsent : NSObject
@property (nonatomic, strong) PrivacyWindowController *privacyDialog;
@property (nonatomic, copy, nullable) void (^handler)(void);
+ (PrivacyConsent *)shared;
- (void)requestPrivacyAccess:(void (^)(void))completionHandler;
@end

NS_ASSUME_NONNULL_END
