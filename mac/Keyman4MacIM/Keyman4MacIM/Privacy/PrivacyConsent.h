/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyConsent.h
 * TestInputMethod2
 * 
 * Created by Shawn Schantz on 2022-09-22.
 * 
 * Used to determine if the user has provided consent to the services needed for key manipulation, and, if not, to request that consent.
 * For versions of macOS prior to 10.15, requires Accessibility access.
 * For 10.15 and later, requires PostEvent access, which is also presented to the user
 * as a need for Accessibility.
 * Both Accessibility and PostEvent encapsulate ListenEvent (or Keyboard Monitoring)
 * permission, so that access is not explicitly needed as long as one of
 * the others is provided first.
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
