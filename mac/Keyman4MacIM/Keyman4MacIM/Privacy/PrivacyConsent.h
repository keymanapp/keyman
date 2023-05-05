/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * PrivacyConsent.h
 * Keyman4MacIM
 * 
 * Created by Shawn Schantz on 2022-09-22.
 * 
 */

#import <Foundation/Foundation.h>
#import "PrivacyWindowController.h"

NS_ASSUME_NONNULL_BEGIN

@interface PrivacyConsent : NSObject
@property (nonatomic, strong) PrivacyWindowController *privacyDialog;
@property (nonatomic, copy, nullable) void (^completionHandler)(void);
+ (PrivacyConsent *)shared;
- (BOOL)checkAccessibility;
- (void)requestPrivacyAccess:(void (^)(void))withCompletionHandler;
@end

NS_ASSUME_NONNULL_END
