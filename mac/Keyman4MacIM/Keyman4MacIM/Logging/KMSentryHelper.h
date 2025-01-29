/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-01-13.
 *
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMSentryHelper : NSObject

+ (void)addLifecycleStateTag: (NSString *)value;
+ (void)addApiComplianceTag: (NSString *)value;
+ (void)addArchitectureTag: (NSString *)value;
+ (void)addOskVisibleTag:(BOOL)value;
+ (void)addClientAppIdTag:(NSString *)value;
+ (void)addKeyboardTag:(NSString *)value;
+ (void)addHasAccessibilityTag:(BOOL)value;
+ (void)addActiveKeyboardCountTag:(NSUInteger)value;
+ (void)addInfoBreadCrumb:(NSString *)category message:(NSString *)messageText;
+ (void)addDebugBreadCrumb:(NSString *)category message:(NSString *)messageText;
+ (void)addUserBreadCrumb:(NSString *)category message:(NSString *)messageText;

@end

NS_ASSUME_NONNULL_END
