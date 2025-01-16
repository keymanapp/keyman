/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Shawn Schantz on 2025-01-13.
 *
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMSentryHelper : NSObject

+ (void)addApiComplianceTag: (NSString *)value;
+ (void)addOskVisibleTag:(NSString *)value;
+ (void)addClientAppIdTag:(NSString *)value;
+ (void)addKeyboardTag:(NSString *)value;
+ (void)addHasAccessibilityTag:(NSString *)value;
+ (void)addBreadCrumb:(NSString *)category message:(NSString *)messageText;
+ (void)addUserBreadCrumb:(NSString *)category message:(NSString *)messageText;
+ (void)addDetailedBreadCrumb:(NSString *)category message:(NSString *)messageText data:( NSDictionary *) map;

@end

NS_ASSUME_NONNULL_END
