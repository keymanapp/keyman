/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Shawn Schantz on 2025-01-13.
 *
 * Description
 */

#import "KMSentryHelper.h"
@import Sentry;

@implementation KMSentryHelper

NSString * const kApiComplianceTagName = @"apiCompliance";
NSString * const kArchitectureTagName = @"architecture";
NSString * const kOskVisibleTagName = @"oskVisible";
NSString * const kClientAppIdTagName = @"clientAppId";
NSString * const kKeyboardTagName = @"keyboard";
NSString * const kHasAccessibilityTagName = @"accessibilityEnabled";
NSString * const kActiveKeyboardCountTagName = @"activeKeyboardCount";
NSString * const kLifecycleStateTagName = @"lifecycleState";

+ (void)addLifecycleStateTag: (NSString *)value {
  [self addCustomTag:kLifecycleStateTagName withValue:value];
}

+ (void)addApiComplianceTag: (NSString *)value {
  [self addCustomTag:kApiComplianceTagName withValue:value];
}

+ (void)addArchitectureTag: (NSString *)value {
  [self addCustomTag:kArchitectureTagName withValue:value];
}

+ (void)addOskVisibleTag:(BOOL)value {
  [self addCustomTag:kOskVisibleTagName withValue:value?@"true":@"false"];
}

+ (void)addClientAppIdTag:(NSString *)value {
  [self addCustomTag:kClientAppIdTagName withValue:value];
}

+ (void)addKeyboardTag:(NSString *)value {
  [self addCustomTag:kKeyboardTagName withValue:value];
}

+ (void)addHasAccessibilityTag:(BOOL)value {
  [self addCustomTag:kHasAccessibilityTagName withValue:value?@"true":@"false"];
}

+ (void)addActiveKeyboardCountTag:(NSUInteger)value {
  [self addCustomTag:kActiveKeyboardCountTagName withValue:[[NSNumber numberWithUnsignedLong:value] stringValue]];
}

/**
 *assign custom keyboard tag in Sentry to initial keyboard
 */
+ (void)addCustomTag:(NSString *)tagName withValue:(NSString *)value {
  [SentrySDK configureScope:^(SentryScope * _Nonnull scope) {
    [scope setTagValue:value forKey:tagName];
  }];
}

+ (void)addBreadCrumb:(NSString *)category message:(NSString *)messageText {
  SentryBreadcrumb *crumb = [[SentryBreadcrumb alloc] init];
  crumb.level = kSentryLevelInfo;
  crumb.category = category;
  crumb.message = messageText;
  [SentrySDK addBreadcrumb:crumb];
}

+ (void)addDebugBreadCrumb:(NSString *)category message:(NSString *)messageText {
  SentryBreadcrumb *crumb = [[SentryBreadcrumb alloc] init];
  crumb.level = kSentryLevelDebug;
  crumb.category = category;
  crumb.message = messageText;
  [SentrySDK addBreadcrumb:crumb];
}

+ (void)addUserBreadCrumb:(NSString *)category message:(NSString *)messageText {
  SentryBreadcrumb *crumb = [[SentryBreadcrumb alloc] init];
  crumb.type = @"user";
  crumb.level = kSentryLevelInfo;
  crumb.category = category;
  crumb.message = messageText;
  [SentrySDK addBreadcrumb:crumb];
}

+ (void)addDetailedBreadCrumb:(NSString *)category message:(NSString *)messageText data:( NSDictionary *) map {
  SentryBreadcrumb *crumb = [[SentryBreadcrumb alloc] init];
  crumb.level = kSentryLevelInfo;
  crumb.category = category;
  crumb.message = messageText;
  crumb.data = map;
  [SentrySDK addBreadcrumb:crumb];
}

@end
