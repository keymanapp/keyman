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
NSString * const kOskVisibleTagName = @"oskVisible";
NSString * const kClientAppIdTagName = @"clientAppId";
NSString * const kKeyboardTagName = @"keyboard";
NSString * const kHasAccessibilityTagName = @"accessibilityEnabled";

+ (void)addApiComplianceTag: (NSString *)value {
  [self addCustomTag:kApiComplianceTagName withValue:value];
}

+ (void)addOskVisibleTag:(NSString *)value {
  [self addCustomTag:kOskVisibleTagName withValue:value];
}

+ (void)addClientAppIdTag:(NSString *)value {
  [self addCustomTag:kClientAppIdTagName withValue:value];
}

+ (void)addKeyboardTag:(NSString *)value {
  [self addCustomTag:kKeyboardTagName withValue:value];
}

+ (void)addHasAccessibilityTag:(NSString *)value {
  [self addCustomTag:kHasAccessibilityTagName withValue:value];
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
  //crumb.message = [NSString stringWithFormat:@"setDefaultSelectedKeyboard: %@", keyboardName];
  [SentrySDK addBreadcrumb:crumb];
}

+ (void)addUserBreadCrumb:(NSString *)category message:(NSString *)messageText {
  SentryBreadcrumb *crumb = [[SentryBreadcrumb alloc] init];
  crumb.type = @"user";
  crumb.level = kSentryLevelInfo;
  crumb.category = category;
  crumb.message = messageText;
  //crumb.message = [NSString stringWithFormat:@"setDefaultSelectedKeyboard: %@", keyboardName];
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
