/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-09-09.
 * 
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

extern NSString *const kInputMethodActivatedNotification;
extern NSString *const kInputMethodDeactivatedNotification;
extern NSString *const kInputMethodClientChangeNotification;

@interface KMInputMethodLifecycle : NSObject
+ (KMInputMethodLifecycle *)shared;
+ (NSString*)getClientApplicationId;
- (void)startLifecycle;
- (void)activateClient:(id)client;
- (void)deactivateClient:(id)client;
- (BOOL)shouldEnableEventTap;
- (BOOL)shouldShowOskOnActivate;

@end

NS_ASSUME_NONNULL_END
