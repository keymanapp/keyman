/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMLogs.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2024-05-16.
 *
 */

#import <Foundation/Foundation.h>
#import <os/log.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMLogs : NSObject

+ (void)reportLogStatus;
+ (os_log_t)startupLog;
+ (os_log_t)privacyLog;
+ (os_log_t)complianceLog;
+ (os_log_t)lifecycleLog;
+ (os_log_t)configLog;
+ (os_log_t)dataLog;
+ (os_log_t)uiLog;
+ (os_log_t)eventsLog;
+ (os_log_t)keyboardLog;
+ (os_log_t)keyLog;
+ (os_log_t)oskLog;
+ (os_log_t)testLog;

@end

NS_ASSUME_NONNULL_END
