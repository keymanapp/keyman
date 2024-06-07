/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMELogs.h
 * KeymanEngine4Mac
 * 
 * Created by Shawn Schantz on 2024-05-16.
 *
 * Contains methods to get singleton logger objects and constants for subsystem and category names.
 */

#import <Foundation/Foundation.h>
#import <os/log.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMELogs : NSObject

+ (os_log_t)startupLog;
+ (os_log_t)configLog;
+ (os_log_t)keyLog;
+ (os_log_t)coreLog;
+ (os_log_t)oskLog;
+ (os_log_t)uiLog;
+ (os_log_t)testLog;

@end

NS_ASSUME_NONNULL_END
