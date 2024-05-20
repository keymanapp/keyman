/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMLogs.h
 * CoreTesterApp
 * 
 * Created by Shawn Schantz on 2024-05-16.
 * 
 * Description...
 */

#import <Foundation/Foundation.h>
#import <os/log.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMLogs : NSObject

+ (os_log_t)startupLog;
+ (os_log_t)keyLog;
+ (os_log_t)oskLog;

@end

NS_ASSUME_NONNULL_END
