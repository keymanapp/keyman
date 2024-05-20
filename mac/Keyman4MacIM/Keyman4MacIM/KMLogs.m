/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * LoggerUtil.m
 * CoreTesterApp
 * 
 * Created by Shawn Schantz on 2024-05-16.
 * 
 * Description...
 */

#import "KMLogs.h"
#import <os/log.h>

@implementation KMLogs

char *const keymanSubsystem = "org.sil.keyman";
char *const startupCategory = "startup";
char *const keyCategory = "key";
char *const oskCategory = "osk";

+ (os_log_t)startupLog {
  return os_log_create(keymanSubsystem, startupCategory);
}

+ (os_log_t)keyLog {
  return os_log_create(keymanSubsystem, keyCategory);
}

+ (os_log_t)oskLog {
  return os_log_create(keymanSubsystem, oskCategory);
}

@end
