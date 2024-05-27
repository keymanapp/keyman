/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMELogs.m
 * KeymanEngine4Mac
 *
 * Created by Shawn Schantz on 2024-05-16.
 * 
 * Contains methods to get singleton logger objects and constants for subsystem and category names.
 */

/**
 * The loggers do not appear to be singletons, but the API which creates them manages
 * them as singletons within macOS so that any subsequent call to create a log object
 * using the same subsystem and category will return the existing instance.
 *
 * For any new log categories used anywhere in the Keyman Engine for Mac, the name should be defined here.
 * The Keyman Input Method may define and use the same category name but will use a different subsystem name.
 */

#import "KMELogs.h"
#import <os/log.h>

@implementation KMELogs

char *const keymanEngineSubsystem = "com.keyman.engine";
char *const startupCategory = "startup";
char *const configCategory = "config";
char *const keyCategory = "key";
char *const coreCategory = "core";
char *const oskCategory = "osk";
char *const uiCategory = "ui";
char *const testCategory = "test";

+ (os_log_t)startupLog {
  return os_log_create(keymanEngineSubsystem, startupCategory);
}

+ (os_log_t)configLog {
  return os_log_create(keymanEngineSubsystem, configCategory);
}

+ (os_log_t)keyLog {
  return os_log_create(keymanEngineSubsystem, keyCategory);
}

+ (os_log_t)coreLog {
  return os_log_create(keymanEngineSubsystem, coreCategory);
}

+ (os_log_t)oskLog {
  return os_log_create(keymanEngineSubsystem, oskCategory);
}

+ (os_log_t)uiLog {
  return os_log_create(keymanEngineSubsystem, uiCategory);
}

+ (os_log_t)testLog {
  return os_log_create(keymanEngineSubsystem, testCategory);
}

@end
