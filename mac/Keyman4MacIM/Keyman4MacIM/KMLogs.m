/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMLogs.m
 * Keyman
 *
 * Created by Shawn Schantz on 2024-05-16.
 * 
 * Contains methods to get singleton logger objects and constants for subsystem and category names.
 */

/**
 *
 * The loggers do not appear to be singletons, but the API which creates them manages
 * them as singletons within macOS so that any subsequent call to create a log object
 * using the same subsystem and category will return the existing instance.
 *
 * For any new log categories used anywhere in the Keyman Input method, the name should be defined here.
 * Keyman Engine may define and use the same category name but will use a different subsystem name. 
 */

#import "KMLogs.h"

@implementation KMLogs

char *const keymanSubsystem = "org.sil.keyman";
char *const startupCategory = "startup";
char *const privacyCategory = "privacy";
char *const complianceCategory = "compliance";
char *const lifecycleCategory = "lifecycle";
char *const configCategory = "config";
char *const dataCategory = "data";
char *const uiCategory = "ui";
char *const eventsCategory = "events";
char *const keyboardCategory = "keyboard";
char *const keyCategory = "key";
char *const oskCategory = "osk";
char *const testCategory = "test";

+ (void)reportLogStatus {
  bool debugLogEnabled = os_log_type_enabled([KMLogs startupLog], OS_LOG_TYPE_DEBUG);
  os_log([KMLogs startupLog], "startupLog has debug messages enabled: %@", debugLogEnabled?@"YES":@"NO");
  bool infoLogEnabled = os_log_type_enabled([KMLogs startupLog], OS_LOG_TYPE_INFO);
  os_log([KMLogs startupLog], "startupLog has info messages enabled: %@", infoLogEnabled?@"YES":@"NO");
}

+ (os_log_t)startupLog {
  return os_log_create(keymanSubsystem, startupCategory);
}

+ (os_log_t)privacyLog {
  return os_log_create(keymanSubsystem, privacyCategory);
}

+ (os_log_t)complianceLog {
  return os_log_create(keymanSubsystem, complianceCategory);
}

+ (os_log_t)lifecycleLog {
  return os_log_create(keymanSubsystem, lifecycleCategory);
}

+ (os_log_t)configLog {
  return os_log_create(keymanSubsystem, configCategory);
}

+ (os_log_t)dataLog{
  return os_log_create(keymanSubsystem, dataCategory);
}

+ (os_log_t)uiLog {
  return os_log_create(keymanSubsystem, uiCategory);
}

+ (os_log_t)eventsLog {
  return os_log_create(keymanSubsystem, eventsCategory);
}

+ (os_log_t)keyboardLog {
  return os_log_create(keymanSubsystem, keyboardCategory);
}

+ (os_log_t)keyLog {
  return os_log_create(keymanSubsystem, keyCategory);
}

+ (os_log_t)oskLog {
  return os_log_create(keymanSubsystem, oskCategory);
}

+ (os_log_t)testLog {
  return os_log_create(keymanSubsystem, testCategory);
}

@end
