/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Serkan Kurt on 2015-01-28
 *
 */

#import <Cocoa/Cocoa.h>
#import <InputMethodKit/InputMethodKit.h>
#import "KMSettingsRepository.h"
#import "KMDataRepository.h"
#import "PrivacyConsent.h"
#import "KMLogs.h"

const NSString *kConnectionName = @"Keyman_Input_Connection";
IMKServer *server;

// command strings passed from Keyman Configuration
NSString *kMigrateCommand = @"migrate";
NSString *kAccessCommand = @"access";
NSString *kCheckCommand = @"check";

// notification messages sent to Keyman Configuration
NSString *kAcessibilityPermissionGrantedMessage = @"granted";
NSString *kAcessibilityPermissionNotGrantedMessage = @"not-granted";


void runAsInputMethod(void) {
  os_log_info([KMLogs startupLog], "main runAsInputMethod");
  NSString *identifier = [[NSBundle mainBundle] bundleIdentifier];
  server = [[IMKServer alloc] initWithName:(NSString *)kConnectionName bundleIdentifier:identifier];
  
  BOOL didLoadNib = [[NSBundle mainBundle] loadNibNamed:@"MainMenu" owner:[NSApplication sharedApplication] topLevelObjects: nil];
  
  os_log_info([KMLogs startupLog], "main Did load MainMenu nib: %@", didLoadNib?@"YES":@"NO");
  
  [[NSApplication sharedApplication] run];
}

/**
 * Migrate data from older locations to the appropriate place for the current version.
 * Executed as requested by the Keyman Configuration app.
 */
int doMigration(void) {
  os_log_info([KMLogs startupLog], "doMigration executed");
  // if necessary, migrate settings and keyboard data for compatibility with Keyman 19
  if ([KMSettingsRepository.shared keyman19SettingsMigrationNeeded]) {
    [KMDataRepository.shared migrateDataForKeyman19];
    [KMSettingsRepository.shared migrateSettingsForKeyman19];
  }
  return 0;
}

/**
 * Make a request to the system to add Accessibility permissions for the Keyman input method.
 * Executed as requested by the Keyman Configuration app.
 */
int requestAccessibility(void) {
  os_log_info([KMLogs startupLog], "doAccessibility executed");
  [PrivacyConsent.shared requestPrivacyAccessForKeyman19:^void (void){
    os_log_info([KMLogs startupLog], "doAccessibility completion handler: requestPrivacyAccessForKeyman19 completed");
  }];
  return 0;
}

/**
 * Check whether Accessibility permissions have been granted by the user for the Keyman input method.
 * Executed as requested by the Keyman Configuration app.
 */
int checkAccessibility(void) {
  BOOL hasAccess = NO;
  NSString *message = kAcessibilityPermissionNotGrantedMessage;
  
  hasAccess = [PrivacyConsent.shared checkPostEventAccess];
  os_log_info([KMLogs startupLog], "checkAccessibility hasAccess: %{public}@", hasAccess?@"YES":@"NO");
  
  if (hasAccess) {
    hasAccess = 0;
    message = kAcessibilityPermissionGrantedMessage;
  } else {
    hasAccess = 1;
  }

  [[NSDistributedNotificationCenter defaultCenter] postNotificationName:@"com.keyman.accessibility.state" object:message userInfo:nil deliverImmediately:YES];

  return hasAccess;
}

int main(int argc, const char * argv[]) {
  
  @autoreleasepool {
    os_log_info([KMLogs startupLog], "main argument count: %d", argc);
    if (argc == 1) {
      runAsInputMethod();
    } else if (argc > 1) {
      const char *installCommand = argv[1];
      NSString *commandString = [NSString stringWithUTF8String:installCommand];
      os_log_info([KMLogs startupLog], "main command: %{public}@", commandString);

      if ([commandString isEqualToString:kMigrateCommand]) {
        return doMigration();
      } else if ([commandString isEqualToString:kAccessCommand]) {
        return requestAccessibility();
      } else if ([commandString isEqualToString:kCheckCommand]) {
        return checkAccessibility();
      } else {
        os_log_info([KMLogs startupLog], "*** unknown command: %{public}@", commandString);
      }
    }
  }
  return 0;
}

