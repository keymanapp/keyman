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
  
  SettingsState state = [KMSettingsRepository.shared determineSettingsState];

  switch (state) {
    case KeymanSettingsVersion17:
      os_log_info([KMLogs startupLog], "doMigration executed for Keyman 17 to current");
      [KMDataRepository.shared migrateDataFromKeyman17];
      [KMSettingsRepository.shared migrateSettingsFromKeyman17];
      break;
    case KeymanSettingsVersion18:
      os_log_info([KMLogs startupLog], "doMigration executed for Keyman 18 to current");
      [KMDataRepository.shared migrateDataFromKeyman18];
      [KMSettingsRepository.shared migrateSettingsFromKeyman18];
      break;
    case KeymanSettingsNotFound:
      os_log_info([KMLogs startupLog], "doMigration: no settings found, creating settings and directories");
      [KMSettingsRepository.shared createSharedSettingsIfNecessary];
      [KMDataRepository.shared createSharedDirectoriesIfNecessary];
      break;
    case KeymanSettingsVersionCurrent:
      os_log_info([KMLogs startupLog], "doMigration: no migration needed, settings are current");
      break;
  }

  return 0;
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

