/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMSettingsRepository.h
 * Keyman
 *
 * Created by Shawn Schantz on 2024-07-29.
 *
 * Singleton object for reading and writing Keyman application settings.
 * Serves as an abstraction to StandardUserDefaults which is currently used to persist application settings.
 */

#import "KMSettingsRepository.h"
#import "KMLogs.h"

NSString *const kStoreKeyboardsInLibraryKey = @"KMStoreKeyboardsInLibraryKey";
NSString *const kActiveKeyboardsKey = @"KMActiveKeyboardsKey";

@implementation KMSettingsRepository

+ (KMSettingsRepository *)shared
{
  static KMSettingsRepository *shared = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    shared = [[KMSettingsRepository alloc] init];
  });
  return shared;
}

- (BOOL)settingsExist
{
  return [[NSUserDefaults standardUserDefaults] objectForKey:kActiveKeyboardsKey] != nil;
}

- (BOOL)keyboardsStoredInLibraryFolder
{
  return [[NSUserDefaults standardUserDefaults] boolForKey:kStoreKeyboardsInLibraryKey];
}

/**
 * Determines whether the keyboards data needs to be moved from the old location in the Documents folder to the new location under /username/Library...
 * This is true if
 * 1) the UserDefaults exist (indicating that this is not a new installation of Keyman) and
 * 2) the value  for KMStoreKeyboardsInLibraryKey is not set to true
 */
- (BOOL)keyboardsMigrationNeeded {
  BOOL keyboardSettingsExist = [self settingsExist];
  os_log([KMLogs startupLog], "  keyboard settings exist: %@", keyboardSettingsExist ? @"YES" : @"NO" );
  
  BOOL keyboardsInLibrary = [self keyboardsStoredInLibraryFolder];
  os_log([KMLogs startupLog], "  keyboards stored in Library: %@", keyboardsInLibrary ? @"YES" : @"NO" );
  
  return !(keyboardSettingsExist && keyboardsInLibrary);
}

@end
