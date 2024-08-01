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

NSString *const kStoreDataInLibraryKey = @"KMStoreDataInLibraryKey";
NSString *const kActiveKeyboardsKey = @"KMActiveKeyboardsKey";
NSString *const kSelectedKeyboardKey = @"KMSelectedKeyboardKey";

NSString *const kObsoletePathComponent = @"/Documents/";
NSString *const kNewPathComponent = @"/Application Support/keyman.inputmethod.Keyman/";

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

- (void)createStorageFlagIfNecessary {
  [[NSUserDefaults standardUserDefaults] setBool:YES forKey:kStoreDataInLibraryKey];
}

- (BOOL)settingsExist
{
  return [[NSUserDefaults standardUserDefaults] objectForKey:kActiveKeyboardsKey] != nil;
}

- (BOOL)dataStoredInLibraryDirectory
{
  return [[NSUserDefaults standardUserDefaults] boolForKey:kStoreDataInLibraryKey];
}

/**
 * Determines whether the keyboard data needs to be moved from the old location in ~/Documents to the new location ~/Library...
 * This is true if
 * 1) the UserDefaults exist (indicating that this is not a new installation of Keyman) and
 * 2) the value  for KMStoreKeyboardsInLibraryKey is not set to true
 */
- (BOOL)dataMigrationNeeded {
  BOOL keymanSettingsExist = [self settingsExist];
  os_log([KMLogs startupLog], "  keyman settings exist: %{public}@", keymanSettingsExist ? @"YES" : @"NO" );
  
  BOOL dataInLibrary = [self dataStoredInLibraryDirectory];
  os_log([KMLogs startupLog], "  data stored in Library: %{public}@", dataInLibrary ? @"YES" : @"NO" );
  
  return !(keymanSettingsExist && dataInLibrary);
}

- (void)convertSettingsForMigration {
  [self convertSelectedKeyboardPathForMigration];
  [self convertActiveKeyboardArrayForMigration];
}

- (void)convertSelectedKeyboardPathForMigration {
  NSString *selectedKeyboardPath = [self selectedKeyboard];
  
  if (selectedKeyboardPath != nil) {
    NSString *newPathString = [self convertOldKeyboardPath:selectedKeyboardPath];
    
    if ([selectedKeyboardPath isNotEqualTo:newPathString]) {
      [self saveSelectedKeyboard:newPathString];
      os_log([KMLogs startupLog], "converted selected keyboard setting from '%{public}@' to '%{public}@'", selectedKeyboardPath, newPathString);
    }
  }
}

/**
 * Convert the path of the keyboard designating the Documents folder to its new location
 * in the Application Support folder
 */

- (NSString *)convertOldKeyboardPath:(NSString *)oldPath {
  NSString *newPathString = @"";
  if(oldPath != nil) {
    newPathString = [oldPath stringByReplacingOccurrencesOfString:kObsoletePathComponent withString:kNewPathComponent];
  }
  return newPathString;
}

- (NSString *)selectedKeyboard {
  return [[NSUserDefaults standardUserDefaults] objectForKey:kSelectedKeyboardKey];
}

- (void)saveSelectedKeyboard:(NSString *)selectedKeyboard {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  [userData setObject:selectedKeyboard forKey:kSelectedKeyboardKey];
}

- (void)convertActiveKeyboardArrayForMigration {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  NSMutableArray *activeKeyboards = [self activeKeyboards];
  NSMutableArray *convertedActiveKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
  BOOL didConvert = NO;
  
  for (NSString *oldPath in activeKeyboards) {
    NSString *newPath = [self convertOldKeyboardPath:oldPath];
    if ([oldPath isNotEqualTo:newPath]) {
      [convertedActiveKeyboards addObject:newPath];
      os_log([KMLogs startupLog], "converted active keyboard from old path '%{public}@' to '%{public}@'", oldPath, newPath);
      // if we have adjusted at least one path, set flag
      didConvert = YES;
    } else {
      // if, somehow, the path does not need converting then retain it in new array
      [convertedActiveKeyboards addObject:oldPath];
    }
  }
  
  // only update array in UserDefaults if we actually converted something
  if (didConvert) {
  [[NSUserDefaults standardUserDefaults] setObject:convertedActiveKeyboards forKey:kActiveKeyboardsKey];
  }
}

- (NSMutableArray *)activeKeyboards {
  NSMutableArray * activeKeyboards = [[[NSUserDefaults standardUserDefaults] arrayForKey:kActiveKeyboardsKey] mutableCopy];
  
  if (!activeKeyboards) {
    activeKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
  }
  return activeKeyboards;
}


@end
