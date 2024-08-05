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
//#import "KMDataRepository.h"
#import "KMLogs.h"

NSString *const kStoreDataInLibraryKey = @"KMStoreDataInLibrary";
NSString *const kActiveKeyboardsKey = @"KMActiveKeyboardsKey";
NSString *const kSelectedKeyboardKey = @"KMSelectedKeyboardKey";
NSString *const kPersistedOptionsKey = @"KMPersistedOptionsKey";

NSString *const kObsoletePathComponent = @"/Documents/";
NSString *const kNewPathComponent = @"/Library/Application Support/keyman.inputmethod.Keyman/";

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
  [self convertPersistedOptionsPathsForMigration];
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

- (void)convertPersistedOptionsPathsForMigration {
  NSDictionary * optionsMap = [self persistedOptions];
  NSMutableDictionary *mutableOptionsMap = nil;
  BOOL optionsChanged = NO;

  if (optionsMap != nil) {
    os_log_info([KMLogs configLog], "optionsMap != nil");
    mutableOptionsMap = [[NSMutableDictionary alloc] initWithCapacity:0];
    for(id key in optionsMap) {
      os_log_info([KMLogs configLog], "persisted options found in UserDefaults with key = %{public}@", key);
    }
    for (NSString *key in optionsMap) {
      NSString *newPathString = [self convertOldKeyboardPath:key];
      NSDictionary *optionsValue = [optionsMap objectForKey:key];

      if ([key isNotEqualTo:newPathString]) {
        optionsChanged = YES;
        
        // insert options into new map with newly converted path as key
        [mutableOptionsMap setObject:optionsValue forKey:newPathString];
        os_log([KMLogs startupLog], "converted option key from '%{public}@' to '%{public}@'", key, newPathString);
      } else {
        // retain options that did not need converting
        [mutableOptionsMap setObject:optionsValue forKey:key];  
      }
    }
    if (optionsChanged) {
      [self savePersistedOptions:mutableOptionsMap];
    }
  }
}

- (NSDictionary *)persistedOptions {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  return [userData dictionaryForKey:kPersistedOptionsKey];
}

- (void)savePersistedOptions:(NSDictionary *) optionsDictionary {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  [userData setObject:optionsDictionary forKey:kPersistedOptionsKey];
}

- (void)removePersistedOptions {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  return [userData removeObjectForKey:kPersistedOptionsKey];
}


@end
