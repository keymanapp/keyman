/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Shawn Schantz on 2024-07-29.
 *
 * Singleton object for reading and writing Keyman application settings.
 * Serves as an abstraction to StandardUserDefaults which is currently used to store application settings.
 */

#import "KMSettingsRepository.h"
#import "KMLogs.h"
#import "KMDataRepository.h"

NSString *const kActiveKeyboardsKey = @"KMActiveKeyboardsKey";
NSString *const kSelectedKeyboardKey = @"KMSelectedKeyboardKey";
NSString *const kPersistedOptionsKey = @"KMPersistedOptionsKey";
NSString *const kAlwaysShowOSKKey = @"KMAlwaysShowOSKKey";
NSString *const kUseVerboseLogging = @"KMUseVerboseLogging";

/**
 The following constant "KMSavedStoresKey" is left here for documentation
 though we have abandoned stores written to UserDefaults with this key because
 they used a less-reliable numeric key prior to integration with Keyman Core.
 It is replaced by the renamed "KMPersistedOptionsKey" which directly
 represents what it is saving.
 */
NSString *const kKMDeprecatedPersistedOptionsKey = @"KMSavedStoresKey";

//NSString *const kObsoletePathComponent = @"/Documents/";
NSString *const kObsoletePathComponent = @"/Documents/Keyman-Keyboards";
NSString *const kNewPathComponent = @"/Library/Application Support/keyman.inputmethod.Keyman/";

/**
 * Store the version number of the data model in the UserDefaults with this key.
 * The first version, 1, is defined to indicate that we are storing the data/keyboards in the Library
 * directory instead of in the Documents directory.
 */
NSString *const kDataModelVersion = @"KMDataModelVersion";
NSInteger const kVersionStoreDataInLibraryDirectory = 1;
NSInteger const kCurrentDataModelVersionNumber = kVersionStoreDataInLibraryDirectory;

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

- (void)setDataModelVersionIfNecessary {
  if (![self dataModelWithKeyboardsInLibrary]) {
    [[NSUserDefaults standardUserDefaults] setInteger:kVersionStoreDataInLibraryDirectory forKey:kDataModelVersion];
  }
}

/**
 * If the selectedKeyboard has not been set, then the settings have not been saved in the UserDefaults.
 * If this method is called after applicationDidFinishLaunching, then it will always return true.
 * If called from awakeFromNib, then it will return false when running for the first time.
 */
- (BOOL)settingsExist
{
  return ([[NSUserDefaults standardUserDefaults] objectForKey:kSelectedKeyboardKey] != nil);
}

- (void)writeOptionForSelectedKeyboard:(NSString *)key withValue:(NSString*)value {
  NSDictionary *optionsMap = [self readOptionsForSelectedKeyboard];
  NSDictionary *newOptionsMap = nil;

  // if we can read an existing options map, then add the specified key-value pair
  if (optionsMap != nil) {
    NSMutableDictionary *mutableOptionsMap = [optionsMap mutableCopy];
    [mutableOptionsMap setObject:value forKey:key];
    newOptionsMap = mutableOptionsMap;
  } else {
    // if no options map exists, create a new one add the specified key-value pair
    newOptionsMap = [[NSDictionary alloc] initWithObjectsAndKeys:value, key, nil];
  }

  // write the options map for the selected keyboard to the dictionary of options
  NSString *selectedKeyboard = [self readSelectedKeyboard];
  os_log_info([KMLogs dataLog], "writeOptionForSelectedKeyboard, adding options map: %{public}@, to keyboard %{public}@", newOptionsMap, selectedKeyboard);
  [self writeKeyboardOptionsMap: selectedKeyboard withOptions:newOptionsMap];
}

- (void)writeKeyboardOptionsMap:(NSString *)keyboardName withOptions:(NSDictionary*) optionsMap {
  NSMutableDictionary *newFullOptionsMap = nil;
  os_log_debug([KMLogs dataLog], "writeKeyboardOptionsMap, adding options map: %{public}@, to keyboard %{public}@", optionsMap, keyboardName);

  NSDictionary *fullOptionsMap = [self readFullOptionsMap];
  // if we can read the existing full options map, then add for the specified keyboard
  if (fullOptionsMap != nil) {
    newFullOptionsMap = [fullOptionsMap mutableCopy];
    [newFullOptionsMap setObject:optionsMap forKey:keyboardName];
  } else {
    // otherwise, create the full options map and add for the specified keyboard
    newFullOptionsMap = [[NSMutableDictionary alloc] initWithObjectsAndKeys:optionsMap, keyboardName, nil];
  }

  [self writeFullOptionsMap:newFullOptionsMap];
}

/**
 * For the first numbered version of the data model, the app stores the keyboards under the ~/Library directory
 * For versions before version 1, the keyboards were stored under the ~/Documents directory.
 */
- (BOOL)dataModelWithKeyboardsInLibrary {
  // [NSUserDefaults integerForKey] returns zero if the key does not exist
  NSInteger dataModelVersion = [[NSUserDefaults standardUserDefaults] integerForKey:kDataModelVersion];
  
  return dataModelVersion >= kVersionStoreDataInLibraryDirectory;
}

/**
 * Determines whether the keyboard data needs to be moved from the old location to the new location
 * This is true if
 * 1) the UserDefaults exist (indicating that this is not a new installation of Keyman) and
 * 2) the value for kVersionStoreDataInLibraryDirectory is < 1,
 */
- (BOOL)dataMigrationNeeded {
  BOOL keymanSettingsExist = [self settingsExist];
  os_log([KMLogs dataLog], "keyman settings exist: %{public}@", keymanSettingsExist ? @"YES" : @"NO" );
  
  BOOL keyboardsStoredInLibrary = [self dataModelWithKeyboardsInLibrary];
  os_log([KMLogs dataLog], "settings indicate that keyboards are stored in ~/Library: %{public}@", keyboardsStoredInLibrary ? @"YES" : @"NO" );
  
  BOOL migrationNeeded = keymanSettingsExist && !keyboardsStoredInLibrary;
  os_log([KMLogs dataLog], "dataMigrationNeeded: %{public}@", migrationNeeded ? @"YES" : @"NO" );

  return migrationNeeded;
}

- (NSString *)readSelectedKeyboard {
  return [[NSUserDefaults standardUserDefaults] objectForKey:kSelectedKeyboardKey];
}

- (void)writeSelectedKeyboard:(NSString *)selectedKeyboard {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  [userData setObject:selectedKeyboard forKey:kSelectedKeyboardKey];
}

- (NSMutableArray *)activeKeyboards {
  NSMutableArray * activeKeyboards = [[[NSUserDefaults standardUserDefaults] arrayForKey:kActiveKeyboardsKey] mutableCopy];
  
  if (!activeKeyboards) {
    activeKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
  }
  return activeKeyboards;
}

/*
 * returns dictionary of persisted options for the single selected keyboard
 */
- (NSDictionary *)readOptionsForSelectedKeyboard {
  NSDictionary *optionsMap = [self readFullOptionsMap];
  NSString *selectedKeyboard = [self readSelectedKeyboard];
  NSDictionary *selectedOptionsMap = [optionsMap objectForKey: selectedKeyboard];
  if (selectedOptionsMap == nil) {
    os_log_info([KMLogs dataLog], "no persisted options found in UserDefaults for keyboard %{public}@ ", selectedKeyboard);
  } else {
    for (NSString *key in selectedOptionsMap) {
      NSString *value = [selectedOptionsMap objectForKey:key];
      os_log_info([KMLogs dataLog], "option for keyboard %{public}@ key: %{public}@, value %{public}@", selectedKeyboard, key, value);
    }
  }
  return selectedOptionsMap;
}

/*
 * returns dictionary of all persisted options for all keyboards
 * (options are stored in UserDefaults as a map of maps)
 */
- (NSDictionary *)readFullOptionsMap {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  return [userData dictionaryForKey:kPersistedOptionsKey];
}

- (void)writeFullOptionsMap:(NSDictionary *) fullOptionsMap {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  [userData setObject:fullOptionsMap forKey:kPersistedOptionsKey];
}

- (void)removeAllOptions {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  return [userData removeObjectForKey:kPersistedOptionsKey];
}

- (void)convertSettingsForMigration {
  os_log_debug([KMLogs dataLog], "converting settings in UserDefaults for migration");
  [self convertSelectedKeyboardPathForMigration];
  [self convertActiveKeyboardArrayForMigration];
  [self convertOptionsPathsForMigration];
}

- (void)convertSelectedKeyboardPathForMigration {
  NSString *selectedKeyboardPath = [self readSelectedKeyboard];
  if (selectedKeyboardPath != nil) {
    NSString *newPathString = [self trimObsoleteKeyboardPath:selectedKeyboardPath];
    
    if ([selectedKeyboardPath isNotEqualTo:newPathString]) {
      [self writeSelectedKeyboard:newPathString];
      os_log_debug([KMLogs dataLog], "converted selected keyboard setting from '%{public}@' to '%{public}@'", selectedKeyboardPath, newPathString);
    }
  }
}

/**
 * To convert the keyboard path for the new location, just trim the parent directory from the path
 * No need to repeatedly store the parent directory with the path of each keyboard
 * If the old directory is not found in the string, then return the string unchanged
 */
- (NSString *)trimObsoleteKeyboardPath:(NSString *)oldPath {
  NSString *newPath = oldPath;
  if(oldPath != nil) {
    NSRange range = [oldPath rangeOfString:kObsoletePathComponent];
    if (range.length > 0) {
      newPath = [oldPath substringFromIndex:range.location + range.length];
      os_log_debug([KMLogs dataLog], "trimmed keyboard path from '%{public}@' to '%{public}@'", oldPath, newPath);
    }
  }
  return newPath;
}

- (void)convertActiveKeyboardArrayForMigration {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  NSMutableArray *keyboards = [self activeKeyboards];
  NSMutableArray *convertedActiveKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
  BOOL didConvert = NO;
  
  for (NSString *oldPath in keyboards) {
    NSString *newPath = [self trimObsoleteKeyboardPath:oldPath];
    if ([oldPath isNotEqualTo:newPath]) {
      [convertedActiveKeyboards addObject:newPath];
      os_log_debug([KMLogs dataLog], "converted active keyboard from old path '%{public}@' to '%{public}@'", oldPath, newPath);
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

- (void)convertOptionsPathsForMigration {
  NSDictionary * optionsMap = [self readFullOptionsMap];
  NSMutableDictionary *mutableOptionsMap = nil;
  BOOL optionsChanged = NO;

  if (optionsMap != nil) {
    os_log_info([KMLogs configLog], "optionsMap != nil");
    mutableOptionsMap = [[NSMutableDictionary alloc] initWithCapacity:0];
    for(id key in optionsMap) {
      os_log_info([KMLogs configLog], "persisted options found in UserDefaults with key = %{public}@", key);
    }
    for (NSString *key in optionsMap) {
      NSString *newPathString = [self trimObsoleteKeyboardPath:key];
      NSDictionary *optionsValue = [optionsMap objectForKey:key];

      if ([key isNotEqualTo:newPathString]) {
        optionsChanged = YES;
        
        // insert options into new map with newly converted path as key
        [mutableOptionsMap setObject:optionsValue forKey:newPathString];
        os_log_debug([KMLogs dataLog], "converted option key from '%{public}@' to '%{public}@'", key, newPathString);
      } else {
        // retain options that did not need converting
        [mutableOptionsMap setObject:optionsValue forKey:key];
      }
    }
    if (optionsChanged) {
      [self writeFullOptionsMap:mutableOptionsMap];
    }
  }
}

- (BOOL)readAlwaysShowOsk {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  return [userData boolForKey:kAlwaysShowOSKKey];
}

- (void)writeAlwaysShowOsk:(BOOL)alwaysShowOsk {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  [userData setBool:alwaysShowOsk forKey:kAlwaysShowOSKKey];
}

- (BOOL)readUseVerboseLogging {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  return [userData boolForKey:kUseVerboseLogging];
}

- (void)writeUseVerboseLogging:(BOOL)verboseLogging {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  [userData setBool:verboseLogging forKey:kUseVerboseLogging];
}

@end
